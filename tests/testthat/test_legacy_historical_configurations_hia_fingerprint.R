test_that("all nine historical HIA configurations have fixed legacy fingerprints", {
  configurations <- load_historical_fingerprint_configurations()
  population_projection <- legacy_hia_population_projection_fixture()
  base_population_year <- 2020L
  base_population <- 4000
  real_get_rr <- get_rr
  rr_cache <- new.env(parent = emptyenv())
  cached_get_rr <- function(version) {
    if (!exists(version, envir = rr_cache, inherits = FALSE)) {
      assign(version, real_get_rr(version), envir = rr_cache)
    }
    get(version, envir = rr_cache, inherits = FALSE)
  }

  fingerprints <- purrr::map_dfr(seq_len(nrow(configurations)), function(i) {
    configuration <- configurations[i, , drop = FALSE]
    configuration_id <- legacy_hia_configuration_value(
      configuration,
      "mapping_id"
    )
    epi_version <- legacy_hia_configuration_value(configuration, "epi_version")
    pop_year <- legacy_hia_configuration_value(configuration, "pop_year")
    crfs <- get_crfs(
      legacy_hia_configuration_value(configuration, "legacy_crfs_version")
    )
    rr_sources <- legacy_hia_configuration_rr_sources(configuration)

    observed_epi_versions <- character()
    observed_epi_long_versions <- character()
    observed_pop_years <- integer()
    legacy_warnings <- character()

    withCallingHandlers(
      with_mocked_bindings(
        get_epi = function(version) {
          observed_epi_versions <<- c(observed_epi_versions, version)
          legacy_hia_epi_fixture(
            crfs = crfs,
            rr_sources = rr_sources,
            epi_version = version
          )
        },
        get_epi_count_long = function(version) {
          observed_epi_long_versions <<- c(
            observed_epi_long_versions,
            version
          )
          legacy_hia_epi_long_fixture(
            rr_sources = rr_sources,
            epi_version = version
          )
        },
        get_epi_location_id = function(...) 1,
        get_rr = cached_get_rr,
        get_pop_year = function(year_desired) {
          observed_pop_years <<- c(observed_pop_years, year_desired)
          base_population_year
        },
        get_pop_proj = function() population_projection,
        diagnose_paf = function(...) invisible(NULL),
        {
          result <- do.call(
            compute_hia,
            legacy_hia_configuration_compute_args(configuration)
          )
        }
      ),
      warning = function(condition) {
        legacy_warnings <<- c(legacy_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )

    expected_population <- base_population *
      population_projection$pop[population_projection$year == pop_year] /
      population_projection$pop[
        population_projection$year == base_population_year
      ]

    expect_equal(observed_epi_versions, epi_version, info = configuration_id)
    expect_true(
      length(observed_epi_long_versions) > 0,
      info = configuration_id
    )
    expect_setequal(
      unique(observed_epi_long_versions),
      epi_version
    )
    expect_equal(observed_pop_years, pop_year, info = configuration_id)
    expect_equal(unique(result$pop), expected_population, info = configuration_id)
    expect_setequal(
      unique(result$estimate),
      c("low", "central", "high")
    )
    expect_true(all(!is.na(result$number)), info = configuration_id)
    expect_true(any(result$number != 0), info = configuration_id)
    expect_false(
      any(grepl("Failed for region", legacy_warnings, fixed = TRUE)),
      info = configuration_id
    )

    legacy_hia_fingerprint(result, configuration_id)
  }) %>%
    dplyr::arrange(
      .data$configuration_id,
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome
    )

  expect_legacy_fingerprint_equal(
    fingerprints,
    read_legacy_historical_configurations_hia_fingerprint()
  )
  expect_setequal(
    unique(fingerprints$configuration_id),
    configurations$mapping_id
  )
})
