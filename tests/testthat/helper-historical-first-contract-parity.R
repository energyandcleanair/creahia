historical_first_contract_id <- function() {
  "legacy_default_gemm_gbd2019_2020"
}


historical_contract_specification <- function(
  configuration_id,
  calc_causes = NULL
) {
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  epi_version <- legacy_hia_configuration_value(configuration, "epi_version")
  epi <- legacy_hia_epi_fixture(
    crfs = legacy_crfs,
    rr_sources = legacy_rr_sources,
    epi_version = epi_version
  )
  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = legacy_rr_sources,
    calc_causes = calc_causes,
    gbd_causes = NULL
  )
  boundary <- .resolve_hia_compatibility_boundary(
    crfs = legacy_crfs,
    rr_sources = parse_rr_sources(rr_selection$rr_sources),
    epi = epi,
    context = list(
      rr_sources_supplied = FALSE,
      epi_supplied = FALSE,
      crfs_supplied = FALSE,
      epi_version = epi_version,
      crfs_version = legacy_hia_configuration_value(
        configuration,
        "legacy_crfs_version"
      ),
      crf_compute = "legacy",
      rr_selection_mode = rr_selection$mode,
      calc_causes = rr_selection$calc_causes,
      gbd_causes = rr_selection$gbd_causes
    ),
    population_year_intent = .capture_population_year_intent(
      pop_year = legacy_hia_configuration_value(configuration, "pop_year"),
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = legacy_hia_configuration_value(
        configuration,
        "pop_year"
      )
    )
  )

  boundary$specification
}


historical_first_contract_specification <- function() {
  historical_contract_specification(
    configuration_id = historical_first_contract_id(),
    calc_causes = "GEMM and GBD"
  )
}


capture_historical_first_contract_warnings <- function(code) {
  observed_warnings <- character()
  value <- withCallingHandlers(
    force(code),
    warning = function(condition) {
      observed_warnings <<- c(
        observed_warnings,
        conditionMessage(condition)
      )
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = observed_warnings)
}


arrange_historical_first_contract_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_first_contract_impacts <- function(impacts) {
  impacts %>%
    dplyr::arrange(
      scenario,
      region_id,
      pollutant,
      cause,
      outcome,
      estimate
    )
}


run_historical_contract_parity_fixture <- function(
  configuration_id,
  calc_causes = NULL,
  activate_route = FALSE
) {
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  specification <- historical_contract_specification(
    configuration_id = configuration_id,
    calc_causes = calc_causes
  )
  registry_crfs <- .resolve_hia_diagnostic_registry_set(specification)
  empty_rr_sources <- legacy_hia_empty_rr_sources()
  epi_version <- legacy_hia_configuration_value(configuration, "epi_version")
  epi <- legacy_hia_epi_fixture(
    crfs = legacy_crfs,
    rr_sources = legacy_rr_sources,
    epi_version = epi_version
  )
  epi_long <- legacy_hia_epi_long_fixture(
    rr_sources = legacy_rr_sources,
    epi_version = epi_version
  )
  conc_map <- legacy_default_fingerprint_conc_map()
  regions <- legacy_hia_fingerprint_regions()
  species <- c("pm25", "no2", "o3_8h", "so2")
  pop_year <- legacy_hia_configuration_value(configuration, "pop_year")
  population_projection <- legacy_hia_population_projection_fixture()
  activated_impacts <- NULL

  testthat::with_mocked_bindings(
    get_crfs = function(...) legacy_crfs,
    get_epi = function(...) epi,
    get_epi_count_long = function(...) epi_long,
    get_epi_location_id = function(...) 1,
    get_pop_year = function(...) 2020L,
    get_pop_proj = function() population_projection,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_paf <- capture_historical_first_contract_warnings(
        compute_hia_paf(
          conc_map = conc_map,
          species = species,
          regions = regions,
          epi_version = epi_version,
          rr_sources = legacy_rr_sources,
          crfs = legacy_crfs,
          diagnostic_folder = NULL,
          crf_compute = "legacy"
        )
      )
      registry_paf <- capture_historical_first_contract_warnings(
        compute_hia_paf(
          conc_map = conc_map,
          species = species,
          regions = regions,
          epi_version = epi_version,
          rr_sources = empty_rr_sources,
          crfs = registry_crfs,
          diagnostic_folder = NULL
        )
      )
      legacy_impacts <- capture_historical_first_contract_warnings(
        compute_hia(
          conc_map = conc_map,
          species = species,
          regions = regions,
          rr_sources = legacy_rr_sources,
          epi_version = epi_version,
          epi = epi,
          crfs = legacy_crfs,
          pop_year = pop_year,
          diagnostic_folder = NULL,
          crf_compute = "legacy"
        )
      )
      registry_impacts <- capture_historical_first_contract_warnings(
        compute_hia(
          conc_map = conc_map,
          species = species,
          regions = regions,
          rr_sources = empty_rr_sources,
          epi_version = epi_version,
          epi = epi,
          crfs = registry_crfs,
          pop_year = pop_year,
          diagnostic_folder = NULL
        )
      )
      if (activate_route) {
        activated_args <- list(
          conc_map = conc_map,
          species = species,
          regions = regions,
          epi_version = epi_version,
          crfs_version = legacy_hia_configuration_value(
            configuration,
            "legacy_crfs_version"
          ),
          pop_year = pop_year,
          diagnostic_folder = NULL
        )
        if (is.null(calc_causes)) {
          activated_args$rr_sources <- legacy_rr_sources
        } else {
          activated_args$calc_causes <- calc_causes
          activated_args$gbd_causes <- "default"
        }
        activated_impacts <- withr::with_options(
          list(
            creahia.hia_compatibility_routing = TRUE,
            creahia.hia_dual_run_observer = NULL,
            creahia.hia_readiness_shadow_observer = NULL
          ),
          capture_historical_first_contract_warnings(
            do.call(compute_hia, activated_args)
          )
        )
      }
    }
  )

  list(
    configuration_id = configuration_id,
    specification = specification,
    registry_crfs = registry_crfs,
    legacy_paf = legacy_paf,
    registry_paf = registry_paf,
    legacy_impacts = legacy_impacts,
    registry_impacts = registry_impacts,
    activated_impacts = activated_impacts
  )
}


run_historical_first_contract_parity_fixture <- function() {
  run_historical_contract_parity_fixture(
    configuration_id = historical_first_contract_id(),
    calc_causes = "GEMM and GBD"
  )
}


expect_historical_contract_parity <- function(
  results,
  expected_rr_source,
  expected_paf_rows,
  expected_warnings = c(
    "PAF estimates are not properly ordered or of the same sign for Diabetes Deaths",
    "PAF estimates are not properly ordered or of the same sign for Diabetes YLDs"
  )
) {
  registry_set <- results$registry_crfs
  rr_components <- results$specification$components$rr

  testthat::expect_true(results$specification$readiness$ready)
  testthat::expect_s3_class(registry_set, "creahia_crf_set")
  testthat::expect_equal(nrow(registry_set), expected_paf_rows)
  testthat::expect_equal(nrow(rr_components), expected_paf_rows - 15L)
  testthat::expect_setequal(unique(rr_components$source), expected_rr_source)

  observed_warning_sets <- list(
    legacy_paf = results$legacy_paf$warnings,
    registry_paf = results$registry_paf$warnings,
    legacy_impacts = results$legacy_impacts$warnings,
    registry_impacts = results$registry_impacts$warnings,
    activated_impacts = results$activated_impacts$warnings
  )
  for (warning_name in names(observed_warning_sets)) {
    testthat::expect_equal(
      sort(observed_warning_sets[[warning_name]]),
      sort(expected_warnings),
      info = warning_name
    )
  }

  legacy_paf <- results$legacy_paf$value %>%
    arrange_historical_first_contract_paf()
  registry_paf <- results$registry_paf$value %>%
    arrange_historical_first_contract_paf()
  testthat::expect_equal(nrow(legacy_paf), expected_paf_rows)
  testthat::expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)

  legacy_impacts <- results$legacy_impacts$value %>%
    arrange_historical_first_contract_impacts()
  registry_impacts <- results$registry_impacts$value %>%
    arrange_historical_first_contract_impacts()
  activated_impacts <- results$activated_impacts$value %>%
    arrange_historical_first_contract_impacts()
  testthat::expect_equal(nrow(legacy_impacts), expected_paf_rows * 3L)
  testthat::expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  testthat::expect_equal(activated_impacts, legacy_impacts, tolerance = 1e-12)

  actual_fingerprint <- legacy_hia_fingerprint(
    activated_impacts,
    results$configuration_id
  )
  expected_fingerprint <-
    read_legacy_historical_configurations_hia_fingerprint() %>%
    dplyr::filter(configuration_id == results$configuration_id)

  testthat::expect_equal(nrow(actual_fingerprint), expected_paf_rows)
  expect_legacy_fingerprint_equal(actual_fingerprint, expected_fingerprint)
}
