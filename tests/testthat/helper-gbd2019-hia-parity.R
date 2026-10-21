run_gbd2019_hia_parity_fixture <- function() {
  version <- RR_GBD2019
  death_causes <- gbd_death_causes(version)
  registry_crfs <- crfs_set(presets = "gbd2019_deaths")
  legacy_crfs <- load_crf_registry() %>%
    dplyr::slice(0)
  legacy_rr_sources <- tibble::tibble(
    cause = death_causes,
    source = version
  )
  empty_rr_sources <- tibble::tibble(
    cause = character(),
    source = character()
  )

  conc_map <- list(
    scenario1 = list(
      BGD = tibble::tibble(
        conc_baseline_pm25 = c(20, 30, 40),
        conc_scenario_pm25 = c(15, 25, 35),
        pop = c(100, 200, 300)
      )
    )
  )
  regions <- tibble::tibble(
    region_id = "BGD",
    region_name = "Bangladesh",
    country_id = "BGD"
  )

  legacy_cause_measures <- get_cause_measure() %>%
    dplyr::filter(cause %in% death_causes)
  epi_long <- dplyr::bind_rows(lapply(
    seq_len(nrow(legacy_cause_measures)),
    function(i) {
      cause <- legacy_cause_measures$cause[[i]]
      measure <- legacy_cause_measures$measure[[i]]
      ages <- gbd_expected_ages(version, cause)

      tibble::tibble(
        location_id = 1,
        cause = cause,
        measure_name = measure,
        age = ages,
        estimate = "central",
        val = i * 100 + seq_along(ages)
      )
    }
  ))

  metric_keys <- build_metric_key(
    legacy_cause_measures$cause,
    legacy_cause_measures$measure
  ) %>%
    unique()
  epi <- tibble::tibble(
    location_id = 1,
    estimate = c("low", "central", "high"),
    pop = 100000,
    country = "BGD",
    iso3 = "BGD"
  )
  for (i in seq_along(metric_keys)) {
    epi[[metric_keys[[i]]]] <- c(80, 100, 120) + i
  }

  results <- testthat::with_mocked_bindings(
    get_epi_location_id = function(...) 1,
    get_epi_count_long = function(...) epi_long,
    diagnose_paf = function(...) invisible(NULL),
    {
      list(
        legacy = compute_hia(
          conc_map = conc_map,
          species = "pm25",
          regions = regions,
          rr_sources = legacy_rr_sources,
          epi_version = version,
          epi = epi,
          crfs = legacy_crfs,
          diagnostic_folder = NULL,
          crf_compute = "legacy"
        ),
        registry = compute_hia(
          conc_map = conc_map,
          species = "pm25",
          regions = regions,
          rr_sources = empty_rr_sources,
          epi_version = version,
          epi = epi,
          crfs = registry_crfs,
          diagnostic_folder = NULL
        )
      )
    }
  )

  results$death_causes <- death_causes
  results
}

arrange_gbd2019_death_results <- function(result) {
  comparison_columns <- c(
    "scenario",
    "region_id",
    "pollutant",
    "cause",
    "outcome",
    "estimate"
  )

  result %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(comparison_columns)))
}

gbd2019_deaths_hia_fingerprint <- function(result) {
  result %>%
    dplyr::filter(estimate == "central") %>%
    arrange_gbd2019_death_results() %>%
    dplyr::select(
      scenario,
      region_id,
      pollutant,
      cause,
      outcome,
      estimate,
      age_group,
      double_counted,
      number
    )
}
