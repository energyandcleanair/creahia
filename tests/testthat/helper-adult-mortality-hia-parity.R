adult_mortality_hia_config <- function(model) {
  switch(
    model,
    gemm = list(
      rr_source = RR_GEMM,
      presets = c(
        "gemm_adult_deaths_aggregate",
        "gemm_adult_deaths_components"
      ),
      expected_ages = function(cause) {
        deduplicate_adult_ages(c(AGE_ADULTS, AGE_ADULTS_SPLIT))
      }
    ),
    fusion = list(
      rr_source = RR_FUSION,
      presets = c(
        "fusion_adult_deaths_aggregate",
        "fusion_adult_deaths_components"
      ),
      expected_ages = function(cause) {
        deduplicate_adult_ages(fusion_expected_ages(cause))
      }
    ),
    stop("Unsupported adult mortality HIA model: ", model, call. = FALSE)
  )
}

run_adult_mortality_hia_parity_fixture <- function(model, preset) {
  config <- adult_mortality_hia_config(model)
  if (!preset %in% config$presets) {
    stop(
      "Preset ", preset, " is not valid for model ", model, ".",
      call. = FALSE
    )
  }

  registry_crfs <- crfs_set(presets = preset)
  causes <- registry_crfs$cause
  legacy_crfs <- load_crf_registry() %>%
    dplyr::slice(0)
  legacy_rr_sources <- tibble::tibble(
    cause = causes,
    source = config$rr_source
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
    dplyr::filter(cause %in% causes)
  epi_long <- dplyr::bind_rows(lapply(
    seq_len(nrow(legacy_cause_measures)),
    function(i) {
      cause <- legacy_cause_measures$cause[[i]]
      measure <- legacy_cause_measures$measure[[i]]
      ages <- config$expected_ages(cause)

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
          epi_version = "gbd2019",
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
          epi_version = "gbd2019",
          epi = epi,
          crfs = registry_crfs,
          diagnostic_folder = NULL
        )
      )
    }
  )

  results$model <- model
  results$preset <- preset
  results$causes <- causes
  results
}

arrange_adult_mortality_death_results <- function(result) {
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

adult_mortality_death_results <- function(result, causes) {
  result %>%
    dplyr::filter(
      cause %in% causes,
      outcome == MEASURE_DEATHS
    ) %>%
    arrange_adult_mortality_death_results()
}

adult_mortality_hia_fingerprint <- function(result, preset) {
  result %>%
    dplyr::filter(estimate == "central") %>%
    arrange_adult_mortality_death_results() %>%
    dplyr::mutate(preset = preset, .before = 1) %>%
    dplyr::select(
      preset,
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

read_adult_mortality_hia_fingerprint <- function(model) {
  readr::read_csv(
    testthat::test_path(
      "fixtures",
      paste0(model, "_adult_deaths_hia_fingerprint.csv")
    ),
    show_col_types = FALSE
  ) %>%
    tibble::as_tibble()
}
