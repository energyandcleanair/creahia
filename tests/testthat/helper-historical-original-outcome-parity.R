historical_original_outcome_registry <- function() {
  .load_historical_rr_registry() %>%
    dplyr::filter(
      source == RR_ORIGINAL,
      cause %in% c("Diabetes", CAUSE_LRICHILD)
    ) %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    new_crfs_set()
}


historical_original_paf_keys <- function() {
  historical_original_outcome_registry() %>%
    dplyr::select(pollutant, cause, outcome)
}


historical_original_impact_keys <- function() {
  historical_original_paf_keys() %>%
    dplyr::mutate(
      outcome = dplyr::if_else(
        cause == CAUSE_LRICHILD & outcome == MEASURE_DEATHS,
        "Deaths.child",
        outcome
      )
    )
}


filter_historical_original_paf <- function(data) {
  data %>%
    dplyr::semi_join(
      historical_original_paf_keys(),
      by = c("pollutant", "cause", "outcome")
    )
}


filter_historical_original_impacts <- function(data) {
  data %>%
    dplyr::semi_join(
      historical_original_impact_keys(),
      by = c("pollutant", "cause", "outcome")
    )
}


arrange_historical_original_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_original_impacts <- function(impacts) {
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


capture_historical_original_warnings <- function(code) {
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


run_historical_original_outcome_parity_fixture <- function() {
  configuration_id <- "legacy_default_gemm_gbd2019_2020"
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  registry_crfs <- historical_original_outcome_registry()
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

  testthat::with_mocked_bindings(
    get_epi_count_long = function(...) epi_long,
    get_epi_location_id = function(...) 1,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_paf <- capture_historical_original_warnings(
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
      registry_paf <- capture_historical_original_warnings(
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
      legacy_impacts <- capture_historical_original_warnings(
        compute_hia(
          conc_map = conc_map,
          species = species,
          regions = regions,
          rr_sources = legacy_rr_sources,
          epi_version = epi_version,
          epi = epi,
          crfs = legacy_crfs,
          diagnostic_folder = NULL,
          crf_compute = "legacy"
        )
      )
      registry_impacts <- capture_historical_original_warnings(
        compute_hia(
          conc_map = conc_map,
          species = species,
          regions = regions,
          rr_sources = empty_rr_sources,
          epi_version = epi_version,
          epi = epi,
          crfs = registry_crfs,
          diagnostic_folder = NULL
        )
      )
    }
  )

  list(
    configuration_id = configuration_id,
    legacy_paf = legacy_paf,
    registry_paf = registry_paf,
    legacy_impacts = legacy_impacts,
    registry_impacts = registry_impacts
  )
}
