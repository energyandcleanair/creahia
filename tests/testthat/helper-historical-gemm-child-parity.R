historical_gemm_child_registry <- function() {
  .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_GEMM, cause == CAUSE_LRICHILD) %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    .new_historical_crf_set()
}


historical_gemm_child_paf_keys <- function() {
  historical_gemm_child_registry() %>%
    dplyr::select(pollutant, cause, outcome)
}


historical_gemm_child_impact_keys <- function() {
  historical_gemm_child_paf_keys() %>%
    dplyr::mutate(
      outcome = dplyr::if_else(
        outcome == MEASURE_DEATHS,
        "Deaths.child",
        outcome
      )
    )
}


filter_historical_gemm_child_paf <- function(data) {
  data %>%
    dplyr::semi_join(
      historical_gemm_child_paf_keys(),
      by = c("pollutant", "cause", "outcome")
    )
}


filter_historical_gemm_child_impacts <- function(data) {
  data %>%
    dplyr::semi_join(
      historical_gemm_child_impact_keys(),
      by = c("pollutant", "cause", "outcome")
    )
}


arrange_historical_gemm_child_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_gemm_child_impacts <- function(impacts) {
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


run_historical_gemm_child_parity_fixture <- function() {
  configuration_id <- "actual_no_argument_default"
  legacy_crfs <- get_crfs("default")
  legacy_rr_sources <- c(RR_GEMM, RR_ORIGINAL)
  registry_crfs <- historical_gemm_child_registry()
  legacy_gemm_sources <- tibble::tibble(
    cause = CAUSE_LRICHILD,
    source = RR_GEMM
  )
  empty_rr_sources <- legacy_hia_empty_rr_sources()
  epi <- legacy_hia_epi_fixture(legacy_crfs, legacy_rr_sources)
  epi_long <- legacy_hia_epi_long_fixture(legacy_rr_sources)
  conc_map <- legacy_default_fingerprint_conc_map()
  regions <- legacy_hia_fingerprint_regions()
  species <- c("pm25", "no2", "o3_8h", "so2")

  testthat::with_mocked_bindings(
    get_epi_count_long = function(...) epi_long,
    get_epi_location_id = function(...) 1,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        rr_sources = parse_rr_sources(legacy_gemm_sources),
        crfs = legacy_crfs,
        diagnostic_folder = NULL,
        crf_compute = "legacy"
      )
      registry_paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        rr_sources = empty_rr_sources,
        crfs = registry_crfs,
        diagnostic_folder = NULL
      )
      legacy_impacts <- compute_hia(
        conc_map = conc_map,
        species = species,
        regions = regions,
        rr_sources = legacy_gemm_sources,
        epi = epi,
        crfs = legacy_crfs,
        diagnostic_folder = NULL,
        crf_compute = "legacy"
      )
      registry_impacts <- compute_hia(
        conc_map = conc_map,
        species = species,
        regions = regions,
        rr_sources = empty_rr_sources,
        epi = epi,
        crfs = registry_crfs,
        diagnostic_folder = NULL
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
