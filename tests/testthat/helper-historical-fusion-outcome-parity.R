historical_fusion_outcome_registry <- function() {
  promoted_rr_registry(RR_FUSION, child = FALSE)
}


historical_fusion_outcome_keys <- function() {
  historical_fusion_outcome_registry() %>%
    dplyr::select(pollutant, cause, outcome)
}


filter_historical_fusion_outcomes <- function(data) {
  data %>%
    dplyr::semi_join(
      historical_fusion_outcome_keys(),
      by = c("pollutant", "cause", "outcome")
    )
}


arrange_historical_fusion_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_fusion_impacts <- function(impacts) {
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


run_historical_fusion_outcome_parity_fixture <- function() {
  configuration_id <- "legacy_default_fusion_gbd2021_2021"
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  registry_crfs <- historical_fusion_outcome_registry()
  legacy_fusion_sources <- registry_crfs %>%
    dplyr::distinct(cause) %>%
    dplyr::mutate(source = RR_FUSION)
  empty_rr_sources <- legacy_hia_empty_rr_sources()
  epi_version <- legacy_hia_configuration_value(configuration, "epi_version")
  pop_year <- legacy_hia_configuration_value(configuration, "pop_year")
  population_projection <- legacy_hia_population_projection_fixture()
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
    get_pop_year = function(...) 2020L,
    get_pop_proj = function() population_projection,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        epi_version = epi_version,
        rr_sources = parse_rr_sources(legacy_fusion_sources),
        crfs = legacy_crfs,
        diagnostic_folder = NULL,
        crf_compute = "legacy"
      )
      registry_paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        epi_version = epi_version,
        rr_sources = empty_rr_sources,
        crfs = registry_crfs,
        diagnostic_folder = NULL
      )
      legacy_impacts <- compute_hia(
        conc_map = conc_map,
        species = species,
        regions = regions,
        rr_sources = legacy_fusion_sources,
        epi_version = epi_version,
        epi = epi,
        crfs = legacy_crfs,
        pop_year = pop_year,
        diagnostic_folder = NULL,
        crf_compute = "legacy"
      )
      registry_impacts <- compute_hia(
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
