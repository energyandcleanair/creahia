arrange_historical_crf_profile_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_crf_profile_impacts <- function(impacts) {
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


run_historical_crf_profile_parity_fixture <- function(
  profile_version,
  selection_id
) {
  memberships <- .load_historical_crf_memberships() %>%
    dplyr::filter(.data$selection_id == !!selection_id)
  registry_crfs <- .resolve_historical_crf_components(
    component_ids = memberships$component_id
  )
  legacy_crfs <- get_crfs(profile_version)
  empty_rr_sources <- legacy_hia_empty_rr_sources()
  epi <- legacy_hia_epi_fixture(legacy_crfs, empty_rr_sources)
  conc_map <- legacy_default_fingerprint_conc_map()
  regions <- legacy_hia_fingerprint_regions()
  species <- c("pm25", "no2", "o3_8h", "so2")

  testthat::with_mocked_bindings(
    get_epi_location_id = function(...) 1,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        rr_sources = empty_rr_sources,
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
        rr_sources = empty_rr_sources,
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
    registry_crfs = registry_crfs,
    legacy_paf = legacy_paf,
    registry_paf = registry_paf,
    legacy_impacts = legacy_impacts,
    registry_impacts = registry_impacts
  )
}
