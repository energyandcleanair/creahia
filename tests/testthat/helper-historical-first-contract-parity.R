historical_first_contract_id <- function() {
  "legacy_default_gemm_gbd2019_2020"
}


historical_first_contract_rr_manifest <- function() {
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == historical_first_contract_id())
  selected_components <- legacy_hia_configuration_rr_sources(configuration) %>%
    parse_rr_sources() %>%
    .normalise_rr_components()

  .load_historical_rr_manifest() %>%
    dplyr::semi_join(selected_components, by = "component_id")
}


resolve_historical_first_contract_public_rr <- function(manifest) {
  public_registry <- load_crf_registry()
  public_manifest <- manifest %>%
    dplyr::filter(coverage_status == "public_registry_candidate")

  purrr::map_dfr(seq_len(nrow(public_manifest)), function(i) {
    component <- public_manifest[i, , drop = FALSE]
    candidate_ids <- strsplit(
      component$public_registry_crf_ids,
      ";",
      fixed = TRUE
    )[[1]]
    candidates <- public_registry %>%
      dplyr::filter(crf_id %in% candidate_ids)
    age_matches <- vapply(seq_len(nrow(candidates)), function(j) {
      candidate_data <- load_crf_tabular(candidates[j, , drop = FALSE])
      identical(
        .canonical_rr_age_contract(candidate_data$age),
        component$age_contract
      )
    }, logical(1))
    matching_candidate <- candidates[age_matches, , drop = FALSE]

    if (nrow(matching_candidate) != 1) {
      stop(
        "Expected one public RR candidate for ",
        component$component_id,
        ", found ",
        nrow(matching_candidate),
        ".",
        call. = FALSE
      )
    }

    matching_candidate %>%
      dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS))
  })
}


resolve_historical_first_contract_rr <- function() {
  manifest <- historical_first_contract_rr_manifest()
  internal_rr <- .load_historical_rr_registry() %>%
    dplyr::semi_join(manifest, by = "component_id") %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS))
  public_rr <- resolve_historical_first_contract_public_rr(manifest)
  resolved <- dplyr::bind_rows(public_rr, internal_rr)

  if (nrow(resolved) != nrow(manifest)) {
    stop(
      "The first historical contract did not resolve every RR component.",
      call. = FALSE
    )
  }
  validate_crf_selection(resolved)
  resolved
}


resolve_historical_first_contract_registry <- function() {
  resolved <- dplyr::bind_rows(
    resolve_historical_default_crfs(),
    resolve_historical_first_contract_rr()
  ) %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS))

  dc_groups <- load_dc_groups()
  aggregate_causes <- dc_groups %>%
    dplyr::filter(role == CRF_DC_ROLE_AGGREGATE) %>%
    dplyr::pull(cause)
  component_causes <- dc_groups %>%
    dplyr::filter(role == CRF_DC_ROLE_COMPONENT) %>%
    dplyr::pull(cause)
  has_pm25_deaths_aggregate <- any(
    resolved$pollutant == "PM25" &
      resolved$outcome == MEASURE_DEATHS &
      resolved$cause %in% aggregate_causes
  )
  if (has_pm25_deaths_aggregate) {
    resolved <- resolved %>%
      dplyr::mutate(
        double_counted = double_counted |
          (
            pollutant == "PM25" &
              outcome == MEASURE_DEATHS &
              cause %in% component_causes
          )
      )
  }

  validate_crf_selection(resolved)
  new_crfs_set(resolved)
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


run_historical_first_contract_parity_fixture <- function() {
  configuration_id <- historical_first_contract_id()
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  registry_crfs <- resolve_historical_first_contract_registry()
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
          diagnostic_folder = NULL
        )
      )
    }
  )

  list(
    configuration_id = configuration_id,
    registry_crfs = registry_crfs,
    legacy_paf = legacy_paf,
    registry_paf = registry_paf,
    legacy_impacts = legacy_impacts,
    registry_impacts = registry_impacts
  )
}
