historical_original_adult_registry <- function(cause) {
  .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_ORIGINAL, .data$cause == .env$cause) %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    new_crfs_set()
}


historical_original_adult_keys <- function(cause) {
  historical_original_adult_registry(cause) %>%
    dplyr::select(pollutant, cause, outcome)
}


filter_historical_original_adult <- function(data, cause) {
  data %>%
    dplyr::semi_join(
      historical_original_adult_keys(cause),
      by = c("pollutant", "cause", "outcome")
    )
}


arrange_historical_original_adult_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_original_adult_impacts <- function(impacts) {
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


run_historical_original_adult_parity_fixture <- function(cause) {
  configuration_id <- "legacy_default_gbd2019_only_2020"
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  registry_crfs <- historical_original_adult_registry(cause)
  legacy_cause_sources <- tibble::tibble(
    cause = cause,
    source = RR_ORIGINAL
  )
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
      legacy_paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        epi_version = epi_version,
        rr_sources = parse_rr_sources(legacy_cause_sources),
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
        rr_sources = legacy_cause_sources,
        epi_version = epi_version,
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
        epi_version = epi_version,
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


expect_historical_original_adult_parity <- function(cause) {
  results <- run_historical_original_adult_parity_fixture(cause)
  internal_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_ORIGINAL, .data$cause == .env$cause)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% internal_registry$component_id)

  testthat::expect_true(nrow(internal_registry) > 0)
  testthat::expect_true(all(
    internal_registry$reproduction_status == "parity_verified"
  ))
  testthat::expect_true(all(manifest$reproduction_status == "parity_verified"))
  testthat::expect_true(all(is.na(
    internal_registry$reproduction_blocking_reason
  )))
  testthat::expect_true(all(is.na(manifest$reproduction_blocking_reason)))
  testthat::expect_true(all(manifest$routing_checklist_complete))
  testthat::expect_true(all(manifest$routing_status == "ready"))
  testthat::expect_true(all(is.na(manifest$routing_blocking_reason)))

  legacy_paf <- results$legacy_paf %>%
    filter_historical_original_adult(cause) %>%
    arrange_historical_original_adult_paf()
  registry_paf <- results$registry_paf %>%
    filter_historical_original_adult(cause) %>%
    arrange_historical_original_adult_paf()

  testthat::expect_equal(nrow(legacy_paf), nrow(internal_registry))
  testthat::expect_equal(nrow(registry_paf), nrow(internal_registry))
  testthat::expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  testthat::expect_true(all(!is.na(registry_paf$central)))
  testthat::expect_true(any(registry_paf$central != 0))

  legacy_impacts <- results$legacy_impacts %>%
    filter_historical_original_adult(cause) %>%
    arrange_historical_original_adult_impacts()
  registry_impacts <- results$registry_impacts %>%
    filter_historical_original_adult(cause) %>%
    arrange_historical_original_adult_impacts()

  testthat::expect_equal(nrow(legacy_impacts), nrow(internal_registry) * 3L)
  testthat::expect_equal(nrow(registry_impacts), nrow(internal_registry) * 3L)
  testthat::expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  testthat::expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  testthat::expect_true(all(!is.na(registry_impacts$number)))
  testthat::expect_true(any(registry_impacts$number != 0))

  central_impacts <- registry_impacts %>%
    dplyr::filter(estimate == "central") %>%
    dplyr::arrange(outcome)
  expected_metadata <- internal_registry %>%
    dplyr::select(outcome, double_counted) %>%
    dplyr::arrange(outcome)
  testthat::expect_setequal(central_impacts$outcome, internal_registry$outcome)
  testthat::expect_true(all(central_impacts$age_group == "25+"))
  testthat::expect_equal(
    central_impacts %>% dplyr::select(outcome, double_counted),
    expected_metadata
  )

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    results$configuration_id
  )
  expected_fingerprint <-
    read_legacy_historical_configurations_hia_fingerprint() %>%
    dplyr::filter(configuration_id == results$configuration_id) %>%
    filter_historical_original_adult(cause)

  testthat::expect_equal(
    nrow(actual_fingerprint),
    nrow(internal_registry)
  )
  testthat::expect_equal(
    nrow(expected_fingerprint),
    nrow(internal_registry)
  )
  testthat::expect_equal(
    actual_fingerprint,
    expected_fingerprint,
    tolerance = 1e-12
  )
}
