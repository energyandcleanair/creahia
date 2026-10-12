historical_rr_parity_registry <- function(source, cause) {
  .load_historical_rr_registry() %>%
    dplyr::filter(
      .data$source == .env$source,
      .data$cause == .env$cause
    ) %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    new_crfs_set()
}


historical_rr_parity_keys <- function(source, cause, impacts = FALSE) {
  keys <- historical_rr_parity_registry(source, cause) %>%
    dplyr::select(pollutant, cause, outcome)

  if (impacts) {
    keys <- keys %>%
      dplyr::mutate(
        outcome = dplyr::if_else(
          cause == CAUSE_LRICHILD & outcome == MEASURE_DEATHS,
          "Deaths.child",
          outcome
        )
      )
  }

  keys
}


filter_historical_rr_parity <- function(data, source, cause, impacts = FALSE) {
  data %>%
    dplyr::semi_join(
      historical_rr_parity_keys(source, cause, impacts = impacts),
      by = c("pollutant", "cause", "outcome")
    )
}


arrange_historical_rr_parity_paf <- function(paf) {
  paf %>%
    dplyr::arrange(scenario, region_id, pollutant, cause, outcome)
}


arrange_historical_rr_parity_impacts <- function(impacts) {
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


run_historical_rr_parity_fixture <- function(
  source,
  cause,
  configuration_id
) {
  configuration <- load_historical_fingerprint_configurations() %>%
    dplyr::filter(mapping_id == configuration_id)
  legacy_crfs <- get_crfs(
    legacy_hia_configuration_value(configuration, "legacy_crfs_version")
  )
  legacy_rr_sources <- legacy_hia_configuration_rr_sources(configuration)
  registry_crfs <- historical_rr_parity_registry(source, cause)
  legacy_cause_sources <- tibble::tibble(
    cause = cause,
    source = source
  )
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


expect_historical_rr_parity <- function(source, cause, configuration_id) {
  results <- run_historical_rr_parity_fixture(
    source = source,
    cause = cause,
    configuration_id = configuration_id
  )
  internal_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(
      .data$source == .env$source,
      .data$cause == .env$cause
    )
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
    filter_historical_rr_parity(source, cause) %>%
    arrange_historical_rr_parity_paf()
  registry_paf <- results$registry_paf %>%
    filter_historical_rr_parity(source, cause) %>%
    arrange_historical_rr_parity_paf()

  testthat::expect_equal(nrow(legacy_paf), nrow(internal_registry))
  testthat::expect_equal(nrow(registry_paf), nrow(internal_registry))
  testthat::expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  testthat::expect_true(all(!is.na(registry_paf$central)))
  testthat::expect_true(any(registry_paf$central != 0))

  legacy_impacts <- results$legacy_impacts %>%
    filter_historical_rr_parity(source, cause, impacts = TRUE) %>%
    arrange_historical_rr_parity_impacts()
  registry_impacts <- results$registry_impacts %>%
    filter_historical_rr_parity(source, cause, impacts = TRUE) %>%
    arrange_historical_rr_parity_impacts()

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
    dplyr::transmute(
      outcome = dplyr::if_else(
        cause == CAUSE_LRICHILD & outcome == MEASURE_DEATHS,
        "Deaths.child",
        outcome
      ),
      double_counted
    ) %>%
    dplyr::arrange(outcome)
  expected_age_group <- if (
    all(internal_registry$age_low == 0) &&
      all(internal_registry$age_high == 4)
  ) {
    "0-4"
  } else {
    "25+"
  }
  testthat::expect_setequal(central_impacts$outcome, expected_metadata$outcome)
  testthat::expect_true(all(central_impacts$age_group == expected_age_group))
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
    filter_historical_rr_parity(source, cause, impacts = TRUE)

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
