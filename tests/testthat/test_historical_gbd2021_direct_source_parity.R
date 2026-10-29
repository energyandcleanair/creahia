gbd2021_direct_source_specification <- function(crfs, epi) {
  .resolve_hia_compatibility_boundary(
    crfs = crfs,
    rr_sources = parse_rr_sources(RR_GBD2021),
    epi = epi,
    context = list(
      rr_sources_supplied = TRUE,
      epi_supplied = FALSE,
      crfs_supplied = FALSE,
      epi_version = "default",
      crfs_version = "default",
      crf_compute = "legacy",
      rr_selection_mode = "rr_sources",
      calc_causes = NULL,
      gbd_causes = NULL
    ),
    population_year_intent = .capture_population_year_intent(
      pop_year = 2020,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = 2020
    )
  )$specification
}


run_gbd2021_direct_source_parity_fixture <- function() {
  legacy_crfs <- get_crfs("default")
  epi <- legacy_hia_epi_fixture(legacy_crfs, RR_GBD2021)
  epi_long <- legacy_hia_epi_long_fixture(RR_GBD2021)
  specification <- gbd2021_direct_source_specification(legacy_crfs, epi)
  registry_crfs <- .resolve_hia_diagnostic_registry_set(specification)
  empty_rr_sources <- legacy_hia_empty_rr_sources()
  conc_map <- legacy_default_fingerprint_conc_map()
  regions <- legacy_hia_fingerprint_regions()
  species <- c("pm25", "no2", "o3_8h", "so2")

  testthat::with_mocked_bindings(
    get_crfs = function(...) legacy_crfs,
    get_epi = function(...) epi,
    get_epi_count_long = function(...) epi_long,
    get_epi_location_id = function(...) 1,
    get_pop_year = function(...) 2020L,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_paf <- capture_historical_first_contract_warnings(
        compute_hia_paf(
          conc_map = conc_map,
          species = species,
          regions = regions,
          rr_sources = RR_GBD2021,
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
          rr_sources = RR_GBD2021,
          epi = epi,
          crfs = legacy_crfs,
          pop_year = 2020,
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
          epi = epi,
          crfs = registry_crfs,
          pop_year = 2020,
          diagnostic_folder = NULL
        )
      )
      activated_impacts <- withr::with_options(
        list(
          creahia.hia_compatibility_routing = TRUE,
          creahia.hia_dual_run_observer = NULL,
          creahia.hia_readiness_shadow_observer = NULL
        ),
        capture_historical_first_contract_warnings(
          compute_hia(
            conc_map = conc_map,
            species = species,
            regions = regions,
            rr_sources = RR_GBD2021,
            pop_year = 2020,
            diagnostic_folder = NULL
          )
        )
      )
    }
  )

  list(
    specification = specification,
    registry_crfs = registry_crfs,
    legacy_paf = legacy_paf,
    registry_paf = registry_paf,
    legacy_impacts = legacy_impacts,
    registry_impacts = registry_impacts,
    activated_impacts = activated_impacts
  )
}


test_that("direct GBD 2021 source selection reproduces through Registry", {
  results <- run_gbd2021_direct_source_parity_fixture()
  rr_manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(source == RR_GBD2021)
  internal_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_GBD2021)

  expect_true(results$specification$readiness$ready)
  expect_equal(
    results$specification$input_contract,
    "component_resolved|legacy_profile_default|rr_sources|default"
  )
  expect_equal(nrow(results$specification$components$rr), 16L)
  expect_equal(nrow(results$registry_crfs), 31L)
  expect_equal(nrow(rr_manifest), 16L)
  expect_true(all(
    rr_manifest$coverage_status == "historical_internal_candidate"
  ))
  expect_equal(nrow(internal_registry), 16L)
  expect_true(all(rr_manifest$routing_status == "ready"))
  expect_true(all(rr_manifest$scientific_review_status == "not_reviewed"))

  expect_equal(
    sort(results$registry_paf$warnings),
    sort(results$legacy_paf$warnings)
  )
  expect_equal(
    arrange_historical_first_contract_paf(results$registry_paf$value),
    arrange_historical_first_contract_paf(results$legacy_paf$value),
    tolerance = 1e-12
  )

  legacy_impacts <- arrange_historical_first_contract_impacts(
    results$legacy_impacts$value
  )
  registry_impacts <- arrange_historical_first_contract_impacts(
    results$registry_impacts$value
  )
  activated_impacts <- arrange_historical_first_contract_impacts(
    results$activated_impacts$value
  )
  expect_equal(nrow(legacy_impacts), 93L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_equal(activated_impacts, legacy_impacts, tolerance = 1e-12)
  expect_equal(
    sort(results$registry_impacts$warnings),
    sort(results$legacy_impacts$warnings)
  )
  expect_equal(
    sort(results$activated_impacts$warnings),
    sort(results$legacy_impacts$warnings)
  )
})
