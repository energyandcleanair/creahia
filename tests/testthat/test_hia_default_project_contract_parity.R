make_default_project_contract_raster <- function() {
  result <- raster::raster(
    nrows = 1,
    ncols = 2,
    xmn = 88,
    xmx = 90,
    ymn = 20,
    ymx = 21,
    crs = "+proj=longlat +datum=WGS84"
  )
  result[] <- c(1, 2)
  result
}


default_project_contract_specification <- function(crfs, rr_sources, epi) {
  boundary <- .resolve_hia_compatibility_boundary(
    crfs = crfs,
    rr_sources = parse_rr_sources(rr_sources),
    epi = epi,
    context = list(
      rr_sources_supplied = FALSE,
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
      pop_year = 2025,
      scale_base_year = 2019,
      scale_target_year = 2025,
      resolved_pop_year = 2025
    )
  )

  boundary$specification
}


run_default_project_contract_fixture <- function() {
  legacy_crfs <- get_crfs("default")
  default_rr_sources <- c(RR_GEMM, RR_ORIGINAL)
  epi <- legacy_hia_epi_fixture(
    crfs = legacy_crfs,
    rr_sources = default_rr_sources
  )
  epi_long <- legacy_hia_epi_long_fixture(default_rr_sources)
  specification <- default_project_contract_specification(
    crfs = legacy_crfs,
    rr_sources = default_rr_sources,
    epi = epi
  )
  registry_crfs <- .resolve_hia_diagnostic_registry_set(specification)
  empty_rr_sources <- legacy_hia_empty_rr_sources()
  conc_map <- legacy_default_fingerprint_conc_map()
  regions <- legacy_hia_fingerprint_regions()
  species <- c("pm25", "no2", "o3_8h", "so2")
  population_projection <- legacy_hia_population_projection_fixture()
  raster <- make_default_project_contract_raster()
  raster_inputs <- stats::setNames(
    rep(list(raster), length(species)),
    species
  )
  wrapper_flat_concentrations <- conc_map[[1]][[1]]
  seen <- new.env(parent = emptyenv())

  testthat::with_mocked_bindings(
    get_crfs = function(...) legacy_crfs,
    get_epi = function(...) epi,
    get_epi_count_long = function(...) epi_long,
    get_epi_location_id = function(...) 1,
    get_pop_year = function(...) 2020L,
    get_pop_proj = function() population_projection,
    diagnose_paf = function(...) invisible(NULL),
    combine_concs = function(...) tibble::tibble(),
    flatten_concs = function(...) wrapper_flat_concentrations,
    add_pop = function(concs, year_desired, ...) {
      seen$wrapper_pop_year <- year_desired
      concs
    },
    extract_concs_and_pop = function(...) conc_map,
    {
      legacy_paf <- capture_historical_first_contract_warnings(
        compute_hia_paf(
          conc_map = conc_map,
          species = species,
          regions = regions,
          rr_sources = default_rr_sources,
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
          rr_sources = default_rr_sources,
          epi = epi,
          crfs = legacy_crfs,
          pop_year = 2025,
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
          pop_year = 2025,
          diagnostic_folder = NULL
        )
      )
      activated_wrapper <- withr::with_options(
        list(
          creahia.hia_compatibility_routing = TRUE,
          creahia.hia_dual_run_observer = NULL,
          creahia.hia_readiness_shadow_observer = NULL
        ),
        capture_historical_first_contract_warnings(
          wrappers.compute_hia_two_images.default(
            perturbation_rasters = raster_inputs,
            baseline_rasters = raster_inputs,
            regions = regions,
            scenario_name = "legacy_default_baseline",
            scale_base_year = 2019,
            scale_target_year = 2025,
            diagnostic_folder = NULL
          )
        )
      )
    }
  )

  list(
    specification = specification,
    registry_crfs = registry_crfs,
    seen = seen,
    legacy_paf = legacy_paf,
    registry_paf = registry_paf,
    legacy_impacts = legacy_impacts,
    registry_impacts = registry_impacts,
    activated_wrapper = activated_wrapper
  )
}


test_that("the package defaults reproduce through the activated wrapper", {
  results <- run_default_project_contract_fixture()
  expected_calculation_warnings <- c(
    "PAF estimates are not properly ordered or of the same sign for Diabetes Deaths",
    "PAF estimates are not properly ordered or of the same sign for Diabetes YLDs"
  )

  expect_true(results$specification$readiness$ready)
  expect_equal(
    results$specification$input_contract,
    "component_resolved|legacy_profile_default|rr_sources|default"
  )
  expect_equal(nrow(results$registry_crfs), 33L)
  expect_equal(nrow(results$specification$components$rr), 18L)
  expect_equal(
    unique(results$specification$components$rr$source[
      results$specification$components$rr$cause == CAUSE_LRICHILD
    ]),
    RR_GEMM
  )

  expect_equal(
    sort(results$legacy_paf$warnings),
    sort(expected_calculation_warnings)
  )
  expect_equal(
    sort(results$registry_paf$warnings),
    sort(expected_calculation_warnings)
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
    results$activated_wrapper$value
  )
  expect_equal(nrow(legacy_impacts), 99L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_equal(activated_impacts, legacy_impacts, tolerance = 1e-12)
  expect_identical(results$seen$wrapper_pop_year, 2025)

  expect_equal(
    sort(results$legacy_impacts$warnings),
    sort(expected_calculation_warnings)
  )
  expect_equal(
    sort(results$registry_impacts$warnings),
    sort(expected_calculation_warnings)
  )
  expect_equal(
    sort(results$activated_wrapper$warnings),
    sort(c(
      expected_calculation_warnings,
      paste(
        "scale_base_year and scale_target_year are deprecated. Use pop_year",
        "instead, as the year you want to scale the population to. The base",
        "year is now determined automatically based on available data.",
        "\npop_year set to 2025"
      )
    ))
  )

  actual_fingerprint <- legacy_hia_fingerprint(
    activated_impacts,
    configuration_id = "actual_no_argument_default"
  )
  expected_fingerprint <- read_legacy_actual_default_hia_fingerprint() %>%
    dplyr::mutate(number = .data$number * 1.5)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})
