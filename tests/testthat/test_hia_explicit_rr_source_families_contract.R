make_explicit_rr_source_contract_raster <- function() {
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


run_explicit_rr_source_wrapper_contract <- function(source) {
  legacy_crfs <- get_crfs("default")
  epi <- legacy_hia_epi_fixture(legacy_crfs, source)
  epi_long <- legacy_hia_epi_long_fixture(source)
  conc_map <- legacy_default_fingerprint_conc_map()
  regions <- legacy_hia_fingerprint_regions()
  raster_input <- list(pm25 = make_explicit_rr_source_contract_raster())
  flat_concentrations <- conc_map[[1]][[1]]

  testthat::with_mocked_bindings(
    get_crfs = function(...) legacy_crfs,
    get_epi = function(...) epi,
    get_epi_count_long = function(...) epi_long,
    get_epi_location_id = function(...) 1,
    get_pop_year = function(...) 2020L,
    diagnose_paf = function(...) invisible(NULL),
    combine_concs = function(...) tibble::tibble(),
    flatten_concs = function(...) flat_concentrations,
    add_pop = function(concs, year_desired, ...) concs,
    extract_concs_and_pop = function(...) conc_map,
    {
      legacy <- withr::with_options(
        list(
          creahia.hia_compatibility_routing = FALSE,
          creahia.hia_dual_run_observer = NULL,
          creahia.hia_readiness_shadow_observer = NULL
        ),
        capture_historical_first_contract_warnings(
          wrappers.compute_hia_two_images.default(
            perturbation_rasters = raster_input,
            baseline_rasters = raster_input,
            regions = regions,
            scenario_name = paste0("explicit_", source),
            pop_year = 2020,
            scale_base_year = NULL,
            rr_sources = source,
            diagnostic_folder = NULL
          )
        )
      )
      activated <- withr::with_options(
        list(
          creahia.hia_compatibility_routing = TRUE,
          creahia.hia_dual_run_observer = NULL,
          creahia.hia_readiness_shadow_observer = NULL
        ),
        capture_historical_first_contract_warnings(
          wrappers.compute_hia_two_images.default(
            perturbation_rasters = raster_input,
            baseline_rasters = raster_input,
            regions = regions,
            scenario_name = paste0("explicit_", source),
            pop_year = 2020,
            scale_base_year = NULL,
            rr_sources = source,
            diagnostic_folder = NULL
          )
        )
      )
    }
  )

  list(legacy = legacy, activated = activated)
}


test_that("public wrapper preserves all accepted direct RR source families", {
  sources <- c(
    RR_ORIGINAL,
    RR_GBD2019,
    RR_GBD2021,
    RR_GEMM,
    RR_FUSION
  )

  results <- purrr::map(sources, run_explicit_rr_source_wrapper_contract)
  names(results) <- sources

  purrr::walk2(results, sources, function(result, source) {
    legacy <- arrange_historical_first_contract_impacts(result$legacy$value)
    activated <- arrange_historical_first_contract_impacts(
      result$activated$value
    )

    expect_true(nrow(legacy) > 0L, info = source)
    expect_equal(activated, legacy, tolerance = 1e-12, info = source)
    expect_equal(
      sort(result$activated$warnings),
      sort(result$legacy$warnings),
      info = source
    )
  })
})
