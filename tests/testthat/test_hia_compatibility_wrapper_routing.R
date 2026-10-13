make_wrapper_routing_raster <- function() {
  result <- raster::raster(
    nrows = 1,
    ncols = 2,
    xmn = 126,
    xmx = 128,
    ymn = 36,
    ymx = 37,
    crs = "+proj=longlat +datum=WGS84"
  )
  result[] <- c(2, 4)
  result
}


make_wrapper_routing_concentrations <- function(scenario = "wrapper_scenario") {
  stats::setNames(
    list(list(
      KOR = data.frame(
        conc_baseline_pm25 = c(28, 35),
        conc_scenario_pm25 = c(26, 31),
        pop = c(2e6, 3e6)
      )
    )),
    scenario
  )
}


make_wrapper_routing_impacts <- function(scenario = "wrapper_scenario") {
  tibble::tibble(
    scenario = scenario,
    pollutant = "PM25",
    cause = "Stroke",
    outcome = "Deaths",
    region_id = "KOR",
    estimate = "central",
    number = 1
  )
}


make_wrapper_routing_calc_causes <- function() {
  c(
    "Stroke_YLDs",
    "Diabetes_YLDs",
    "COPD_YLDs",
    "NCD.LRI_Deaths",
    "IHD_Deaths",
    "Stroke_Deaths",
    "COPD_Deaths",
    "LC_Deaths",
    "LRI_Deaths",
    "LRI.child_Deaths",
    "Diabetes_Deaths"
  )
}


make_wrapper_routing_flat_concentrations <- function() {
  data.frame(
    conc_baseline_pm25 = c(28, 35),
    conc_scenario_pm25 = c(26, 31),
    pop = c(2e6, 3e6)
  )
}


test_that("default wrapper preserves list output on the activated route", {
  seen <- new.env(parent = emptyenv())
  raster <- make_wrapper_routing_raster()
  conc_map <- make_wrapper_routing_concentrations()
  expected_hia <- make_wrapper_routing_impacts()
  old_options <- options(
    creahia.hia_compatibility_routing = TRUE,
    creahia.hia_dual_run_observer = NULL,
    creahia.hia_readiness_shadow_observer = NULL
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    combine_concs = function(...) tibble::tibble(),
    flatten_concs = function(...) make_wrapper_routing_flat_concentrations(),
    add_pop = function(concs, ...) concs,
    extract_concs_and_pop = function(...) conc_map,
    compute_hia_paf = function(..., crfs, rr_sources, crf_compute) {
      seen$crfs <- crfs
      seen$rr_sources <- rr_sources
      seen$crf_compute <- crf_compute
      tibble::tibble()
    },
    compute_hia_impacts = function(...) expected_hia,
    get_pop_year = function(...) 2022,
    {
      result <- wrappers.compute_hia_two_images.default(
        perturbation_rasters = list(pm25 = raster),
        baseline_rasters = list(pm25 = raster),
        regions = tibble::tibble(region_id = "KOR"),
        scenario_name = "wrapper_scenario",
        pop_year = 2022,
        scale_base_year = NULL,
        crfs_version = "C40",
        epi_version = "C40",
        return_concentrations = TRUE,
        diagnostic_folder = NULL,
        calc_causes = make_wrapper_routing_calc_causes(),
        gbd_causes = "default"
      )
    }
  )

  expect_named(result, c("hia", "concentrations"))
  expect_identical(result$hia, expected_hia)
  expect_s3_class(result$concentrations, "data.frame")
  expect_equal(nrow(result$concentrations), 1L)
  expect_equal(result$concentrations$conc_baseline_pm25, 32.2)
  expect_equal(result$concentrations$conc_scenario_pm25, 29)
  expect_equal(result$concentrations$pop, 5e6)
  expect_identical(seen$crf_compute, "registry")
  expect_s3_class(seen$crfs, "creahia_crf_set")
  expect_false(has_rr_sources(seen$rr_sources))
})


test_that("character wrapper preserves its activated-route RDS contract", {
  seen <- new.env(parent = emptyenv())
  output_dir <- withr::local_tempdir()
  raster <- make_wrapper_routing_raster()
  raster_path <- file.path(output_dir, "pm25.tif")
  raster::writeRaster(raster, raster_path, overwrite = TRUE)
  conc_map <- make_wrapper_routing_concentrations("scenario_a")
  expected_hia <- make_wrapper_routing_impacts("scenario_a")
  old_options <- options(
    creahia.hia_compatibility_routing = TRUE,
    creahia.hia_dual_run_observer = NULL,
    creahia.hia_readiness_shadow_observer = NULL
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    align_raster_to_grid = function(r, ...) r,
    combine_concs = function(...) tibble::tibble(),
    flatten_concs = function(...) make_wrapper_routing_flat_concentrations(),
    add_pop = function(concs, ...) concs,
    extract_concs_and_pop = function(...) conc_map,
    compute_hia_paf = function(..., crfs, rr_sources, crf_compute) {
      seen$crfs <- crfs
      seen$rr_sources <- rr_sources
      seen$crf_compute <- crf_compute
      tibble::tibble()
    },
    compute_hia_impacts = function(...) expected_hia,
    get_pop_year = function(year_desired) {
      seen$population_year <- year_desired
      2019
    },
    {
      expect_warning(
        wrappers.compute_hia_two_images.character(
          scenarios = "scenario_a",
          perturbation_rasters_table = tibble::tibble(
            scenario = "scenario_a",
            species = "pm25",
            path = raster_path
          ),
          baseline_rasters_table = tibble::tibble(species = "pm25"),
          grid_raster = raster,
          regions = tibble::tibble(region_id = "KOR"),
          scale_base_year = 2015,
          scale_target_year = 2019,
          crfs_version = "C40",
          epi_version = "C40",
          return_concentrations = TRUE,
          output_folder = output_dir,
          diagnostic_folder = NULL,
          calc_causes = make_wrapper_routing_calc_causes(),
          gbd_causes = "default",
          custom_glue = "hia_GBD2019_{scen}.RDS"
        ),
        "pop_year set to 2019",
        fixed = TRUE
      )
    }
  )

  result_path <- file.path(output_dir, "hia", "hia_GBD2019_scenario_a.RDS")
  expect_true(file.exists(result_path))
  saved <- readRDS(result_path)
  expect_named(saved, c("hia", "concentrations"))
  expect_identical(saved$hia, expected_hia)
  expect_s3_class(saved$concentrations, "data.frame")
  expect_equal(nrow(saved$concentrations), 1L)
  expect_equal(saved$concentrations$conc_baseline_pm25, 32.2)
  expect_equal(saved$concentrations$conc_scenario_pm25, 29)
  expect_equal(saved$concentrations$pop, 5e6)
  expect_identical(seen$population_year, 2019)
  expect_identical(seen$crf_compute, "registry")
  expect_s3_class(seen$crfs, "creahia_crf_set")
  expect_false(has_rr_sources(seen$rr_sources))
})
