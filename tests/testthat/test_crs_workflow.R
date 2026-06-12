library(testthat)

# ==============================================================================
# Cross-CRS workflow test
#
# creaexposure::get_concentration() now uses terra::project() instead of
# terra::resample() when fitting to grid_raster, so the result is returned in
# the grid's CRS (e.g. a UTM grid from a CALPUFF workflow) rather than requiring
# the grid to already be in the source lon/lat CRS.
#
# This test verifies that the full creahia extraction/HIA pipeline (add_pop ->
# get_adm -> extract_concs_and_pop -> compute_hia) runs end-to-end on a non
# lon/lat grid, and that the results are consistent between an equivalent lon/lat
# and UTM workflow, i.e. switching to project() does not distort the HIA inputs.
#
# The exposure field is synthesised here (a smooth, deterministic PM2.5 map
# projected from lon/lat to UTM) rather than fetched via creaexposure, so the
# test does not depend on the vandonkelaar concentration data (which is not part
# of the CI image). It still requires the GPW population and GADM boundary data,
# both of which ARE bundled into the CI image (see tests/gcs_test_files.txt).
# ==============================================================================

library(terra)
library(dplyr)

skip_if_no_gis_data <- function() {
  ok <- tryCatch({
    length(creahia:::get_pop_years_available()) > 0
  }, error = function(e) FALSE)
  testthat::skip_if(!isTRUE(ok), "GIS_DIR population/boundary data not available")
}

# Bangladesh bounding box, UTM zone 46N == EPSG:32646
make_grid_lonlat <- function(res = 0.02) {
  terra::rast(xmin = 88, xmax = 92, ymin = 20, ymax = 27,
              res = res, crs = "epsg:4326")
}
make_grid_utm <- function(res_m = 2000) {
  terra::project(make_grid_lonlat(), "epsg:32646", res = res_m)
}

# Synthesise a smooth, deterministic PM2.5 baseline (~20-80 ug/m3) on a lon/lat
# grid. A coarse random field bilinearly resampled to the grid gives a spatially
# varying but smooth surface that reprojects near-losslessly, so the cross-CRS
# consistency checks reflect the pop/region CRS plumbing rather than artefacts of
# interpolating white noise.
make_conc_lonlat <- function(grid_ll) {
  coarse <- terra::rast(terra::ext(grid_ll), resolution = 0.5, crs = "epsg:4326")
  set.seed(42)
  terra::values(coarse) <- runif(terra::ncell(coarse), min = 20, max = 80)
  conc <- terra::resample(coarse, grid_ll, method = "bilinear")
  names(conc) <- "pm25"
  conc
}

# Run the full extraction pipeline for a given grid, returning the per-region
# concentration/population tibble. `conc` is the baseline concentration raster.
run_extract <- function(grid, conc) {
  baseline   <- conc %>% creahelpers::to_raster()
  perturbation <- (conc * 0) %>% creahelpers::to_raster()  # zero-perturbation scenario
  names(baseline) <- names(perturbation) <- "pm25"

  conc_perturbation <- tibble(species = "pm25",
                              conc_perturbation = list(perturbation),
                              scenario = "test")
  conc_baseline <- tibble(species = "pm25",
                          conc_baseline = list(baseline))

  concs <- creahia::combine_concs(conc_perturbation, conc_baseline) %>%
    creahia::flatten_concs() %>%
    creahia::add_pop(grid, year_desired = 2020)

  regions <- creahia::get_adm(grid, admin_level = 0, res = "low", iso3s = "BGD")

  creahia::extract_concs_and_pop(concs, regions, "pm25")
}

summarise_extract <- function(conc_regions) {
  df <- bind_rows(conc_regions$test) %>% bind_rows()
  list(
    pop  = sum(df$pop, na.rm = TRUE),
    conc = weighted.mean(df$conc_baseline_pm25, df$pop, na.rm = TRUE)
  )
}


test_that("HIA extraction is consistent across lon/lat and UTM grids", {
  skip_if_no_gis_data()

  g_ll  <- make_grid_lonlat()
  g_utm <- make_grid_utm()

  # Same underlying exposure field, expressed on each grid's CRS.
  conc_ll  <- make_conc_lonlat(g_ll)
  conc_utm <- terra::project(conc_ll, g_utm)

  # The UTM workflow must run end-to-end without error.
  res_ll  <- run_extract(g_ll,  conc_ll)
  res_utm <- expect_no_error(run_extract(g_utm, conc_utm))

  s_ll  <- summarise_extract(res_ll)
  s_utm <- summarise_extract(res_utm)

  # Total population over Bangladesh should agree between the two projections.
  expect_equal(s_utm$pop, s_ll$pop, tolerance = 0.03)

  # Population-weighted mean PM2.5 should agree between the two projections.
  expect_equal(s_utm$conc, s_ll$conc, tolerance = 0.05)
})


# Run the full HIA wrapper (which internally runs compute_hia) for a given grid,
# returning total central Deaths over Bangladesh. A spatially-constant baseline
# is used so reprojection is near-lossless and any difference reflects the
# pop/region CRS plumbing rather than interpolation of the exposure field.
run_compute_hia_deaths <- function(grid, baseline_val = 60, target_val = 20) {
  baseline <- terra::setValues(grid, baseline_val) %>% creahelpers::to_raster()
  perturbation <- terra::setValues(grid, target_val - baseline_val) %>% creahelpers::to_raster()
  names(baseline) <- names(perturbation) <- "pm25"

  hia <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = perturbation),
    baseline_rasters     = list(pm25 = baseline),
    administrative_level = 0,
    administrative_res   = "low",
    administrative_iso3s = "BGD",
    pop_year             = 2020,
    epi_version          = "gbd2023",
    scale_base_year      = NULL,
    scale_target_year    = NULL
  )

  hia %>%
    dplyr::filter(outcome == "Deaths", estimate == "central") %>%
    dplyr::summarise(number = sum(number, na.rm = TRUE)) %>%
    dplyr::pull(number)
}


test_that("compute_hia runs and agrees across lon/lat and UTM grids", {
  skip_if_no_gis_data()

  g_ll  <- make_grid_lonlat(res = 0.05)
  g_utm <- terra::project(g_ll, "epsg:32646", res = 5000)

  deaths_ll  <- run_compute_hia_deaths(g_ll)
  deaths_utm <- expect_no_error(run_compute_hia_deaths(g_utm))

  # Both must produce a non-trivial HIA (avoided deaths from the perturbation).
  expect_lt(deaths_ll,  0)
  expect_lt(deaths_utm, 0)

  # Total avoided deaths should agree between the two projections.
  expect_equal(deaths_utm, deaths_ll, tolerance = 0.03)
})
