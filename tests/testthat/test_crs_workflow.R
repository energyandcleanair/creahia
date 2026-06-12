library(testthat)

# ==============================================================================
# Cross-CRS workflow test
#
# creaexposure::get_concentration() now uses terra::project() instead of
# terra::resample() when fitting to grid_raster, so the result is returned in
# the grid's CRS (e.g. a UTM grid from a CALPUFF workflow) rather than requiring
# the grid to already be in the source lon/lat CRS.
#
# This test verifies that:
#   1. get_concentration() honours the grid CRS (lon/lat AND UTM).
#   2. The full creahia extraction pipeline (add_pop -> get_adm ->
#      extract_concs_and_pop) runs end-to-end on a non-lon/lat grid.
#   3. Results (total population and population-weighted mean concentration) are
#      consistent between the lon/lat and UTM workflows, i.e. switching to
#      project() does not distort the HIA inputs.
#
# Requires GIS_DIR with pm25/vandonkelaar concentration + GPW population data,
# so it is skipped when that data is unavailable.
# ==============================================================================

library(terra)
library(dplyr)

skip_if_no_gis_data <- function() {
  ok <- tryCatch({
    length(creaexposure::get_concentration_available_years(
      "pm25", source = "vandonkelaar")) > 0 &&
      length(creahia:::get_pop_years_available()) > 0
  }, error = function(e) FALSE)
  testthat::skip_if(!isTRUE(ok), "GIS_DIR concentration/population data not available")
}

# Bangladesh bounding box, UTM zone 46N == EPSG:32646
make_grid_lonlat <- function(res = 0.02) {
  terra::rast(xmin = 88, xmax = 92, ymin = 20, ymax = 27,
              res = res, crs = "epsg:4326")
}
make_grid_utm <- function(res_m = 2000) {
  terra::project(make_grid_lonlat(), "epsg:32646", res = res_m)
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


test_that("get_concentration returns rasters in the grid CRS (lon/lat and UTM)", {
  skip_if_no_gis_data()

  g_ll  <- make_grid_lonlat()
  g_utm <- make_grid_utm()

  conc_ll  <- creaexposure::get_concentration("pm25", source = "vandonkelaar",
                                              year = 2020, grid_raster = g_ll)
  conc_utm <- creaexposure::get_concentration("pm25", source = "vandonkelaar",
                                              year = 2020, grid_raster = g_utm)

  # The core of the resample -> project change: output adopts the grid CRS.
  expect_true(terra::same.crs(conc_ll,  g_ll))
  expect_true(terra::same.crs(conc_utm, g_utm))
  expect_false(terra::same.crs(conc_utm, conc_ll))

  # Sanity: plausible PM2.5 values, not all-NA (the failure mode project() fixes).
  expect_gt(mean(terra::values(conc_utm), na.rm = TRUE), 0)
  expect_false(all(is.na(terra::values(conc_utm))))
})


test_that("HIA extraction is consistent across lon/lat and UTM grids", {
  skip_if_no_gis_data()

  g_ll  <- make_grid_lonlat()
  g_utm <- make_grid_utm()

  conc_ll  <- creaexposure::get_concentration("pm25", source = "vandonkelaar",
                                              year = 2020, grid_raster = g_ll)
  conc_utm <- creaexposure::get_concentration("pm25", source = "vandonkelaar",
                                              year = 2020, grid_raster = g_utm)

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
