wrapper_crfs_raster <- function(value) {
  r <- terra::rast(
    xmin = 106, xmax = 107, ymin = -7, ymax = -6, res = 0.25,
    crs = "+proj=longlat +datum=WGS84"
  )
  r[] <- value
  r
}


run_wrapper_crfs_hia <- function(...) {
  regions <- sf::st_sf(
    region_id = "IDN",
    region_name = "Indonesia",
    country_id = "IDN",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(106, -7), c(107, -7), c(107, -6), c(106, -6), c(106, -7)
      ))),
      crs = 4326
    )
  )

  testthat::with_mocked_bindings(
    get_pop_count = function(grid_raster, ...) {
      pop <- raster::raster(grid_raster)
      pop[] <- 10000
      pop
    },
    wrappers.compute_hia_two_images(
      perturbation_rasters = list(
        pm25 = wrapper_crfs_raster(-5),
        no2 = wrapper_crfs_raster(-3)
      ),
      baseline_rasters = list(
        pm25 = wrapper_crfs_raster(40),
        no2 = wrapper_crfs_raster(30)
      ),
      regions = regions,
      pop_year = 2020,
      diagnostic_folder = NULL,
      ...
    ),
    .package = "creahia"
  )
}


test_that("the wrapper passes a crfs_set() selection to compute_hia()", {
  crfs <- crfs_set(presets = "gbd2019")
  hia <- run_wrapper_crfs_hia(crfs = crfs)

  # Only the selected PM2.5 curves run: no NO2 or other CRFs are added.
  expect_true(all(hia$pollutant == "PM25"))
  expect_setequal(unique(hia$cause), unique(crfs$cause))
})


test_that("the wrapper passes epi to compute_hia()", {
  epi <- get_epi("default")
  expect_equal(
    run_wrapper_crfs_hia(epi = epi),
    run_wrapper_crfs_hia()
  )
})
