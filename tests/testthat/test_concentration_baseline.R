library(testthat)

# ==============================================================================
# Tests for baseline concentration wrappers (concentration.R)
# These test the integration contract between creahia and creaexposure.
# ==============================================================================

# Helper: create a SpatRaster with a given unit
make_raster_with_unit <- function(vals = 25, unit = NULL,
                                  nrows = 10, ncols = 10,
                                  xmin = 0, xmax = 10, ymin = 0, ymax = 10) {
  r <- terra::rast(nrows = nrows, ncols = ncols,
                   xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   vals = vals)
  if (!is.null(unit)) terra::units(r) <- unit
  r
}


# --- .validate_unit -----------------------------------------------------------

test_that(".validate_unit passes for correct unit", {
  r <- make_raster_with_unit(unit = "µg/m3")
  expect_silent(creahia:::.validate_unit(r, enc2native("µg/m3")))
})

test_that(".validate_unit errors when unit is missing", {
  r <- make_raster_with_unit(unit = NULL)
  expect_error(
    creahia:::.validate_unit(r, enc2native("µg/m3")),
    "No unit set"
  )
})

test_that(".validate_unit errors when unit is wrong", {
  r <- make_raster_with_unit(unit = "kg/m3")
  expect_error(
    creahia:::.validate_unit(r, enc2native("µg/m3")),
    "Unexpected unit"
  )
})


# --- get_conc_baseline_pm25 ---------------------------------------------------

test_that("get_conc_baseline_pm25 calls creaexposure with correct args", {
  captured <- list()
  mock_raster <- make_raster_with_unit(vals = 25, unit = "µg/m3")
  grid <- make_raster_with_unit(vals = 0)

  local_mocked_bindings(
    get_concentration = function(pollutant, source, year, grid_raster, ...) {
      captured$pollutant <<- pollutant
      captured$source <<- source
      captured$year <<- year
      mock_raster
    },
    .package = "creaexposure"
  )

  result <- get_conc_baseline_pm25(target_year = 2022, grid_raster = grid)
  expect_equal(captured$pollutant, "pm25")
  expect_equal(captured$source, "vandonkelaar")
  expect_equal(captured$year, 2022)
})

test_that("get_conc_baseline_pm25 rejects wrong unit from creaexposure", {
  mock_raster <- make_raster_with_unit(vals = 25, unit = "kg/m3")
  grid <- make_raster_with_unit(vals = 0)

  local_mocked_bindings(
    get_concentration = function(...) mock_raster,
    .package = "creaexposure"
  )

  expect_error(get_conc_baseline_pm25(target_year = 2022, grid_raster = grid),
               "Unexpected unit")
})


# --- get_conc_baseline_o3 -----------------------------------------------------

test_that("get_conc_baseline_o3 maps species='o3' to variant='m3m'", {
  captured <- list()
  mock_raster <- make_raster_with_unit(vals = 50)
  grid <- make_raster_with_unit(vals = 0)

  local_mocked_bindings(
    get_concentration = function(pollutant, source, variant, grid_raster, ...) {
      captured$variant <<- variant
      mock_raster
    },
    .package = "creaexposure"
  )

  get_conc_baseline_o3(grid_raster = grid, species = "o3")
  expect_equal(captured$variant, "m3m")
})

test_that("get_conc_baseline_o3 maps species='o3_8h' to variant='sm8h'", {
  captured <- list()
  mock_raster <- make_raster_with_unit(vals = 50)
  grid <- make_raster_with_unit(vals = 0)

  local_mocked_bindings(
    get_concentration = function(pollutant, source, variant, grid_raster, ...) {
      captured$variant <<- variant
      mock_raster
    },
    .package = "creaexposure"
  )

  get_conc_baseline_o3(grid_raster = grid, species = "o3_8h")
  expect_equal(captured$variant, "sm8h")
})


# --- get_conc_baseline_no2 ----------------------------------------------------

test_that("get_conc_baseline_no2 passes scale_year to creaexposure", {
  captured <- list()
  mock_raster <- make_raster_with_unit(vals = 20, unit = "µg/m3")
  grid <- make_raster_with_unit(vals = 0)

  local_mocked_bindings(
    get_concentration = function(pollutant, source, grid_raster, scale_year, ...) {
      captured$scale_year <<- scale_year
      mock_raster
    },
    .package = "creaexposure"
  )

  get_conc_baseline_no2(grid_raster = grid, no2_targetyear = 2023)
  expect_equal(captured$scale_year, 2023)
})

test_that("get_conc_baseline_no2 rejects wrong unit from creaexposure", {
  mock_raster <- make_raster_with_unit(vals = 20, unit = "molecules/cm2")
  grid <- make_raster_with_unit(vals = 0)

  local_mocked_bindings(
    get_concentration = function(...) mock_raster,
    .package = "creaexposure"
  )

  expect_error(get_conc_baseline_no2(grid_raster = grid, no2_targetyear = 2023),
               "Unexpected unit")
})
