

get_uniform_exposure_hia <- function(baseline, target, calc_causes = "GEMM and GBD"){

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  res <- 0.01
  baseline_rast <- terra::rast(
    xmin=88,
    xmax=92,
    ymin=20,
    ymax=27,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  baseline_rast[] <- baseline

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  perturbation_rast <- target-baseline_rast

  creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = perturbation_rast),
    baseline_rasters = list(pm25 = baseline_rast),
    scale_base_year = NULL, # Just to avoid unnecessary warning
    scale_target_year = NULL,  # Just to avoid unnecessary warning
    pop_year=2020,
    administrative_level = 0,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    epi_version = "gbd2019",
    calc_causes = calc_causes
  )

}

test_that("Confidence interval makes sense when comparing two scenarios", {


  hia_1 <- get_uniform_exposure_hia(60, 0)
  hia_2 <- get_uniform_exposure_hia(60, 5)


  # Because p1 is a larger (negative) perturbation
  # We expect number_1 to be larger (negative) numbers
  join_by <- setdiff(names(hia_1), c("number"))

  error <- hia_1 %>%
    left_join(hia_2, by = join_by, suffix = c("_1", "_2")) %>%
    filter(number_1 > number_2)

  testthat::expect_true(nrow(error) == 0)

})


test_that("Order of estimates is consistent", {


  hia <- get_uniform_exposure_hia(60, 0)

  # Expect the order low < central < high to be consistent
  inconsistent_order <- hia %>%
    spread(estimate, number) %>%
    mutate(order=case_when(
      low <= central & central <= high ~ "increasing",
      low >= central & central >= high ~ "decreasing",
      T ~ "inconsistent"
    )) %>%
    summarise(ok = n_distinct(order) == 1 & !("inconsistent" %in% order))

  testthat::expect_true(inconsistent_order$ok)

})


test_that("Estimates are of the same sign with uniform exposure", {

  # This can happen when small changes at low level?
  hia <- get_uniform_exposure_hia(6, 5)

  hia %>%
    summarise(ok = all(number >= 0) | all(number <= 0)) %>%
    pull(ok) %>%
    testthat::expect_true()

})
