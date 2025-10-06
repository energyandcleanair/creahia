library(testthat)

test_that("Results using rr_sources is simlar to old way using calc_causes", {


  res <- 0.01
  m <- terra::rast(
    xmin=88,
    xmax=92,
    ymin=20,
    ymax=27,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  m[] <- 60

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  p1 <- -m
  p2 <- 5-m


  # GEMM and GBD
  hia_old <- creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = list(pm25 = p1),
      baseline_rasters = list(pm25 = m),
      pop_year = 2020,
      administrative_level = 1,
      administrative_res = "low",
      administrative_iso3s = "BGD",
      calc_causes = 'GEMM and GBD',
      gbd_version = "gbd2019",
      gbd_causes = "default"
    )

  hia_new <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p1),
    baseline_rasters = list(pm25 = m),
    pop_year = 2020,
    administrative_level = 1,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    rr_source = c(RR_GEMM, RR_ORIGINAL)
  )

  comparison <- full_join(
    hia_old %>% rename(number_old=number),
    hia_new %>% rename(number_new=number)
  ) %>%
  # These should differ on LRI.child: in GEMM and GBD, LRI.child is from GBD
  # However, when we define rr_sources = c(GEMM, ORIGINAL) it will takes GEMM's
  filter(cause != 'LRI.child')

  expect_equal(
    comparison$number_old,
    comparison$number_new,
    tolerance = 0.01
  )
})
