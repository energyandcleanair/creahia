
test_that("Confidence interval makes sense when comparing two scenarios", {


  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
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

  hia_1 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p1),
    baseline_rasters = list(pm25 = m),
    scale_base_year = 2020,
    scale_target_year = 2020,
    administrative_level = 0,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    epi_version = "gbd2019",
    calc_causes = "GBD only"
  )

  hia_2 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p2),
    baseline_rasters = list(pm25 = m),
    scale_base_year = 2020,
    scale_target_year = 2020,
    administrative_level = 0,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    epi_version = "gbd2019",
    calc_causes = "GBD only"
  )


  # Because p1 is a larger (negative) perturbation
  # We expect number_1 to be larger (negative) numbers
  join_by <- setdiff(names(hia_1), c("number"))

  error <- hia_1 %>%
    left_join(hia_2, by = join_by, suffix = c("_1", "_2")) %>%
    filter(number_1 > number_2)

  testthat::expect_true(nrow(error) == 0)



  hia_3 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p2),
    baseline_rasters = list(pm25 = m),
    scale_base_year = 2020,
    scale_target_year = 2020,
    administrative_level = 0,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    epi_version = "gbd2019",
    calc_causes = "GEMM and GBD"
  )


  # Expect the order low < central < high to be consistent
  inconsistent_order <- hia_3 %>%
    spread(estimate, number) %>%
    mutate(order=case_when(
      low <= central & central <= high ~ "increasing",
      low >= central & central >= high ~ "decreasing",
      T ~ "inconsistent"
    )) %>%
    summarise(ok = n_distinct(order) == 1 & !("inconsistent" %in% order))

  testthat::expect_true(inconsistent_order$ok)

})
