

get_random_exposure_hia <- function(levels, min=20, max=60, target=0, calc_causes = "GEMM and GBD"){

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

  baseline_rast[] <- runif(prod(dim(baseline_rast)), min, max)

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  perturbation_rast <- target-baseline_rast

  # Compute HIAs
  lapply(levels, function(level){
    creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = list(pm25 = perturbation_rast),
      baseline_rasters = list(pm25 = baseline_rast),
      scale_base_year = NULL, # Just to avoid unnecessary warning
      scale_target_year = NULL,  # Just to avoid unnecessary warning
      pop_year=2020,
      administrative_level = level,
      administrative_res = "low",
      administrative_iso3s = "BGD",
      epi_version = "gbd2019",
      calc_causes = calc_causes
    ) %>%
      mutate(level=level)
  }) %>%
    bind_rows()
}

test_that("HIAs are consistent across levels", {


  hias <- get_random_exposure_hia(levels=c(0,1))

  # Comparison
  comparison <- hias %>%
    group_by(level, Outcome, Cause, scenario, estimate) %>%
    summarise_at("number", sum) %>%
    ungroup() %>%
    group_by(Outcome, Cause, scenario, estimate) %>%
    summarise(deviation=(max(number) - min(number))/mean(number)) %>%
    ungroup() %>%
    arrange(desc(abs(deviation)))


  # For central, expect very close results
  max_deviation_central <- comparison %>% filter(estimate=="central") %>% summarise(max_deviation=max(abs(deviation), na.rm=T))
  testthat::expect_lt(max_deviation_central$max_deviation, 0.05)

})

