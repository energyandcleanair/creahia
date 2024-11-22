# Unless otherwise specified, we want central estimates of health impacts numbers to remain the same
# across versions.
# This is an initial test. As new versions come up, it might be beneficial to include more cases
# and allow for estimates to vary in certain version changes.


get_fingerprint_bgd <- function(){

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  current_version <- as.character(packageVersion("creahia"))
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

  hia <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p1),
    baseline_rasters = list(pm25 = m),
    pop_year = 2020,
    administrative_level = 1,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    epi_version = "gbd2019",
    calc_causes = "GBD only"
  )

  # Add metadata
  hia$version <- current_version
  hia$epi_version <- "gbd2019"
  hia$calc_causes <- "GBD only"
  hia$pop_year <- 2020

  return(hia)
}


get_fingerprints <- function(){

  tibble(filepath=list.files("tests/data/versions", full.names = TRUE)) %>%
    mutate(
      # version already in hia
      # version = str_remove(basename(filepath), "_hia_bgd.csv"),
      # case after _hia_
      case = str_extract(filepath, "(?<=_hia_).+(?=\\.csv)")
    ) %>%
    rowwise() %>%
    mutate(hia = list(read_csv(filepath))) %>%
    select(-filepath) %>%
    unnest(hia)
}

test_that("Estimates are compatible with previous versions", {



  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)
  library(glue)
  library(tidyr)


  # Get fingerprint(s)
  hia <- get_fingerprint_bgd()

  # Save it
  current_version <- as.character(packageVersion("creahia"))
  filepath <- glue("tests/data/versions/{current_version}_hia_bgd.csv")
  dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
  write.csv(hia, filepath, row.names = FALSE)

  # Compare fingerprints
  fingerprints <- get_fingerprints()


  # Test that all central values are equal
  different_central <- fingerprints %>%
    filter(estimate=="central") %>%
    group_by(case, scenario, region_id, Pollutant, Outcome, Cause, AgeGrp, epi_version, calc_causes, pop_year) %>%
    summarise(number = n(), unique = n_distinct(number)) %>%
    ungroup() %>%
    filter(number < max(number) | unique > 1) %>%
    distinct(Cause, Outcome)

  # Test that it is empty
  testthat::expect_equal(nrow(different_central), 0, info = glue("Different central values: {different_central$Cause} - {different_central$Outcome}"))

})
