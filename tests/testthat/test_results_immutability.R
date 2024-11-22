# For certain versions, we want overall numbers to remain the same
# This really depends on the modifications brought up


get_fingerprint_bgd <- function(){

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

  hia <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p1),
    baseline_rasters = list(pm25 = m),
    pop_year = 2020,
    administrative_level = 0,
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
      case = str_extract(filepath, "(?<=_hia_).+(?=\\.csv)"),
      hia = list(read_csv(filepath))) %>%
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


  # Get fingerprint(s)
  hia <- get_fingerprint_bgd()

  # Save it
  current_version <- as.character(packageVersion("creahia"))
  filepath <- glue("tests/data/versions/{current_version}_hia_bgd.csv")
  dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
  write.csv(hia, filepath, row.names = FALSE)



  # Compare fingerprints
  fingerprints <- get_fingerprints()


})
