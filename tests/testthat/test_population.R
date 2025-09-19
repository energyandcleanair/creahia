
test_that("Population is properly calculated", {
  
  # Skip test if GIS data is not available
  gis_dir <- Sys.getenv("GIS_DIR", "")
  if(gis_dir == "" || !dir.exists(file.path(gis_dir, "population"))) {
    skip("GIS data not available - population files not found")
  }

  # Create a grid raster around Bangladesh
  iso2 <- "BD"

  # Data validation from UN, which is used by GPW, though the numbers below are from the 2024 version
  # whilst GPW seems to use UNWPP 2015
  # https://population.un.org/dataportal/data/indicators/49/locations/608,356/start/1990/end/2024/table/pivotbylocation?df=8779997f-7e7e-46b1-a155-28fa58ffa20d
  pop_validated <- list(
    "BD" = list(
      `2015` = 159e6,
      `2020` = 166e6
    ),
    "PH" = list(
      `2015` = 105.3e6,
      `2020` = 112.1e6
    ),
    "IN" = list(
      `2015` = 132.8e6,
      `2020` = 140.3e6
    ),
    "VN" = list(
      `2015` = 92.8e6,
      `2020` = 98.1e6
    )
  )

  # Use creahelpers::get_adm now that we have GIS data in CI
  adm <- creahelpers::get_adm(level = 0, res = "low", iso2s = iso2)
  bbox <- sf::st_bbox(adm)

  # Create a grid to project the population
  grid <- terra::rast(terra::ext(bbox), 1000, 1000)

  # Get population raster in 2015
  pop2015 <- get_pop_count(grid, year_desired=2015)
  pop2020 <- get_pop_count(grid, year_desired=2020)

  # Ensure they all have same dimension / areas
  expect_equal(terra::ext(grid), terra::ext(pop2015))
  expect_equal(terra::ext(grid), terra::ext(pop2020))

  # Check that the population in 2015 is less than the population in 2020
  attr(pop2015, 'year') -> year2015
  attr(pop2020, 'year') -> year2020

  expect_equal(year2015, 2015) #If fails: The year attribute should be 2015. Maybe you're missing the population file?
  expect_equal(year2020, 2020) #If fails: The year attribute should be 2020. Maybe you're missing the population file?


  # Extract population values
  pop2015_extracted <- terra::extract(pop2015, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]
  pop2020_extracted <- terra::extract(pop2020, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]


  # Compare with validated data
  # It is not very close at the moment... something is still weird
  # It is not the reprojection, as the unprojected version is also off (see below)
  # Could be the area calculation? Or the boundaries
  expect_equal(pop2015_extracted, pop_validated[[iso2]]$`2015`, tolerance=0.05)
  expect_equal(pop2020_extracted, pop_validated[[iso2]]$`2020`, tolerance=0.05)


  # Try the unprojected version (much slower)
  # pop2015_unproj <- get_pop_count(grid=NULL, year_desired=2015)
  # pop2020_unproj <- get_pop_count(grid=NULL, year_desired=2020)
  #
  # pop2015_unproj_extracted <- terra::extract(pop2015_unproj, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]
  # pop2020_unproj_extracted <- terra::extract(pop2020_unproj, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]

})


get_random_exposure_hia <- function(levels,
                                    min=20,
                                    max=60,
                                    target=0,
                                    pop_year=2020,
                                    calc_causes="GEMM and GBD",
                                    iso3="BGD"){

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  res <- 0.01
  iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')
  # Use creahelpers::get_adm now that we have GIS data in CI
  adm <- creahelpers::get_adm(level = 0, res = "low", iso2s = iso2)
  bbox <- sf::st_bbox(adm)
  baseline_rast <- terra::rast(
    xmin=bbox$xmin,
    xmax=bbox$xmax,
    ymin=bbox$ymin,
    ymax=bbox$ymax,
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
      pop_year=pop_year,
      administrative_level = level,
      administrative_res = "low",
      administrative_iso3s = iso3,
      epi_version = "gbd2019",
      calc_causes = calc_causes
    ) %>%
      mutate(level=level)
  }) %>%
    bind_rows()
}


test_that("Population is properly calculated and scaled- using HIA", {

  library(terra)
  library(dplyr)
  library(creahia)
  library(creaexposure)
  iso3 <- "ZAF"

  hia_2015 <- get_random_exposure_hia(levels=c(0,1,2),
                                      pop_year=2015,
                                      iso3=iso3
  ) %>%
    mutate(year=2015)


  hia_2019 <- get_random_exposure_hia(levels=c(0,1,2),
                                      pop_year=2019,
                                      iso3=iso3) %>%
    mutate(year=2019)


  hias <- bind_rows(hia_2015, hia_2019)

  pop_hia <- hias %>%
    distinct(iso3, year, level, pop) %>%
    group_by(iso3, year, level) %>%
    summarise(pop_hia=sum(pop))

  pop_wb <- wbstats::wb_data(indicator="SP.POP.TOTL",
                             country=iso3,
                             start_date=min(hias$year),
                             end_date=max(hias$year)) %>%
    select(iso3=iso3c,
           year=date,
           pop_wb=`SP.POP.TOTL`)

  comparison <- pop_hia %>%
    left_join(pop_wb, by=c("iso3", "year")) %>%
    mutate(pop_diff=(pop_hia-pop_wb)/pop_wb)

  expect_equal(max(abs(comparison$pop_diff)), 0, tolerance=0.1)

})
