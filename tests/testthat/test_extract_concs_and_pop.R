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


test_that("Regions extraction works and doesn't have any double counting", {


  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  res <- 0.01
  pm25 <- terra::rast(
    xmin=88,
    xmax=92,
    ymin=20,
    ymax=27,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  pm25[] <- runif(prod(dim(pm25)), min, max)

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  # perturbation_rast <- target-pm25

  # res <- creaexposure::RES_30_SEC
  # bbox <- creaexposure::get_bbox("BD")
  # pop <- creaexposure::data.pop(res=res, bbox=bbox)
  # pm25 <- creaexposure::data.basemap_pm25(pop=pop, res=res, year=2019)
  pop_year <- 2020

  # Build required data ---------------------------------------------------------------------
  baseline_rasters <- list(pm25=pm25)
  perturbation_rasters <- list(pm25=-pm25)
  perturbation_rasters <- perturbation_rasters %>%
    creahelpers::to_raster() %>%
    creahelpers::unrasterstack()

  baseline_rasters <- baseline_rasters %>%
    creahelpers::to_raster() %>%
    creahelpers::unrasterstack()

  species <- names(perturbation_rasters)
  grid_raster <- perturbation_rasters[[1]] %>% raster::raster()

  conc_perturbation <- tibble(species = "pm25",
                              conc_perturbation = perturbation_rasters,
                              scenario = "test")

  conc_baseline <- tibble(species = names(baseline_rasters),
                          conc_baseline = raster::as.list(raster::stack(baseline_rasters)))


  # 03: Combine and flatten: one row per scenario --------------------------------------------
  concs <- creahia::combine_concs(conc_perturbation, conc_baseline) %>% # combine table
    creahia::flatten_concs() %>% # long to wide
    creahia::add_pop(grid_raster, year_desired=pop_year)


  regions_0 <- creahia::get_model_adm(grid_raster,
                                      admin_level = 0,
                                      res = "low",
                                      iso3s = "BGD")

  regions_1 <- creahia::get_model_adm(grid_raster,
                                      admin_level = 1,
                                      res = "low",
                                      iso3s = "BGD")

  # Extract concentrations and population
  conc_regions_0 <- creahia::extract_concs_and_pop(concs, regions_0, species)
  conc_regions_1 <- creahia::extract_concs_and_pop(concs, regions_1, species)

  # Check that population is similar
  bind_rows(conc_regions_0$test) %>% bind_rows() %>% pull(pop) %>% sum() -> pop_hia_0
  bind_rows(conc_regions_1$test) %>% bind_rows() %>% pull(pop) %>% sum() -> pop_hia_1
  testthat::expect_equal(pop_hia_0, pop_hia_1, tolerance=1e-2)

  # Check that population weighted average is similar
  bind_rows(conc_regions_0$test) %>% bind_rows() %>% summarise(conc=weighted.mean(conc_baseline_pm25, pop, na.rm=T)) %>% pull() -> conc_hia_0
  bind_rows(conc_regions_1$test) %>% bind_rows() %>% summarise(conc=weighted.mean(conc_baseline_pm25, pop, na.rm=T)) %>% pull() -> conc_hia_1
  testthat::expect_equal(conc_hia_0, conc_hia_1, tolerance=1e-2)

})
