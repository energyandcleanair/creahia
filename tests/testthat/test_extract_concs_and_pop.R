

test_that("Regions extraction works and doesn't have any double counting", {


  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  res <- creaexposure::RES_30_SEC
  bbox <- creaexposure::get_bbox("BD")
  pop <- creaexposure::data.pop(res=res, bbox=bbox)
  pm25 <- creaexposure::data.basemap_pm25(pop=pop, res=res, year=2019)
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

  # Compute hia for boths
  hia_0 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = perturbation_rasters,
    baseline_rasters = baseline_rasters,
    administrative_iso3s = c("BGD"),
    administrative_level = 0,
    administrative_res = "low",
    pop_year=2020)

  hia_1 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = perturbation_rasters,
    baseline_rasters = baseline_rasters,
    administrative_iso3s = c("BGD"),
    administrative_level = 1,
    administrative_res = "low",
    pop_year=2020
    )

  # Check deaths are similar
  hia_0 %>% filter(estimate=="central", Outcome=="Deaths", !double_counted) %>% pull(number) %>% sum() -> deaths_0
  hia_1 %>% filter(estimate=="central", Outcome=="Deaths", !double_counted) %>% pull(number) %>% sum() -> deaths_1
  testthat::expect_equal(deaths_0, deaths_1, tolerance=1e-2)

})
