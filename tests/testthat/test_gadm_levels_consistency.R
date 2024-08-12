testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())




test_that("GADM level 2 gives similar results to level 1 and level 0 - Bangladesh", {


  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  res <- creaexposure::RES_30_SEC
  bbox <- creaexposure::get_bbox("BD")
  pop <- creaexposure::data.pop(res=res, bbox=bbox)
  pm25 <- creaexposure::data.basemap_pm25(pop=pop, res=res, year=2019)

  baseline_rasters <- list(pm25=pm25 %>% `values<-`(0),
                           no2=pm25 %>% `values<-`(0))
  perturbation_rasters <- list(pm25=pm25,
                               no2=pm25 %>% `values<-`(0))


  levels <- c(2,3)
  hias <- lapply(levels, function(level){
      creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = perturbation_rasters,
      baseline_rasters = baseline_rasters,
      administrative_iso3s = c("BGD"),
      administrative_level = level,
      administrative_res = "full",
      crfs_version = "C40",
      epi_version = "gbd2019", # Subnational only available with this epi for now
      valuation_version = "viscusi"
    ) %>%
        mutate(gadm_level=level)
  })


  # Test that values are roughly equal
  comparison <- bind_rows(hias) %>%
    filter(!double_counted, estimate=='central') %>%
    group_by(estimate, gadm_level, iso3, Outcome) %>%
    dplyr::summarise(number=sum(number, na.rm=T)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from='gadm_level', values_from='number', names_prefix = 'gadm_')

  testthat::expect_equal(comparison$gadm_2, comparison$gadm_3, tolerance=1e-2)
  testthat::expect_true(sum(comparison$gadm_3) > 0)

})


test_that("Population is properly calculated", {

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  res <- creaexposure::RES_30_SEC
  bbox <- creaexposure::get_bbox("BD")
  pop <- creaexposure::data.pop(res=res, bbox=bbox)
  pm25 <- creaexposure::data.basemap_pm25(pop=pop, res=res, year=2019)

  baseline_rasters <- list(pm25=pm25 %>% `values<-`(0),
                           no2=pm25 %>% `values<-`(0))
  perturbation_rasters <- list(pm25=pm25,
                               no2=pm25 %>% `values<-`(0))


  # 03: Extract concentrations ----
  # conc_regions <- creahia::extract_concs_and_pop(concs, regions, pollutants_for_hia)



  hia <- creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = perturbation_rasters,
      baseline_rasters = baseline_rasters,
      administrative_iso3s = c("BGD"),
      administrative_level = 1,
      administrative_res = "full",
      crfs_version = "C40",
      epi_version = "gbd2019",
      valuation_version = "viscusi")


  pop_hia <- sum(hia %>% select(region_id, pop) %>% distinct(region_id, .keep_all = T) %>% pull(pop))


  gadm <- creahelpers::get_adm(level=0, res="full", iso2s="BD")
  creahia::get_pop(gadm) %>% sum() -> pop_gadm_0

  # Sum population from pop raster
  pop_qty <- sum(terra::app(terra::cellSize(pop) / 1e6* pop, sum))
  terra::extract(pop_qty, terra::vect(gadm), sum, na.rm=T)



  testthat::expect_equal(comparison$gadm_2, comparison$gadm_3, tolerance=1e-2)
  testthat::expect_true(sum(comparison$gadm_3) > 0)

})
