skip("Only need to be run when creating new epi.")

testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("GADM level 2 gives similar results to level 1 and level 0 - Indonesia", {

  # Sourc test helper functions
  try(source("tests/helpers.R"), silent=T)
  try(source("../helpers.R"), silent=T)

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)

  # print(get_examples_dir())
  print(creahia::get_hia_path("GEMM Calculator (PNAS)_ab.xlsx"))
  print(find.package("creahia"))

  pm25_perturbation <- terra::rast(get_test_file('example_meteosim/data/CCTM_d02_2019010112_GR1.nc_PM25_UGM3.tif')) %>%
    terra::app(sum) * 1e16
  no2_perturbation <- terra::rast(get_test_file('example_meteosim/data/CCTM_d02_2019010112_GR1.nc_NO2_UGM3.tif')) %>%
    terra::app(sum) * 1e16

  baseline_rasters <- list(pm25=pm25_perturbation %>% `values<-`(0),
                           no2=no2_perturbation %>% `values<-`(0))
  perturbation_rasters <- list(pm25=pm25_perturbation,
                               no2=no2_perturbation)


  hia_gadm0 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = perturbation_rasters,
    baseline_rasters = baseline_rasters,
    administrative_iso3s = c("IDN"),
    administrative_level = 0,
    administrative_res = "low",
    crfs_version = "C40",
    epi_version = "gbd2019", # Subnational only available with this epi for now
    valuation_version = "viscusi"
  )

  hia_gadm1 <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = perturbation_rasters,
    baseline_rasters = baseline_rasters,
    administrative_iso3s = c("IDN"),
    administrative_level = 1,
    administrative_res = "low",
    crfs_version = "C40",
    epi_version = "gbd2019", # Subnational only available with this epi for now
    valuation_version = "viscusi"
  )


  # Test that values are roughly equal
  comparison <- bind_rows(
    hia_gadm0 %>% mutate(gadm_level=0),
    hia_gadm1 %>% mutate(gadm_level=1)
  ) %>%
    filter(!double_counted, estimate=='central') %>%
    group_by(estimate, gadm_level, iso3, Outcome) %>%
    dplyr::summarise(number=sum(number, na.rm=T)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from='gadm_level', values_from='number', names_prefix = 'gadm_')

  testthat::expect_equal(comparison$gadm_0, comparison$gadm_1, tolerance=1e-2)
  testthat::expect_true(sum(comparison$gadm_0) > 0)

})
