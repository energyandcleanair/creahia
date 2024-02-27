testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("GBD2019 and GBD2017 have comparable data - input", {

  copy_epi_data_update()
  pop <- get_epi_pop()
  pop.total <- get_pop_total(pop)


  crude2017 <- get_death_crude(version='gbd2017')
  crude2019 <- get_death_crude(version='gbd2019')

  child2017 <- get_death_child_lri(pop.total=pop.total, version='gbd2017')
  child2019 <- get_death_child_lri(pop.total=pop.total, version='gbd2019')

  yld2017 <- get_yld(pop.total, version='gbd2017')
  yld2019 <- get_yld(pop.total, version='gbd2019')

  totcp2017 <- get_death_totcp(yld=yld2017, pop.total=pop.total, version='gbd2017')
  totcp2019 <- get_death_totcp(yld=yld2019, pop.total=pop.total, version='gbd2019')

  asthma2017 <- get_asthma_prev(pop.total, version='gbd2017')
  asthma2019 <- get_asthma_prev(pop.total, version='gbd2019')

  ihme2017 <- get_ihme(version = "gbd2017")
  ihme2019 <- get_ihme(version = "gbd2017")


  compare <- function(df2017, df2019, tolerance=0.1){

    options(dplyr.summarise.inform = FALSE)
    allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                               "PLW", "KNA", "SMR", "TKL", "TUV")

    # Fill metric_name and/or measure_name if missing
    fill_cols <- function(df){
      if(!"metric_name" %in% names(df)) df$metric_name <- df$var
      if(!"measure_name" %in% names(df)) df$measure_name <- df$estimate
      if(!"var" %in% names(df)) df$var <- df$cause_name
      df
    }

    comparison <- bind_rows(
      df2017 %>% mutate(version='gbd2017'),
      df2019 %>% mutate(version='gbd2019')
    ) %>%
      fill_cols() %>%
      select(measure_name, location_id, iso3, location_level, metric_name, estimate, var, val, version) %>%
      spread(version, val) %>%
      mutate(rel_diff=(gbd2019/gbd2017)-1)

    missing_iso3s <- comparison %>%
      filter(location_level==3,
             is.na(gbd2017 + gbd2019)) %>%
      pull(iso3) %>%
      unique()

    testthat::expect_in(missing_iso3s, allowed_missing_iso3s)

    # Test that numbers are relatively close but not identical
    # across two versions, for selected countries
    should_be_close_iso3s <- c('USA', 'CHN', 'IND', 'PHL', 'IDN', 'GBR', 'DEU', 'JPN')
    idx_iso3 <- comparison$iso3 %in% should_be_close_iso3s & (comparison$location_level==3)
    testthat::expect_equal(comparison$gbd2017[idx_iso3], comparison$gbd2019[idx_iso3], tolerance=tolerance)
    testthat::expect_true(all(comparison$gbd2017[idx_iso3] != comparison$gbd2019[idx_iso3]))


    # Check years
    testthat::expect_true(all(df2017$year == 2017))
    testthat::expect_true(all(df2019$year == 2019))

    # Test that it has subnational data
    comparison %>%
      filter(location_level==4,
             !is.na(gbd2017 + gbd2019)) %>%
      nrow() %>%
      testthat::expect_gt(100)

    # Test no duplicate
    bind_rows(
      df2017 %>% mutate(version='gbd2017'),
      df2019 %>% mutate(version='gbd2019')
    ) %>%
      fill_cols() %>%
      group_by(measure_name, location_id, iso3, location_level, metric_name, estimate, var, version) %>%
      dplyr::summarise(n=n()) %>%
      pull(n) %>%
      max() %>%
      testthat::expect_equal(1)
  }

  compare(crude2017, crude2019, tolerance=0.2)
  compare(child2017, child2019, tolerance=0.3)
  compare(yld2017, yld2019, tolerance=0.3)
  compare(totcp2017, totcp2019, tolerance=0.2)
  compare(asthma2017, asthma2019, tolerance=0.4)


})

test_that("GBD2019 and GBD2017 have comparable data - output", {

  epi2017 <- get_epi('gbd2017')
  epi2019 <- get_epi('gbd2019')

  comparison <- bind_rows(
    epi2017 %>% mutate(version='gbd2017'),
    epi2019 %>% mutate(version='gbd2019')
  ) %>%
    select(-c(location_name)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, country, region, income_group, version)) %>%
    tidyr::spread(version, value)

  missing_iso3s <- comparison %>%
    filter(location_level==3,
           is.na(gbd2017 + gbd2019)) %>%
    pull(iso3) %>%
    unique()

  allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                             "PLW", "KNA", "SMR", "TKL", "TUV")
  testthat::expect_in(missing_iso3s, allowed_missing_iso3s)


})

test_that("GBD2019 and GBD2017 have comparable data - IHME", {

  ihme2017 <- get_ihme('gbd2017')
  ihme2019 <- get_ihme('gbd2019')

  comparison <- bind_rows(
    ihme2017 %>% mutate(version='gbd2017'),
    ihme2019 %>% mutate(version='gbd2019')
  ) %>%
    filter(location_level == 3) %>%
    select(measure_name, location_id, iso3, location_level, age, cause_short, cause_name, sex_name, estimate, val, version) %>%
    spread(version, val)

  missing_iso3s <- comparison %>%
    filter(location_level==3,
           is.na(gbd2017 + gbd2019)) %>%
    pull(iso3) %>%
    unique()

  allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                             "PLW", "KNA", "SMR", "TKL", "TUV")
  testthat::expect_in(missing_iso3s, allowed_missing_iso3s)
})

test_that("GBD2019 and GBD2017 yield roughly similar results - Philipines", {


  library(tidyverse)
  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)

  obs_map <- terra::rast(get_test_file('example_ph_who/obs_map.tif'))
  baseline_map <- terra::rast(get_test_file('example_ph_who/zero_map.tif'))
  perturbation_map <- obs_map - baseline_map
  names(perturbation_map) <- names(obs_map)

  hia <- lapply(c('gbd2017', 'gbd2019'), function(epi_version){
    creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = raster::stack(perturbation_map),
      baseline_rasters = raster::stack(baseline_map),
      administrative_iso3s = c("PHL"),
      administrative_level = 0,
      crfs_version = "C40",
      epi_version = epi_version,
      ihme_version = "gbd2019" # Use for comparable population
    ) %>%
      mutate(epi_version = epi_version)
  }) %>%
    bind_rows()


  comparison <- hia %>%
    filter(!double_counted,
           estimate=='central') %>%
    group_by(Outcome, epi_version) %>%
    summarise(value=sum(number, na.rm=T))


  # Test that deaths close to 65000
  comparison_outcome <- comparison %>%
    tidyr::spread(Outcome, value)
  comparison_outcome$Death_target <- 65000
  testthat::expect_equal(comparison_outcome$Deaths, comparison_outcome$Death_target , tolerance = 1e-1)

  comparison_epi <- comparison %>%
    tidyr::spread(epi_version, value)

  testthat::expect_true(sum(comparison_epi$gbd2017) > 0)
  # Values should be equal on non GBD
  non_gbd <- c("exac", "PTB", "LBW", "Absences")
  idx_non_gbd <- comparison_epi$Outcome %in% non_gbd
  testthat::expect_identical(comparison_epi$gbd2017[idx_non_gbd], comparison_epi$gbd2019[idx_non_gbd])

  # Not equal but close for GBD outcomes (except Deaths.child which has similar epi data apparently, at least in PHP)
  idx_gbd <- !idx_non_gbd
  testthat::expect_equal(comparison_epi$gbd2017[idx_gbd], comparison_epi$gbd2019[idx_gbd], tolerance=1e-1)
  testthat::expect_true(all(comparison_epi$gbd2017[idx_gbd]!=comparison_epi$gbd2019[idx_gbd]))

})
