skip("Only need to be run when creating new epi.")

testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("GBD2021, GBD2019 and GBD2017 have comparable data - input", {

  copy_epi_data_update()
  pop <- get_epi_pop()
  pop.total <- get_pop_total(pop)


  crude2017 <- get_death_crude(version='gbd2017')
  crude2019 <- get_death_crude(version='gbd2019')
  crude2021 <- get_death_crude(version='gbd2021')

  child2017 <- get_death_child_lri(pop.total=pop.total, version='gbd2017')
  child2019 <- get_death_child_lri(pop.total=pop.total, version='gbd2019')
  child2021 <- get_death_child_lri(pop.total=pop.total, version='gbd2021')

  yld2017 <- get_yld(pop.total, version='gbd2017')
  yld2019 <- get_yld(pop.total, version='gbd2019')
  yld2021 <- get_yld(pop.total, version='gbd2021')

  # totcp2017 <- get_death_totcp(yld=yld2017, pop.total=pop.total, version='gbd2017')
  # totcp2019 <- get_death_totcp(yld=yld2019, pop.total=pop.total, version='gbd2019')
  # totcp2021 <- get_death_totcp(yld=yld2021, pop.total=pop.total, version='gbd2021')

  asthma2017 <- get_asthma_prev(pop.total, version='gbd2017')
  asthma2019 <- get_asthma_prev(pop.total, version='gbd2019')
  asthma2021 <- get_asthma_prev(pop.total, version='gbd2021')

  ihme2017 <- get_ihme(version = "gbd2017")
  ihme2019 <- get_ihme(version = "gbd2017")
  ihme2021 <- get_ihme(version = "gbd2021")


  compare <- function(df2017, df2019, df2021, tolerance=0.1){

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
      df2019 %>% mutate(version='gbd2019'),
      df2021 %>% mutate(version='gbd2021')
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
    testthat::expect_true(all(df2021$year == 2019)) # Actually there is no year differentiator in the code so all years should be equal?

    # Test that it has subnational data
    comparison %>%
      filter(location_level==4,
             !is.na(gbd2017 + gbd2019 + gbd2021)) %>%
      nrow() %>%
      testthat::expect_gt(100)

    # Test no duplicate
    bind_rows(
      df2017 %>% mutate(version='gbd2017'),
      df2019 %>% mutate(version='gbd2019'),
      df2021 %>% mutate(version='gbd2021'),
    ) %>%
      fill_cols() %>%
      group_by(measure_name, location_id, iso3, location_level, metric_name, estimate, var, version) %>%
      dplyr::summarise(n=n()) %>%
      pull(n) %>%
      max() %>%
      testthat::expect_equal(1)
  }

  compare(crude2017, crude2019, crude2021, tolerance=0.2)
  compare(child2017, child2019, child2021, tolerance=0.3)
  compare(yld2017, yld2019, yld2021, tolerance=0.3)
  compare(asthma2017, asthma2019, asthma2021, tolerance=0.4)


})

test_that("GBD2021, GBD2019 and GBD2017 have comparable data - output", {

  epi2017 <- get_epi('gbd2017')
  epi2019 <- get_epi('gbd2019')
  epi2021 <- get_epi('gbd2021')

  comparison <- bind_rows(
    epi2017 %>% mutate(version='gbd2017'),
    epi2019 %>% mutate(version='gbd2019'),
    epi2021 %>% mutate(version='gbd2021')
  ) %>%
    select(-c(location_name)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, country, region, income_group, version)) %>%
    tidyr::spread(version, value)

  missing_iso3s <- comparison %>%
    filter(location_level==3,
           is.na(gbd2017 + gbd2019 + gbd2021)) %>%
    pull(iso3) %>%
    unique()

  allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                             "PLW", "KNA", "SMR", "TKL", "TUV")
  testthat::expect_in(missing_iso3s, allowed_missing_iso3s)


  # Plot comparison
  # bind_rows(
  #   epi2017 %>% mutate(version='gbd2017'),
  #   epi2019 %>% mutate(version='gbd2019'),
  #   epi2021 %>% mutate(version='gbd2021')
  # ) %>%
  #   select(-c(location_name)) %>%
  #   tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, country, region, income_group, version)) %>%
  #   filter(iso3=="ZAF", location_level==3) %>%
  #   ggplot() + geom_col(aes(version, value, fill=version), position='dodge') + facet_wrap(~name, scales='free_y')

})

test_that("GBD2021, GBD2019 and GBD2017 have comparable data - IHME", {

  ihme2017 <- get_ihme('gbd2017')
  ihme2019 <- get_ihme('gbd2019')
  ihme2021 <- get_ihme('gbd2021')

  comparison <- bind_rows(
    ihme2017 %>% mutate(version='gbd2017'),
    ihme2019 %>% mutate(version='gbd2019'),
    ihme2021 %>% mutate(version='gbd2021')
  ) %>%
    filter(location_level == 3) %>%
    select(measure_name, location_id, iso3, location_level, age, cause, sex_name, estimate, val, version) %>%
    spread(version, val)

  missing_iso3s <- comparison %>%
    filter(location_level==3,
           # COPD for low age NA in new versions
           ! age %in% c("5-9", "10-14", "<5") | cause != "COPD",
           is.na(gbd2017 + gbd2019 + gbd2021)) %>%
    pull(iso3) %>%
    unique()

  allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                             "PLW", "KNA", "SMR", "TKL", "TUV")
  testthat::expect_in(missing_iso3s, allowed_missing_iso3s)
})

test_that("GBD2021, GBD2019 and GBD2017 yield roughly similar results - Philipines", {

  library(tidyverse)
  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)

  obs_map <- terra::rast(get_test_file('example_ph_who/obs_map.tif'))
  baseline_map <- terra::rast(get_test_file('example_ph_who/zero_map.tif'))
  perturbation_map <- obs_map - baseline_map
  names(perturbation_map) <- names(obs_map)

  hia <- lapply(c(
    # 'gbd2017',
    'gbd2019',
    'gbd2021'), function(epi_version){
    creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = raster::stack(perturbation_map),
      baseline_rasters = raster::stack(baseline_map),
      administrative_iso3s = c("PHL"),
      administrative_level = 0,
      crfs_version = "C40",
      epi_version = epi_version,
      rr_sources = RR_GBD2021
    ) %>%
      mutate(epi_version = epi_version)
  }) %>%
    bind_rows()


  unique(hia$Cause)

  comparison <- hia %>%
    filter(!double_counted,
           estimate=='central') %>%
    group_by(Outcome, double_counted, epi_version) %>%
    dplyr::summarise(value=sum(number, na.rm=T))


  # Test that deaths close to 65000
  comparison_outcome <- comparison %>%
    filter(!double_counted) %>%
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
  testthat::expect_equal(comparison_epi$gbd2017[idx_gbd], comparison_epi$gbd2021[idx_gbd], tolerance=1e-1)
  testthat::expect_true(all(comparison_epi$gbd2017[idx_gbd]!=comparison_epi$gbd2019[idx_gbd]))
  testthat::expect_true(all(comparison_epi$gbd2017[idx_gbd]!=comparison_epi$gbd2021[idx_gbd]))

})


test_that("New epi doesn't differ too much from old one", {


  version <- "gbd2019"

  new <- read_csv(glue("inst/extdata/epi_for_hia_{version}.csv"))
  old <- read_csv(glue("https://raw.githubusercontent.com/energyandcleanair/creahia/7966ab8f14bda553e4cfbb1eb59dd872d3217c84/inst/extdata/epi_for_hia_{version}.csv"))

  # It should not have changed central
  comparison <- bind_rows(
    new %>% mutate(source="new"),
    old %>% mutate(source="old")
  ) %>%
    filter(!is.na(location_id)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, location_name, country, region, income_group, pop, source),
                        names_to="var", values_to="value") %>%
    select(location_id, estimate, source, var, value) %>%
  spread(source, value) %>%
    mutate(diff_rel=(new/old)-1)

  # Should be perfectly matching
  # i.d. no NA in new or old
  missing <- comparison %>%
    filter(is.na(new) != is.na(old)) %>%
    nrow()

  testthat::expect_equal(missing, 0)


  different_central <- comparison %>%
    filter(estimate=='central') %>%
    filter(abs(diff_rel) > 0.001) %>%
    nrow()

  testthat::expect_equal(different_central, 0)


  # No low > central or high < central
  inversed <- new %>%
    filter(!is.na(location_id)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, location_name, country, region, income_group, pop),
                        names_to="var", values_to="value") %>%
    select(location_id, estimate, var, value) %>%
    spread(estimate, value) %>%
    filter(low > central | high < central | low > high) %>%
    nrow()

  testthat::expect_equal(inversed, 0)
})

