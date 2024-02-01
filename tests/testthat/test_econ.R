testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("Test GDP scaling for several countries", {


  hia_cost <- readRDS(get_test_file(file.path("example_kaz", "hia_cost.RDS")))



  econ_unscaled <- creahia::get_econ_forecast(hia_cost,
                                        years=seq(2010, 2020),
                                        pop_targetyr=2019,
                                        GDP_scaling=F)

  econ_scaled <- creahia::get_econ_forecast(hia_cost,
                                            years=seq(2010, 2020),
                                            pop_targetyr=2019,
                                            GDP_scaling=T)


  comparison <- bind_rows(
    econ_unscaled %>%
      group_by(iso3, year, type='unscaled') %>%
      summarise(value=sum(cost_mn_currentUSD, na.rm=T)),

    econ_scaled %>%
      group_by(iso3, year, type='scaled') %>%
      summarise(value=sum(cost_mn_currentUSD, na.rm=T))) %>%
    bind_rows() %>%
    tidyr::spread(type, value)


  testthat::expect_true(sum(comparison$scaled) > 0)
  # Values should be equal on reference year
  testthat::expect_equal(comparison$scaled[comparison$year==2019], comparison$unscaled[comparison$year==2019], tolerance=1e-10)
  # Not equal but close on other years
  testthat::expect_equal(comparison$scaled, comparison$unscaled, tolerance=1e-1)
  testthat::expect_true(any(comparison$scaled!=comparison$unscaled))

})
