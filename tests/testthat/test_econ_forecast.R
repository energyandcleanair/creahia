testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())

# Test the economic forecasting system

test_that("Test population scaling only - fatal vs non-fatal outcomes", {

  # Create test HIA cost data with both fatal and non-fatal outcomes
  test_hia_cost <- data.frame(
    iso3 = c("USA", "USA"),
    region_id = c("USA", "USA"),
    Outcome = c("Deaths", "Asthma.Prev"),
    year = c(2019, 2019),
    number = c(100, 1000),
    cost_mn_currentUSD = c(1.1, 0.1),
    cost_mn_currentLCU = c(1.1, 0.1),
    AgeGrp = c("25+", "25+"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test forecast with ONLY population scaling (no GDP scaling)
  testthat::expect_no_error({
    forecast_pop_only <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = c(2020, 2023),
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Test structure
  testthat::expect_true(is.data.frame(forecast_pop_only))
  testthat::expect_true(nrow(forecast_pop_only) > 0)

  # Test required columns exist
  required_cols <- c("iso3", "Outcome", "year", "pop_scaling", "gdp_scaling", "cost_mn_currentUSD")
  testthat::expect_true(all(required_cols %in% names(forecast_pop_only)))

  # Test that population scaling is reasonable
  testthat::expect_true(all(forecast_pop_only$pop_scaling >= 0.5 & forecast_pop_only$pop_scaling <= 2.0, na.rm = TRUE))

  # Test that GDP scaling is 1 (no GDP scaling applied)
  testthat::expect_true(all(forecast_pop_only$gdp_scaling == 1, na.rm = TRUE))

  # Test that costs are scaled by population only
  # For same country, same year, fatal vs non-fatal outcomes should have DIFFERENT pop_scaling
  # (fatal uses death rates, non-fatal uses population rates)
  usa_2020 <- forecast_pop_only %>% filter(iso3 == "USA", year == 2020)
  # Should have different scaling for fatal vs non-fatal outcomes
  fatal_scaling <- usa_2020 %>% filter(fatal == TRUE) %>% pull(pop_scaling) %>% unique()
  non_fatal_scaling <- usa_2020 %>% filter(fatal == FALSE) %>% pull(pop_scaling) %>% unique()
  testthat::expect_true(length(fatal_scaling) == 1 && length(non_fatal_scaling) == 1,
                       info = "Should have consistent scaling within fatal/non-fatal groups")
  testthat::expect_true(fatal_scaling != non_fatal_scaling,
                       info = "Fatal and non-fatal outcomes should have different population scaling")

})

test_that("Test GDP scaling and discounting only", {

  # Create test HIA cost data
  test_hia_cost <- data.frame(
    iso3 = c("USA", "ZAF"),
    region_id = c("USA", "ZAF"),
    Outcome = c("Deaths", "Deaths"),
    year = c(2019, 2019),
    number = c(100, 50),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 9.3),
    AgeGrp = c("25+", "25+"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test forecast with GDP scaling
  testthat::expect_no_error({
    forecast_gdp_scaled <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = c(2020, 2023),
      reference_year = 2019,
      use_gdp_scaling = TRUE,
      discount_rate = 0.03
    )
  })

  # Test structure
  testthat::expect_true(is.data.frame(forecast_gdp_scaled))
  testthat::expect_true(nrow(forecast_gdp_scaled) > 0)

  # Test that GDP scaling is positive
  testthat::expect_true(all(forecast_gdp_scaled$gdp_scaling > 0, na.rm = TRUE))

  # Test that costs are scaled by both population and GDP
  testthat::expect_true("cost_mn_currentUSD" %in% names(forecast_gdp_scaled))

  # Test that reference year (2019) has GDP scaling = 1
  ref_year_data <- forecast_gdp_scaled %>% filter(year == 2019)
  testthat::expect_true(nrow(ref_year_data) > 0, info = "Should have reference year data")
  testthat::expect_true(all(abs(ref_year_data$gdp_scaling - 1) < 0.01),
                       info = "Reference year should have GDP scaling close to 1")

  # Test that GDP scaling changes over time (due to GDP growth and discounting)
  usa_data <- forecast_gdp_scaled %>% filter(iso3 == "USA") %>% arrange(year)
  testthat::expect_true(nrow(usa_data) > 1, info = "Should have multiple years of USA data")
  # GDP scaling should be different from 1 for future years
  future_years <- usa_data %>% filter(year > 2019)
  testthat::expect_true(any(abs(future_years$gdp_scaling - 1) > 0.01),
                       info = "Future years should have GDP scaling different from 1")

  # Test that different countries have different GDP scaling factors
  testthat::expect_true(nrow(forecast_gdp_scaled) > 1, info = "Should have multiple rows of data")
  unique_gdp_scalings <- length(unique(forecast_gdp_scaled$gdp_scaling))
  testthat::expect_true(unique_gdp_scalings > 1,
                       info = "Different countries should have different GDP scaling factors")
})

test_that("Test population vs GDP scaling comparison", {

  # Create test HIA cost data
  test_hia_cost <- data.frame(
    iso3 = c("USA", "ZAF"),
    region_id = c("USA", "ZAF"),
    Outcome = c("Deaths", "Deaths"),
    year = c(2019, 2019),
    number = c(100, 50),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 9.3),
    AgeGrp = c("25+", "25+"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test with ONLY population scaling
  testthat::expect_no_error({
    forecast_pop_only <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = c(2020, 2023),
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Test with BOTH population and GDP scaling
  testthat::expect_no_error({
    forecast_both <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = c(2020, 2023),
      reference_year = 2019,
      use_gdp_scaling = TRUE,
      discount_rate = 0.03
    )
  })

  # Test that both produce same structure
  testthat::expect_equal(nrow(forecast_pop_only), nrow(forecast_both))
  testthat::expect_equal(ncol(forecast_pop_only), ncol(forecast_both))

  # Test that population scaling is the same in both
  testthat::expect_equal(forecast_pop_only$pop_scaling, forecast_both$pop_scaling)

  # Test that GDP scaling is 1 in pop-only, but varies in both
  testthat::expect_true(all(forecast_pop_only$gdp_scaling == 1))
  testthat::expect_true(any(forecast_both$gdp_scaling != 1))

  # Test that costs are different between the two approaches
  # (because GDP scaling affects the final cost calculation)
  testthat::expect_true(any(forecast_pop_only$cost_mn_currentUSD != forecast_both$cost_mn_currentUSD))

  # Test that the relationship is: cost_both = cost_pop_only * GDPscaling
  # (since both have same pop_scaling)
  for(i in 1:nrow(forecast_pop_only)) {
    expected_cost <- forecast_pop_only$cost_mn_currentUSD[i] * forecast_both$gdp_scaling[i]
    actual_cost <- forecast_both$cost_mn_currentUSD[i]
    testthat::expect_equal(actual_cost, expected_cost, tolerance = 0.001,
                          info = paste("Row", i, "cost calculation mismatch"))
  }
})

test_that("Test get_econ_forecast age group handling", {

  # Create test HIA cost data with different age groups
  test_hia_cost <- data.frame(
    iso3 = c("USA", "USA"),
    region_id = c("USA", "USA"),
    Outcome = c("Deaths", "Deaths"),
    year = c(2019, 2019),
    number = c(100, 50),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 0.5),
    AgeGrp = c("25+", "0-4"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test forecast with different age groups
  testthat::expect_no_error({
    forecast_age <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = c(2020, 2023),
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Test that all age groups are handled
  testthat::expect_true(is.data.frame(forecast_age))
  testthat::expect_true(nrow(forecast_age) > 0)

  # Test that synthetic age groups are created if needed
  testthat::expect_true("AgeGrp" %in% names(forecast_age))
})

test_that("Test get_econ_forecast input handling", {

  # Test with list input (hia_cost$hia_cost)
  test_hia_cost <- data.frame(
    iso3 = c("USA"),
    region_id = c("USA"),
    Outcome = c("Deaths"),
    year = c(2019),
    number = c(100),
    cost_mn_currentUSD = c(1.1),
    cost_mn_currentLCU = c(1.1),
    AgeGrp = c("25+"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Test with list input
  testthat::expect_no_error({
    forecast_list <- creahia::get_econ_forecast(
      list(hia_cost = test_hia_cost),
      forecast_years = c(2020),
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Test with data frame input
  testthat::expect_no_error({
    forecast_df <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = c(2020),
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Both should produce same results
  testthat::expect_equal(nrow(forecast_list), nrow(forecast_df))
})

test_that("Test get_econ_forecast edge cases", {

  # Test with single year
  test_hia_cost <- data.frame(
    iso3 = c("USA"),
    region_id = c("USA"),
    Outcome = c("Deaths"),
    year = c(2019),
    number = c(100),
    cost_mn_currentUSD = c(1.1),
    cost_mn_currentLCU = c(1.1),
    AgeGrp = c("25+"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Test with single forecast year
  testthat::expect_no_error({
    forecast_single <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = 2023,
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Test that both 2019 (target) and 2023 (forecast) are present
  testthat::expect_true(all(c(2019, 2023) %in% unique(forecast_single$year)))

  # Test with same target and forecast year
  testthat::expect_no_error({
    forecast_same <- creahia::get_econ_forecast(
      test_hia_cost,
      forecast_years = 2019,
      reference_year = 2019,
      use_gdp_scaling = FALSE
    )
  })

  # Scaling factors should be 1 for target year (2019)
  target_year_data <- forecast_same %>% filter(year == 2019)
  testthat::expect_true(all(abs(target_year_data$pop_scaling - 1) < 0.01))
})

test_that("Test GDP scaling for several countries", {

  hia_cost <- readRDS(get_test_file(file.path("example_kaz", "hia_cost.RDS")))



  econ_unscaled <- creahia::get_econ_forecast(hia_cost,
                                        forecast_years=seq(2010, 2020),
                                        reference_year=2019,
                                        use_gdp_scaling=FALSE)

  econ_scaled <- creahia::get_econ_forecast(hia_cost,
                                            forecast_years=seq(2010, 2020),
                                            reference_year=2019,
                                            use_gdp_scaling=TRUE)

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
