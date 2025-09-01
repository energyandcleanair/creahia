testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())

# Test the economic computation system

test_that("Test get_hia_cost calculates costs correctly", {

  # Create simple test HIA dataset
  test_hia <- data.frame(
    iso3 = c("USA", "ZAF"),
    region_id = c("USA", "ZAF"),
    Outcome = c("Deaths", "Deaths"),
    number = c(100, 50),
    Pollutant = c("PM2.5", "PM2.5"),
    estimate = c("central", "central"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test that get_hia_cost runs without errors
  testthat::expect_no_error({
    hia_cost <- creahia::get_hia_cost(
      hia = test_hia,
      valuation_version = "viscusi_gni",
      current_year = 2023
    )
  })

  # Test structure
  testthat::expect_true(is.data.frame(hia_cost))
  testthat::expect_true(nrow(hia_cost) == nrow(test_hia))

  # Test required columns exist
  required_cols <- c("iso3", "Outcome", "number", "cost_mn_currentUSD", "cost_mn_currentLCU")
  testthat::expect_true(all(required_cols %in% names(hia_cost)))

  # Test that costs are calculated and positive
  testthat::expect_true(all(hia_cost$cost_mn_currentUSD > 0, na.rm = TRUE))
  testthat::expect_true(all(hia_cost$cost_mn_currentLCU > 0, na.rm = TRUE))

  # Test that all outcomes have valuations
  testthat::expect_true(all(!is.na(hia_cost$valuation_current_usd)))

  # Test that GDP data is attached
  testthat::expect_true("gdp_curr_usd" %in% names(hia_cost))
  testthat::expect_true("lcu_per_usd" %in% names(hia_cost))
})

test_that("Test get_total_cost_by_outcome works correctly", {

  # Create test HIA cost data
  test_hia_cost <- data.frame(
    scenario = c("BAU", "BAU"),
    estimate = c("central", "central"),
    Outcome = c("Deaths", "YLLs"),
    number = c(100, 2000),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(20.4, 9.3),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test function runs without error
  testthat::expect_no_error({
    cost_by_outcome <- creahia::get_total_cost_by_outcome(test_hia_cost)
  })

  # Test structure
  testthat::expect_true(is.data.frame(cost_by_outcome))
  testthat::expect_true(nrow(cost_by_outcome) == 2)  # 2 outcomes

  # Test that costs are summed correctly
  deaths_cost <- cost_by_outcome %>% filter(Outcome == "Deaths")
  testthat::expect_equal(deaths_cost$cost_mn_currentUSD, 1.1)
  testthat::expect_equal(deaths_cost$cost_mn_currentLCU, 20.4)

  # Test that valuation columns are calculated
  testthat::expect_true("valuation_currentUSD" %in% names(cost_by_outcome))
  testthat::expect_true("valuation_currentLCU" %in% names(cost_by_outcome))
})

test_that("Test get_total_cost_by_region works correctly", {

  # Create test HIA cost data with regions
  test_hia_cost <- data.frame(
    scenario = c("BAU", "BAU"),
    estimate = c("central", "central"),
    region_id = c("USA", "ZAF"),
    pop = c(1000000, 500000),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 9.3),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test function runs without error
  testthat::expect_no_error({
    cost_by_region <- creahia::get_total_cost_by_region(test_hia_cost)
  })

  # Test structure
  testthat::expect_true(is.data.frame(cost_by_region))
  testthat::expect_true(nrow(cost_by_region) == 2)  # 2 regions

  # Test that costs are summed correctly
  usa_cost <- cost_by_region %>% filter(region_id == "USA")
  testthat::expect_equal(usa_cost$cost_mn_currentUSD, 1.1)
  testthat::expect_equal(usa_cost$cost_mn_currentLCU, 1.1)

  # Test that population is preserved
  testthat::expect_true("pop" %in% names(cost_by_region))
})

test_that("Test get_econ_forecast works correctly", {

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

  # Test forecast without GDP scaling
  testthat::expect_no_error({
    forecast_unscaled <- creahia::get_econ_forecast(
      test_hia_cost,
      years = c(2020, 2023),
      pop_targetyr = 2019,
      GDP_scaling = FALSE
    )
  })

  # Test structure
  testthat::expect_true(is.data.frame(forecast_unscaled))
  testthat::expect_true(nrow(forecast_unscaled) > 0)

  # Test that forecast years are present
  testthat::expect_true(all(c(2020, 2023) %in% forecast_unscaled$year))

  # Test that costs are scaled by population
  testthat::expect_true("cost_mn_currentUSD" %in% names(forecast_unscaled))

  # Test forecast with GDP scaling
  testthat::expect_no_error({
    forecast_scaled <- creahia::get_econ_forecast(
      test_hia_cost,
      years = c(2020, 2023),
      pop_targetyr = 2019,
      GDP_scaling = TRUE
    )
  })

  # Test that GDP scaling produces different results
  testthat::expect_true(nrow(forecast_scaled) > 0)
  testthat::expect_true("cost_mn_currentUSD" %in% names(forecast_scaled))
})

test_that("Test compute_econ_costs integration", {

  # Create test HIA dataset
  test_hia <- data.frame(
    iso3 = c("USA", "ZAF"),
    pop = c(1000000, 500000),
    region_id = c("USA", "ZAF"),
    Outcome = c("Deaths", "Deaths"),
    number = c(100, 50),
    Pollutant = c("PM2.5", "PM2.5"),
    estimate = c("central", "central"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test that main function runs without errors
  testthat::expect_no_error({
    econ_results <- creahia::compute_econ_costs(
      hia = test_hia,
      valuation_version = "viscusi_gni",
      current_year = 2023
    )
  })

  # Test structure
  testthat::expect_true(is.list(econ_results))
  testthat::expect_true(all(c("hia_cost", "cost_by_outcome", "cost_by_region") %in% names(econ_results)))

  # Test that all components are data frames
  testthat::expect_true(is.data.frame(econ_results$hia_cost))
  testthat::expect_true(is.data.frame(econ_results$cost_by_outcome))
  testthat::expect_true(is.data.frame(econ_results$cost_by_region))

  # Test that costs are calculated
  testthat::expect_true(all(econ_results$hia_cost$cost_mn_currentUSD > 0, na.rm = TRUE))
  testthat::expect_true(all(econ_results$cost_by_outcome$cost_mn_currentUSD > 0, na.rm = TRUE))
})
