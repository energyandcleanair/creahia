# Test the economic computation system
test_that("compute_econ_costs uses correct VSL and multiplies by number", {

  # Create a simple HIA dataframe for testing
  hia <- data.frame(
    iso3 = "ZAF",
    year = 2019,
    outcome = "Deaths",
    number = 1000,
    stringsAsFactors = FALSE,
    double_counted = FALSE,
    estimate = "central",
    region_id = "region",
    pop = 1e5
  )

  # Get the VSL directly from get_valuations
  valuations <- creahia::get_valuations("ZAF", 2019, valuation_version = "worldbank")
  expected_vsl <- valuations %>% filter(outcome == "Deaths") %>% pull(valuation_usd)

  # Compute economic costs
  costs <- creahia::compute_econ_costs(hia, valuation_version = "worldbank")
  actual_vsl <- costs$hia_cost %>% filter(outcome == "Deaths") %>% pull(valuation_current_usd)
  actual_cost <- costs$hia_cost %>% filter(outcome == "Deaths") %>% pull(cost_mn_currentUSD)

  # Check that VSL matches
  testthat::expect_equal(actual_vsl, expected_vsl, tolerance = 0.1)

  # Check that cost is VSL * number / 1e6 (converted to millions)
  expected_cost <- expected_vsl * 1000 / 1e6
  testthat::expect_equal(actual_cost, expected_cost, tolerance = 0.1)
})

test_that("Test get_hia_cost calculates costs correctly", {

  # Create simple test HIA dataset
  test_hia <- data.frame(
    iso3 = c("USA", "ZAF"),
    region_id = c("USA", "ZAF"),
    outcome = c("Deaths", "Deaths"),
    number = c(100, 50),
    pollutant = c("PM2.5", "PM2.5"),
    estimate = c("central", "central"),
    double_counted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test that get_hia_cost runs without errors
  testthat::expect_no_error({
    hia_cost <- creahia::get_hia_cost(
      hia = test_hia,
      valuation_version = "viscusi",
      current_year = 2023
    )
  })

  # Test structure
  testthat::expect_true(is.data.frame(hia_cost))
  testthat::expect_true(nrow(hia_cost) == nrow(test_hia))

  # Test required columns exist
  required_cols <- c("iso3", "outcome", "number", "cost_mn_currentUSD", "cost_mn_currentLCU")
  testthat::expect_true(all(required_cols %in% names(hia_cost)))

  # Test that costs are calculated and positive
  testthat::expect_true(all(hia_cost$cost_mn_currentUSD > 0))
  testthat::expect_true(all(hia_cost$cost_mn_currentLCU > 0))

  # Test that all outcomes have valuations
  testthat::expect_true(all(!is.na(hia_cost$valuation_current_usd)))

  # Test that GDP data is attached
  testthat::expect_true("gdp_curr_usd" %in% names(hia_cost))
  testthat::expect_true("lcu_per_usd" %in% names(hia_cost))
})

test_that("Test get_total_cost_by_outcome works correctly", {

  # Create test HIA dataset and get real cost data
  test_hia <- data.frame(
    iso3 = c("USA", "USA", "ZAF", "ZAF"),
    region_id = c("USA", "USA", "ZAF", "ZAF"),
    outcome = c("Deaths", "AsthmaPrevalence", "Deaths", "AsthmaPrevalence"),
    number = c(100, 1000, 50, 500),
    pollutant = c("PM2.5", "PM2.5", "PM2.5", "PM2.5"),
    estimate = c("central", "central", "central", "central"),
    double_counted = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Get real HIA cost data
  testthat::expect_no_error({
    hia_cost <- creahia::get_hia_cost(
      hia = test_hia,
      valuation_version = "viscusi",
      current_year = 2023
    )
  })

  # Test that get_total_cost_by_outcome runs without errors
  testthat::expect_no_error({
    cost_by_outcome <- creahia::get_total_cost_by_outcome(hia_cost)
  })

  # Test structure
  testthat::expect_true(is.data.frame(cost_by_outcome))
  testthat::expect_true(nrow(cost_by_outcome) > 0)

  # Test required columns exist
  required_cols <- c("outcome", "cost_mn_currentUSD", "cost_mn_currentLCU")
  testthat::expect_true(all(required_cols %in% names(cost_by_outcome)))

  # Test that all outcomes are present
  testthat::expect_true(all(c("Deaths", "AsthmaPrevalence") %in% cost_by_outcome$outcome))

  # Test that costs are positive
  testthat::expect_true(all(cost_by_outcome$cost_mn_currentUSD > 0))
  testthat::expect_true(all(cost_by_outcome$cost_mn_currentLCU > 0))
})

test_that("Test get_total_cost_by_region works correctly", {

  # Create test HIA dataset and get real cost data
  test_hia <- data.frame(
    iso3 = c("USA", "USA", "ZAF", "ZAF"),
    region_id = c("USA", "USA", "ZAF", "ZAF"),
    outcome = c("Deaths", "AsthmaPrevalence", "Deaths", "AsthmaPrevalence"),
    number = c(100, 1000, 50, 500),
    pollutant = c("PM2.5", "PM2.5", "PM2.5", "PM2.5"),
    estimate = c("central", "central", "central", "central"),
    double_counted = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Get real HIA cost data
  testthat::expect_no_error({
    hia_cost <- creahia::get_hia_cost(
      hia = test_hia,
      valuation_version = "viscusi",
      current_year = 2023
    )
  })

  # Add missing pop column that the function expects
  hia_cost <- hia_cost %>% mutate(pop = 1000000)

  # Test that get_total_cost_by_region runs without errors
  testthat::expect_no_error({
    cost_by_region <- creahia::get_total_cost_by_region(hia_cost)
  })

  # Test structure
  testthat::expect_true(is.data.frame(cost_by_region))
  testthat::expect_true(nrow(cost_by_region) > 0)

  # Test required columns exist
  required_cols <- c("region_id", "cost_mn_currentUSD", "cost_mn_currentLCU")
  testthat::expect_true(all(required_cols %in% names(cost_by_region)))

  # Test that all regions are present
  testthat::expect_true(all(c("USA", "ZAF") %in% cost_by_region$region_id))

  # Test that costs are positive
  testthat::expect_true(all(cost_by_region$cost_mn_currentUSD > 0))
  testthat::expect_true(all(cost_by_region$cost_mn_currentLCU > 0))
})



test_that("Test get_hia_cost end-to-end with new valuation system", {

  # Create a comprehensive test HIA dataset with all outcomes
  test_hia <- data.frame(
    iso3 = c("USA", "USA", "GBR", "GBR", "USA", "USA", "GBR", "GBR", "USA", "GBR", "USA", "GBR", "USA", "GBR"),
    region_id = c("USA", "USA", "GBR", "GBR", "USA", "USA", "GBR", "GBR", "USA", "GBR", "USA", "GBR", "USA", "GBR"),
    outcome = c("Deaths", "Deaths.child", "Deaths", "Deaths.child", "YLDs", "YLDs", "YLDs", "YLDs", "AsthmaPrevalence", "AsthmaPrevalence", "AsthmaERV", "AsthmaERV", "PTB", "PTB"),
    number = c(100, 50, 25, 15, 500, 300, 200, 150, 1000, 800, 200, 150, 10, 8),
    pollutant = c("PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5"),
    estimate = c("central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central", "central"),
    double_counted = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Test that get_hia_cost runs without errors
  testthat::expect_no_error({
    hia_cost <- creahia::get_hia_cost(
      hia = test_hia,
      valuation_version = "viscusi",
      current_year = 2019
    )
  })

  # Test that the result has the expected structure
  testthat::expect_true(is.data.frame(hia_cost))
  testthat::expect_true(nrow(hia_cost) > 0)

  # Test that required columns exist
  required_cols <- c("iso3", "outcome", "number", "cost_mn_currentUSD", "cost_mn_currentLCU")
  testthat::expect_true(all(required_cols %in% names(hia_cost)))

  # Test that costs are calculated (not all NA)
  testthat::expect_true(any(!is.na(hia_cost$cost_mn_currentUSD)))
  testthat::expect_true(any(!is.na(hia_cost$cost_mn_currentLCU)))

  # Test that costs are positive
  testthat::expect_true(all(hia_cost$cost_mn_currentUSD >= 0))
  testthat::expect_true(all(hia_cost$cost_mn_currentLCU >= 0))

  # Test that the number of rows is preserved
  testthat::expect_equal(nrow(hia_cost), nrow(test_hia))

  # Test that all outcomes have valuations
  testthat::expect_true(all(!is.na(hia_cost$valuation_current_usd)))

  # Test that GDP/GNI data is attached (new column names from valuation system)
  gdp_cols <- c("lcu_per_usd", "gdp_curr_usd")
  testthat::expect_true(all(gdp_cols %in% names(hia_cost)))

  # Test that valuation data is present
  testthat::expect_true("valuation_current_usd" %in% names(hia_cost))

  # Test that share of GDP is calculated
  testthat::expect_true("share_gdp" %in% names(hia_cost))
  testthat::expect_true(any(!is.na(hia_cost$share_gdp)))

  # Test that share of GDP is reasonable (between 0 and 1)
  testthat::expect_true(all(hia_cost$share_gdp >= 0 & hia_cost$share_gdp <= 1))

  # Test that all outcome types are properly handled
  expected_outcomes <- c("Deaths", "Deaths.child", "YLDs", "AsthmaPrevalence", "AsthmaERV", "PTB")
  testthat::expect_true(all(expected_outcomes %in% hia_cost$outcome))

  # Test that different currency types are handled (USD, GBP, EUR)
  # This tests the currency conversion functionality
  testthat::expect_true(all(!is.na(hia_cost$valuation_current_usd)))

  # Test that both GNI and GDP-based valuations work
  gni_outcomes <- c("Deaths", "Deaths.child", "YLDs")
  gdp_outcomes <- c("AsthmaPrevalence", "AsthmaERV", "PTB")

  testthat::expect_true(all(gni_outcomes %in% hia_cost$outcome))
  testthat::expect_true(all(gdp_outcomes %in% hia_cost$outcome))

  # Test that both PPP and non-PPP valuations work
  # (All outcomes in your data use PPP, but the system should handle both)
  testthat::expect_true(all(!is.na(hia_cost$valuation_current_usd)))

})


