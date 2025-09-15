testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())

# Test the economic income system foundation

test_that("Test get_income returns expected structure", {

  # Test that income data can be loaded
  testthat::expect_no_error({
    income <- creahia::get_income()
  })

  # Test structure
  testthat::expect_true(is.data.frame(income))
  testthat::expect_true(nrow(income) > 0)

  # Test required columns
  required_cols <- c("iso3", "year", "income_type", "value")
  testthat::expect_true(all(required_cols %in% names(income)))

  # Test data types
  testthat::expect_true(is.character(income$iso3))
  testthat::expect_true(is.numeric(income$year))
  testthat::expect_true(is.character(income$income_type))
  testthat::expect_true(is.numeric(income$value))

  # Test that income types follow expected pattern
  expected_patterns <- c("GDP\\.PC\\.currUSD", "GDP\\.PC\\.PPP\\.currUSD",
                         "GNI\\.PC\\.currUSD", "GNI\\.PC\\.PPP\\.currUSD",
                         "GDP\\.TOT\\.currUSD", "GDP\\.TOT\\.currLCU",
                         "PPP\\.convLCUUSD")

  for(pattern in expected_patterns) {
    matching_types <- income %>% filter(stringr::str_detect(income_type, pattern))
    testthat::expect_true(nrow(matching_types) > 0,
                          info = paste("Pattern", pattern, "not found in income types"))
  }

  # Test that major countries are present
  major_countries <- c("USA", "GBR", "DEU", "FRA", "ITA", "ESP", "CAN", "AUS", "JPN", "CHN", "IND", "BRA", "ZAF")

  for(country in major_countries) {
    country_data <- income %>% filter(iso3 == !!country)
    testthat::expect_true(nrow(country_data) > 0,
                          info = paste("Country", country, "not found in income data"))
  }

  # Test that World Bank aggregate regions are present
  aggregate_regions <- c("WLD", "OED", "EUU", "EMU")

  for(region in aggregate_regions) {
    region_data <- income %>% filter(iso3 == !!region)
    testthat::expect_true(nrow(region_data) > 0,
                          info = paste("Region", region, "not found in income data"))
  }

  # Test that we have data for recent years
  recent_years <- c(2019, 2020, 2021, 2022, 2023)

  for(year in recent_years) {
    year_data <- income %>% filter(year == !!year)
    testthat::expect_true(nrow(year_data) > 0,
                          info = paste("Year", year, "not found in income data"))
  }

})
