test_that("Test get_valuations_raw loads data correctly", {

  # Test that raw valuation data can be loaded
  testthat::expect_no_error({
    raw_valuation <- creahia::get_valuations_raw("viscusi")
  })

  # Test structure
  testthat::expect_true(is.data.frame(raw_valuation))
  testthat::expect_true(nrow(raw_valuation) > 0)

  # Test required columns
  required_cols <- c("Outcome", "Outcome_name", "valuation", "currency", "unit",
                     "year", "source", "reference_income_level", "elasticity",
                     "gni_or_gdp", "ppp")
  testthat::expect_true(all(required_cols %in% names(raw_valuation)))

  # Test that all outcomes are present
  expected_outcomes <- c("Asthma.Prev", "exac", "PTB", "Deaths", "Deaths.child", "YLDs", "Absences")
  testthat::expect_true(all(expected_outcomes %in% raw_valuation$Outcome))

  # Test that all currencies are handled
  expected_currencies <- c("USD", "GBP", "EUR")
  testthat::expect_true(all(expected_currencies %in% raw_valuation$currency))

  # Test that elasticity values are reasonable
  testthat::expect_true(all(raw_valuation$elasticity >= 0.5 & raw_valuation$elasticity <= 2))

  # Test that Death outcomes use GNI
  death_outcomes <- raw_valuation %>% filter(Outcome %in% c("Deaths", "Deaths.child"))
  testthat::expect_true(all(death_outcomes$gni_or_gdp == "gni"))

  # Test that Death outcomes do not use PPP
  testthat::expect_true(all(death_outcomes$ppp == FALSE))
})

test_that("Test transfer_to_usd handles different currencies", {

  # Create test data with different currencies
  test_data <- data.frame(
    Outcome = c("Test1", "Test2", "Test3"),
    reference_income_level = c("World", "World", "World"),
    currency = c("USD", "GBP", "EUR"),
    year = c(2018, 2018, 2010),
    valuation = c(100, 100, 100),
    stringsAsFactors = FALSE
  )

  # Test that function runs without error
  testthat::expect_no_error({
    result <- creahia::transfer_to_usd(test_data)
  })

  # Test structure
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(nrow(result) == nrow(test_data))

  # Test that USD values are unchanged
  usd_row <- result %>% filter(currency == "USD")
  testthat::expect_equal(usd_row$valuation_usd_reference, 100)

  # Test that GBP values are converted (using hardcoded rate 1.334946)
  gbp_row <- result %>% filter(currency == "GBP")
  testthat::expect_equal(gbp_row$valuation_usd_reference, 100 * 1.334946)

  # Test that EUR values are converted (using hardcoded rate 1.326974)
  eur_row <- result %>% filter(currency == "EUR")
  testthat::expect_equal(eur_row$valuation_usd_reference, 100 * 1.326974)

  # Test that reference_income_level is properly handled
  testthat::expect_true("reference_income_level" %in% names(result))
})

test_that("Test attach_reference_income works correctly", {

  # Get raw valuation data
  raw_valuation <- creahia::get_valuations_raw("viscusi")

  # Test that function runs without error
  testthat::expect_no_error({
    result <- creahia::attach_reference_income(raw_valuation)
  })

  # Test structure
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(nrow(result) == nrow(raw_valuation))

  # Test that income_type is created correctly
  testthat::expect_true("income_type" %in% names(result))
  testthat::expect_true("income_reference" %in% names(result))

  # Test that GNI PPP outcomes get correct income type
  gni_ppp_outcomes <- result %>% filter(gni_or_gdp == "gni" & ppp == TRUE)
  testthat::expect_true(all(gni_ppp_outcomes$income_type == "GNI.PC.PPP.constUSD"))

  # Test that reference income levels are properly matched
  testthat::expect_true(all(!is.na(result$income_reference)))

  # Test that local reference levels are handled
  california_rows <- result %>% filter(reference_income_level == "California")
  testthat::expect_true(nrow(california_rows) > 0)
})

test_that("Test attach_target_income works correctly", {

  # Get raw valuation and attach reference income
  raw_valuation <- creahia::get_valuations_raw("viscusi")
  valuation_with_ref <- creahia::attach_reference_income(raw_valuation)

  # Test with specific countries and years
  testthat::expect_no_error({
    result <- creahia::attach_target_income(valuation_with_ref,
                                           iso3s = c("ZAF", "USA"),
                                           years = c(2020, 2023))
  })

  # Test structure
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(nrow(result) > 0)

  # Test that target income is attached
  testthat::expect_true("income_target" %in% names(result))
  testthat::expect_true("lcu_per_usd" %in% names(result))
  testthat::expect_true("gdp_curr_usd" %in% names(result))

  # Test that all required countries are present
  testthat::expect_true(all(c("ZAF", "USA") %in% result$iso3))

  # Test that all required years are present
  testthat::expect_true(all(c(2020, 2023) %in% result$year))

  # Test that income data is properly joined
  testthat::expect_true(all(!is.na(result$income_target)))
  testthat::expect_true(all(!is.na(result$lcu_per_usd)))
})

test_that("Test compute_transferred_valuation works correctly", {

  # Get raw valuation and process through all steps
  raw_valuation <- creahia::get_valuations_raw("viscusi")
  valuation_with_ref <- creahia::attach_reference_income(raw_valuation)
  valuation_with_target <- creahia::attach_target_income(valuation_with_ref,
                                                        iso3s = c("ZAF"),
                                                        years = c(2023))

  # Test that function runs without error
  testthat::expect_no_error({
    result <- creahia::compute_transferred_valuation(valuation_with_target)
  })

  # Test structure
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(nrow(result) > 0)

  # Test that transferred valuation is calculated
  testthat::expect_true("valuation_usd" %in% names(result))

  # Test that all valuations are positive
  testthat::expect_true(all(result$valuation_usd > 0))

  # Test that all valuations are finite
  testthat::expect_true(all(is.finite(result$valuation_usd)))

  # Test that the number of rows is preserved
  testthat::expect_equal(nrow(result), nrow(valuation_with_target))
})

test_that("Test get_valuations runs end-to-end", {

  # Test that the main function runs without error
  testthat::expect_no_error({
    result <- creahia::get_valuations(valuation_version = "viscusi",
                                     iso3s = c("ZAF", "USA"),
                                     years = c(2023))
  })

  # Test structure
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true(nrow(result) > 0)

  # Test that required columns are present
  required_cols <- c("Outcome", "iso3", "year", "valuation_usd",
                     "lcu_per_usd", "gdp_curr_usd")
  testthat::expect_true(all(required_cols %in% names(result)))

  # Test that all outcomes are processed
  expected_outcomes <- c("Asthma.Prev", "exac", "PTB", "Deaths", "Deaths.child", "YLDs", "Absences")
  testthat::expect_true(all(expected_outcomes %in% result$Outcome))

  # Test that all countries are processed
  testthat::expect_true(all(c("ZAF", "USA") %in% result$iso3))

  # Test that all years are processed
  testthat::expect_true(all(c(2023) %in% result$year))

  # Test that valuations are reasonable
  testthat::expect_true(all(result$valuation_usd > 0))
  testthat::expect_true(all(result$lcu_per_usd > 0))
  testthat::expect_true(all(result$gdp_curr_usd > 0))
})

test_that("get_valuations returns correct VSL for ZAF 2019 - Worldbank version", {

  # Test get_valuations function directly
  valuations <- creahia::get_valuations("ZAF", 2019, valuation_version = "worldbank")

  # Check that we get a data frame with expected columns
  testthat::expect_true(is.data.frame(valuations))
  testthat::expect_true("Outcome" %in% names(valuations))
  testthat::expect_true("iso3" %in% names(valuations))
  testthat::expect_true("year" %in% names(valuations))
  testthat::expect_true("valuation_usd" %in% names(valuations))

  # Check that we have data for ZAF 2019
  zaf_2019 <- valuations %>% filter(iso3 == "ZAF", year == 2019)
  testthat::expect_true(nrow(zaf_2019) > 0)

  # Check that we have a VSL for Deaths
  deaths_vsl <- zaf_2019 %>% filter(Outcome == "Deaths")
  testthat::expect_true(nrow(deaths_vsl) == 1)
  testthat::expect_true(!is.na(deaths_vsl$valuation_usd))
  testthat::expect_true(deaths_vsl$valuation_usd > 0)

  # Manually compute expected VSL for comparison
  vsl_ref_amount <- 3830000
  vsl_ref_iso3 <- "OED"
  vsl_ref_elasticity <- 1.2
  vsl_ref_year <- 2011

  vsls <- creahia::safe_wb_data(c(gdp_current = 'NY.GDP.PCAP.CD',
                         gdp_ppp_current = 'NY.GDP.PCAP.PP.CD',
                         gdp_ppp_constant = 'NY.GDP.PCAP.PP.KD',
                         gdp_constant = 'NY.GDP.PCAP.KD',
                         gdp_current_lcu = 'NY.GDP.PCAP.CN'
  ),
  start_date = vsl_ref_year,
  end_date = 2019,
  country=c("ZAF", vsl_ref_iso3)) %>%
    select(iso3c, date, gdp_current, gdp_ppp_constant, gdp_current_lcu, gdp_ppp_current) %>%
    mutate(iso3c = if_else(iso3c == vsl_ref_iso3, "REF", iso3c)) %>%
    pivot_wider(names_from = iso3c, values_from = -c(iso3c, date))

  vsls$vsl_ppp <- vsl_ref_amount * (vsls$gdp_ppp_constant_ZAF / vsls$gdp_ppp_constant_REF[vsls$date==vsl_ref_year]) ^ vsl_ref_elasticity
  vsls$vsl_market <- vsls$vsl_ppp * vsls$gdp_current_ZAF / vsls$gdp_ppp_constant_ZAF

  # Remove attribute
  attr(vsls$vsl_market, "label") <- NULL

  vsl_manual_2019 <- vsls %>% filter(date == 2019) %>% pull(vsl_market)

  # Compare with tolerance
  testthat::expect_equal(deaths_vsl$valuation_usd, vsl_manual_2019, tolerance = 0.01)
})

test_that("get_valuations returns correct VSL for ZAF 2019 - Viscusi version", {

  # Test get_valuations function directly
  valuations <- creahia::get_valuations("ZAF", 2019, valuation_version = "viscusi")

  # Check that we get a data frame with expected columns
  testthat::expect_true(is.data.frame(valuations))
  testthat::expect_true("Outcome" %in% names(valuations))
  testthat::expect_true("iso3" %in% names(valuations))
  testthat::expect_true("year" %in% names(valuations))
  testthat::expect_true("valuation_usd" %in% names(valuations))

  # Check that we have data for ZAF 2019
  zaf_2019 <- valuations %>% filter(iso3 == "ZAF", year == 2019)
  testthat::expect_true(nrow(zaf_2019) > 0)

  # Check that we have a VSL for Deaths
  deaths_vsl <- zaf_2019 %>% filter(Outcome == "Deaths")
  testthat::expect_true(nrow(deaths_vsl) == 1)
  testthat::expect_true(!is.na(deaths_vsl$valuation_usd))
  testthat::expect_true(deaths_vsl$valuation_usd > 0)

  # Manually compute expected VSL for comparison
  vsl_ref_amount <- 9631000
  vsl_ref_iso3 <- "USA"
  vsl_ref_elasticity <- 1
  vsl_ref_year <- 2015

  vsls <- creahia::safe_wb_data(c(gdp_current = 'NY.GDP.PCAP.CD',
                         gdp_ppp_current = 'NY.GDP.PCAP.PP.CD',
                         gdp_ppp_constant = 'NY.GDP.PCAP.PP.KD',
                         gdp_constant = 'NY.GDP.PCAP.KD',
                         gdp_current_lcu = 'NY.GDP.PCAP.CN',
                         gni_current = 'NY.GNP.PCAP.CD',
                         gni_constant = 'NY.GNP.PCAP.KD',
                         gni_ppp_constant = 'NY.GNP.PCAP.PP.KD',
                         gni_ppp_current = 'NY.GNP.PCAP.PP.CD'
  ),
  start_date = vsl_ref_year,
  end_date = 2019,
  country=c("ZAF", vsl_ref_iso3)) %>%
    select(iso3c, date, gni_constant, gni_current) %>%
    mutate(iso3c = if_else(iso3c == vsl_ref_iso3, "REF", iso3c)) %>%
    pivot_wider(names_from = iso3c, values_from = -c(iso3c, date))

  vsls$vsl_market <- vsl_ref_amount * (vsls$gni_constant_ZAF / vsls$gni_constant_REF[vsls$date==vsl_ref_year]) ^ vsl_ref_elasticity * vsls$gni_current_ZAF / vsls$gni_constant_ZAF

  # Remove attribute
  attr(vsls$vsl_market, "label") <- NULL

  vsl_manual_2019 <- vsls %>% filter(date == 2019) %>% pull(vsl_market)

  # Compare with tolerance
  testthat::expect_equal(deaths_vsl$valuation_usd, vsl_manual_2019, tolerance = 0.1)
})

test_that("Test valuation system with multiple countries and years", {

  # Test with multiple countries and years
  testthat::expect_no_error({
    multi_valuation <- creahia::get_valuations(valuation_version = "viscusi",
                                              iso3s = c("ZAF", "USA", "GBR"),
                                              years = c(2020, 2023))
  })

  # Test structure
  testthat::expect_true(is.data.frame(multi_valuation))
  testthat::expect_true(nrow(multi_valuation) > 0)

  # Test that all countries are present
  testthat::expect_true(all(c("ZAF", "USA", "GBR") %in% multi_valuation$iso3))

  # Test that all years are present
  testthat::expect_true(all(c(2020, 2023) %in% multi_valuation$year))

  # Test that all outcomes are present for each country-year combination
  expected_combinations <- length(c("ZAF", "USA", "GBR")) * length(c(2020, 2023)) * 7  # 7 outcomes
  testthat::expect_equal(nrow(multi_valuation), expected_combinations)

  # Test that valuations are reasonable across all countries
  testthat::expect_true(all(multi_valuation$valuation_usd > 0))
  testthat::expect_true(all(multi_valuation$lcu_per_usd > 0))

  # Test that USA valuations are in USD (lcu_per_usd should be close to 1)
  usa_rows <- multi_valuation %>% filter(iso3 == "USA")
  testthat::expect_true(all(abs(usa_rows$lcu_per_usd - 1) < 0.1))

  # Test that GBR valuations use GBP conversion
  gbr_rows <- multi_valuation %>% filter(iso3 == "GBR")
  testthat::expect_true(all(gbr_rows$lcu_per_usd > 0.7 & gbr_rows$lcu_per_usd < 1.5))
})
