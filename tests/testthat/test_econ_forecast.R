testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())

# Test the economic forecasting system

test_that("Test population scaling only - fatal vs non-fatal outcomes", {

  # Create test HIA cost data with both fatal and non-fatal outcomes
  test_hia_cost <- data.frame(
    iso3 = c("USA", "USA"),
    region_id = c("USA", "USA"),
    outcome = c("Deaths", "Asthma.Prev"),
    year = c(2019, 2019),
    number = c(100, 1000),
    cost_mn_currentUSD = c(1.1, 0.1),
    cost_mn_currentLCU = c(1.1, 0.1),
    age_group = c("25+", "25+"),
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
  required_cols <- c("iso3", "outcome", "year", "pop_scaling", "gdp_scaling", "cost_mn_currentUSD")
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
    outcome = c("Deaths", "Deaths"),
    year = c(2019, 2019),
    number = c(100, 50),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 9.3),
    age_group = c("25+", "25+"),
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
    outcome = c("Deaths", "Deaths"),
    year = c(2019, 2019),
    number = c(100, 50),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 9.3),
    age_group = c("25+", "25+"),
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
    outcome = c("Deaths", "Deaths"),
    year = c(2019, 2019),
    number = c(100, 50),
    cost_mn_currentUSD = c(1.1, 0.5),
    cost_mn_currentLCU = c(1.1, 0.5),
    age_group = c("25+", "0-4"),
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
  testthat::expect_true("age_group" %in% names(forecast_age))
})

test_that("Test get_econ_forecast input handling", {

  # Test with list input (hia_cost$hia_cost)
  test_hia_cost <- data.frame(
    iso3 = c("USA"),
    region_id = c("USA"),
    outcome = c("Deaths"),
    year = c(2019),
    number = c(100),
    cost_mn_currentUSD = c(1.1),
    cost_mn_currentLCU = c(1.1),
    age_group = c("25+"),
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
    outcome = c("Deaths"),
    year = c(2019),
    number = c(100),
    cost_mn_currentUSD = c(1.1),
    cost_mn_currentLCU = c(1.1),
    age_group = c("25+"),
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

test_that("Test use_pop_scaling toggles population scaling on and off", {

  test_hia_cost <- data.frame(
    iso3 = c("USA", "USA", "ZAF"),
    region_id = c("USA", "USA", "ZAF"),
    outcome = c("Deaths", "Asthma.Prev", "Deaths"),
    year = c(2019, 2019, 2019),
    number = c(100, 1000, 50),
    cost_mn_currentUSD = c(1.1, 0.1, 0.5),
    cost_mn_currentLCU = c(1.1, 0.1, 9.3),
    age_group = c("25+", "25+", "25+"),
    double_counted = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  forecast_with <- creahia::get_econ_forecast(
    test_hia_cost,
    forecast_years = c(2020, 2023),
    reference_year = 2019,
    use_pop_scaling = TRUE,
    use_gdp_scaling = FALSE
  )

  forecast_without <- creahia::get_econ_forecast(
    test_hia_cost,
    forecast_years = c(2020, 2023),
    reference_year = 2019,
    use_pop_scaling = FALSE,
    use_gdp_scaling = FALSE
  )

  # Structural equivalence: same rows expanded across years
  testthat::expect_equal(nrow(forecast_with), nrow(forecast_without))
  testthat::expect_setequal(unique(forecast_without$year), c(2019, 2020, 2023))

  # With pop scaling off, every pop_scaling value should be exactly 1
  testthat::expect_true(all(forecast_without$pop_scaling == 1))
  # And forecast-year counts must match the reference-year inputs (no growth applied)
  joined <- dplyr::left_join(
    forecast_without %>% dplyr::select(iso3, outcome, age_group, year, number_forecast = number),
    test_hia_cost %>% dplyr::select(iso3, outcome, age_group, number_ref = number),
    by = c("iso3", "outcome", "age_group")
  )
  testthat::expect_equal(joined$number_forecast, joined$number_ref)

  # With pop scaling on, future-year scaling diverges from 1 for at least one row
  future_with <- forecast_with %>% dplyr::filter(year != 2019)
  testthat::expect_true(any(abs(future_with$pop_scaling - 1) > 1e-6))

  # Reference year is identical in both cases (pop_scaling == 1 by construction)
  ref_with <- forecast_with %>% dplyr::filter(year == 2019) %>% dplyr::arrange(iso3, outcome)
  ref_without <- forecast_without %>% dplyr::filter(year == 2019) %>% dplyr::arrange(iso3, outcome)
  testthat::expect_equal(ref_with$number, ref_without$number)
  testthat::expect_equal(ref_with$cost_mn_currentUSD, ref_without$cost_mn_currentUSD)

  # Combining use_pop_scaling = FALSE with GDP scaling: counts stay flat, costs reflect GDP only
  forecast_gdp_only <- creahia::get_econ_forecast(
    test_hia_cost,
    forecast_years = c(2020, 2023),
    reference_year = 2019,
    use_pop_scaling = FALSE,
    use_gdp_scaling = TRUE,
    discount_rate = 0.03
  )
  testthat::expect_true(all(forecast_gdp_only$pop_scaling == 1))
  testthat::expect_true(any(abs(forecast_gdp_only$gdp_scaling - 1) > 1e-6))
  # number column unaffected by GDP scaling
  joined_gdp <- dplyr::left_join(
    forecast_gdp_only %>% dplyr::select(iso3, outcome, age_group, year, number_forecast = number),
    test_hia_cost %>% dplyr::select(iso3, outcome, age_group, number_ref = number),
    by = c("iso3", "outcome", "age_group")
  )
  testthat::expect_equal(joined_gdp$number_forecast, joined_gdp$number_ref)
})

test_that("Test GDP scaling for several countries", {

  hia_cost <- readRDS(get_test_file(file.path("example_kaz", "hia_cost.RDS")))

  # Rename columns to match new format
  names(hia_cost) <- gsub("Outcome", "outcome", names(hia_cost))
  names(hia_cost) <- gsub("Pollutant", "pollutant", names(hia_cost))
  names(hia_cost) <- gsub("Cause", "cause", names(hia_cost))
  names(hia_cost) <- gsub("AgeGrp", "age_group", names(hia_cost))



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

# ---------------------------------------------------------------------------
# apply_econ_scaling: targeted unit tests
# ---------------------------------------------------------------------------

# Shared minimal input covering fatal (Deaths, YLLs) and non-fatal (YLDs,
# Asthma.Prev) outcomes across two countries and two age groups.
make_apply_input <- function() {
  data.frame(
    iso3 = c("USA", "USA", "USA", "USA", "ZAF"),
    region_id = c("USA", "USA", "USA", "USA", "ZAF"),
    outcome = c("Deaths", "YLLs", "YLDs", "Asthma.Prev", "Deaths"),
    year = 2019,
    number = c(100, 5000, 200, 1000, 50),
    cost_mn_currentUSD = c(1.1, 2.2, 0.3, 0.4, 0.5),
    cost_mn_currentLCU = c(1.1, 2.2, 0.3, 0.4, 9.3),
    age_group = c("25+", "25+", "25+", "0-4", "25+"),
    double_counted = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("apply_econ_scaling: NULL scaling tables expand years and set fatal flag without changing values", {

  hia_cost <- make_apply_input()

  out <- creahia:::apply_econ_scaling(
    hia_cost,
    pop_scaling = NULL,
    gdp_scaling_tbl = NULL,
    reference_year = 2019,
    forecast_years = c(2020, 2025)
  )

  testthat::expect_setequal(unique(out$year), c(2019, 2020, 2025))
  testthat::expect_equal(nrow(out), nrow(hia_cost) * 3)
  testthat::expect_true(all(out$pop_scaling == 1))
  testthat::expect_true(all(out$gdp_scaling == 1))
  testthat::expect_true(all(out$gdp_pc_scaling == 1))

  # fatal derivation: Deaths and YLLs only
  fatal_lookup <- out %>%
    dplyr::distinct(outcome, fatal)
  testthat::expect_equal(
    fatal_lookup$fatal[fatal_lookup$outcome == "Deaths"], TRUE
  )
  testthat::expect_equal(
    fatal_lookup$fatal[fatal_lookup$outcome == "YLLs"], TRUE
  )
  testthat::expect_equal(
    fatal_lookup$fatal[fatal_lookup$outcome == "YLDs"], FALSE
  )
  testthat::expect_equal(
    fatal_lookup$fatal[fatal_lookup$outcome == "Asthma.Prev"], FALSE
  )

  # values flat across years because scaling = 1
  flat <- out %>%
    dplyr::group_by(iso3, outcome, age_group) %>%
    dplyr::summarise(n_unique = dplyr::n_distinct(number),
                     c_unique = dplyr::n_distinct(cost_mn_currentUSD),
                     lcu_unique = dplyr::n_distinct(cost_mn_currentLCU),
                     .groups = "drop")
  testthat::expect_true(all(flat$n_unique == 1))
  testthat::expect_true(all(flat$c_unique == 1))
  testthat::expect_true(all(flat$lcu_unique == 1))
})

test_that("apply_econ_scaling: explicit pop_scaling multiplies counts and costs (incl. LCU)", {

  hia_cost <- make_apply_input()

  pop_scaling <- tidyr::crossing(
    hia_cost %>% dplyr::distinct(iso3, age_group),
    fatal = c(TRUE, FALSE),
    year = c(2019, 2025)
  ) %>%
    dplyr::mutate(pop_scaling = dplyr::case_when(
      year == 2019 ~ 1,
      fatal ~ 1.20,         # death scaling
      TRUE ~ 1.05            # population scaling
    ))

  out <- creahia:::apply_econ_scaling(
    hia_cost,
    pop_scaling = pop_scaling,
    gdp_scaling_tbl = NULL,
    reference_year = 2019,
    forecast_years = 2025
  )

  ref <- hia_cost
  scaled <- out %>% dplyr::filter(year == 2025)

  joined <- scaled %>%
    dplyr::left_join(ref %>% dplyr::select(iso3, outcome, age_group,
                                           number_ref = number,
                                           usd_ref = cost_mn_currentUSD,
                                           lcu_ref = cost_mn_currentLCU),
                     by = c("iso3", "outcome", "age_group"))

  expected_factor <- ifelse(joined$fatal, 1.20, 1.05)
  testthat::expect_equal(joined$number, joined$number_ref * expected_factor)
  testthat::expect_equal(joined$cost_mn_currentUSD, joined$usd_ref * expected_factor)
  testthat::expect_equal(joined$cost_mn_currentLCU, joined$lcu_ref * expected_factor)

  # reference year untouched
  ref_year <- out %>% dplyr::filter(year == 2019) %>% dplyr::arrange(iso3, outcome, age_group)
  ref_sorted <- ref %>% dplyr::arrange(iso3, outcome, age_group)
  testthat::expect_equal(ref_year$number, ref_sorted$number)
  testthat::expect_equal(ref_year$cost_mn_currentUSD, ref_sorted$cost_mn_currentUSD)
})

test_that("apply_econ_scaling: gdp_scaling_tbl scales costs only, leaves counts flat", {

  hia_cost <- make_apply_input()

  gdp_tbl <- dplyr::tibble(
    iso3 = c("USA", "USA", "ZAF", "ZAF"),
    year = c(2019, 2025, 2019, 2025),
    gdp_pc_scaling = c(1, 1.30, 1, 1.50),
    gdp_scaling   = c(1, 1.10, 1, 1.25)   # already discounted
  )

  out <- creahia:::apply_econ_scaling(
    hia_cost,
    pop_scaling = NULL,
    gdp_scaling_tbl = gdp_tbl,
    reference_year = 2019,
    forecast_years = 2025
  )

  scaled <- out %>% dplyr::filter(year == 2025) %>%
    dplyr::left_join(hia_cost %>% dplyr::select(iso3, outcome, age_group,
                                                number_ref = number,
                                                usd_ref = cost_mn_currentUSD,
                                                lcu_ref = cost_mn_currentLCU),
                     by = c("iso3", "outcome", "age_group"))

  expected_gdp <- ifelse(scaled$iso3 == "USA", 1.10, 1.25)
  # counts unchanged (pop_scaling = 1)
  testthat::expect_equal(scaled$number, scaled$number_ref)
  # costs multiplied by gdp_scaling only
  testthat::expect_equal(scaled$cost_mn_currentUSD, scaled$usd_ref * expected_gdp)
  testthat::expect_equal(scaled$cost_mn_currentLCU, scaled$lcu_ref * expected_gdp)
})

test_that("apply_econ_scaling: share_gdp is recomputed from GDP totals when present, dropped otherwise", {

  base <- make_apply_input() %>% dplyr::filter(iso3 == "USA", outcome == "Deaths")

  # Case 1: LCU GDP total present → recompute share_gdp = cost_LCU * 1e6 / GDP_LCU_scaled
  hia_lcu <- base %>%
    dplyr::mutate(GDP.TOT.currLCU = 25e12,
                  share_gdp = NA_real_)

  pop_scaling <- tidyr::crossing(
    iso3 = "USA", age_group = "25+",
    fatal = c(TRUE, FALSE),
    year = c(2019, 2025)
  ) %>%
    dplyr::mutate(pop_scaling = dplyr::case_when(
      year == 2019 ~ 1,
      fatal ~ 1.10,
      TRUE ~ 1.05
    ))
  gdp_tbl <- dplyr::tibble(
    iso3 = c("USA", "USA"),
    year = c(2019, 2025),
    gdp_pc_scaling = c(1, 1.30),
    gdp_scaling = c(1, 1.20)
  )

  out_lcu <- creahia:::apply_econ_scaling(
    hia_lcu,
    pop_scaling = pop_scaling,
    gdp_scaling_tbl = gdp_tbl,
    reference_year = 2019,
    forecast_years = 2025
  )

  row_2025 <- out_lcu %>% dplyr::filter(year == 2025)
  testthat::expect_equal(nrow(row_2025), 1)
  # cost_mn_currentLCU = 1.1 * 1.10 (pop, fatal) * 1.20 (gdp) = 1.452
  # GDP scaled = 25e12 * 1.10 * 1.30 = 35.75e12
  # share_gdp = 1.452e6 / 35.75e12 = 4.061538e-08
  testthat::expect_equal(row_2025$share_gdp, (1.1 * 1.10 * 1.20 * 1e6) / (25e12 * 1.10 * 1.30))

  # Case 2: only stale share_gdp column, no GDP totals → column dropped
  hia_stale <- base %>% dplyr::mutate(share_gdp = 0.001)
  out_drop <- creahia:::apply_econ_scaling(
    hia_stale,
    pop_scaling = NULL,
    gdp_scaling_tbl = NULL,
    reference_year = 2019,
    forecast_years = 2025
  )
  testthat::expect_false("share_gdp" %in% names(out_drop))
})

test_that("apply_econ_scaling: duplicate pop_scaling keys stop with informative error", {

  hia_cost <- make_apply_input() %>% dplyr::filter(iso3 == "USA", outcome == "Deaths")

  dup_pop <- dplyr::tibble(
    iso3 = "USA", age_group = "25+", fatal = TRUE,
    year = c(2019, 2019, 2025),
    pop_scaling = c(1, 1, 1.1)
  )

  testthat::expect_error(
    creahia:::apply_econ_scaling(
      hia_cost,
      pop_scaling = dup_pop,
      gdp_scaling_tbl = NULL,
      reference_year = 2019,
      forecast_years = 2025
    ),
    regexp = "duplicate key"
  )
})

test_that("apply_econ_scaling: missing iso3 in pop_scaling stops with informative error", {

  hia_cost <- make_apply_input()  # USA + ZAF

  pop_scaling_usa_only <- tidyr::crossing(
    iso3 = "USA",
    age_group = c("25+", "0-4"),
    fatal = c(TRUE, FALSE),
    year = c(2019, 2025)
  ) %>% dplyr::mutate(pop_scaling = ifelse(year == 2019, 1, 1.05))

  testthat::expect_error(
    creahia:::apply_econ_scaling(
      hia_cost,
      pop_scaling = pop_scaling_usa_only,
      gdp_scaling_tbl = NULL,
      reference_year = 2019,
      forecast_years = 2025
    ),
    regexp = "Missing population scaling"
  )
})
