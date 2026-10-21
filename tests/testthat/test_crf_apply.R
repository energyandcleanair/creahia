test_that("apply_crf_tabular matches legacy PAF helper with one-age tabular RR", {
  crf <- tibble::tibble(
    crf_id = "test_tabular_pm25_ihd_deaths_v1",
    pollutant = "PM25",
    cause = "IHD",
    outcome = "Deaths",
    form = CRF_FORM_TABULAR,
    data_path = "unused.csv"
  )

  fake_rr <- tibble::tibble(
    cause = "IHD",
    age = "25+",
    exposure = c(0, 10, 20, 30, 40),
    low = c(1.00, 1.05, 1.10, 1.15, 1.20),
    central = c(1.00, 1.08, 1.16, 1.24, 1.32),
    high = c(1.00, 1.12, 1.24, 1.36, 1.48)
  )

  fake_epi <- tibble::tibble(
    location_id = 1,
    cause = "IHD",
    measure_name = "Deaths",
    age = "25+",
    estimate = "central",
    val = 100
  )

  conc_base <- c(20, 25, 30)
  conc_perm <- c(15, 20, 25)
  pop <- c(1000, 1500, 2000)

  with_mocked_bindings(
    load_crf_tabular = function(...) fake_rr %>% dplyr::select(-cause),
    get_rr = function(...) fake_rr,
    get_epi_count_long = function(...) fake_epi,
    get_epi_location_id = function(...) 1,
    {
      new_result <- apply_crf_tabular(crf, conc_base, conc_perm, pop, "TEST", "gbd2019")

      # Compare with legacy country_paf_perm function
      legacy_result <- country_paf_perm(
        pm.base = conc_base,
        pm.perm = conc_perm,
        pop = pop,
        region_id = "TEST",
        cause = "IHD",
        measure = "Deaths",
        rr_source = "test",
        epi_version = "gbd2019"
      )

      new_values <- new_result %>%
        dplyr::select(low, central, high) %>%
        unlist() %>%
        unname()

      legacy_values <- legacy_result %>%
        unname()

      # Compare the new and legacy values
      expect_equal(new_values, legacy_values, tolerance = 1e-12)

      # Check that the new result has the expected structure
      expect_named(
        new_result,
        c("pollutant", "cause", "outcome", "region_id", "low", "central", "high")
      )


    }
  )
})

test_that("get_hazard_ratio_tabular returns correct interpolated values",{
  fake_rr <- tibble::tibble(
    cause = "IHD",
    age = "25+",
    exposure = c(0, 10, 20, 30, 40),
    low = c(1.00, 1.05, 1.10, 1.15, 1.20),
    central = c(1.00, 1.08, 1.16, 1.24, 1.32),
    high = c(1.00, 1.12, 1.24, 1.36, 1.48)
  )

  result <- get_hazard_ratio_tabular(
    conc = 25,
    rr = fake_rr %>% dplyr::select(-cause),
    age = "25+"
  )

  expect_equal(unname(result["low"]), 1.125)
  expect_equal(unname(result["central"]), 1.2)
  expect_equal(unname(result["high"]), 1.3)
})

test_that("calculate_log_linear_paf matches closed-form AF", {
  # When actual and reference concentration changes match, PAF should be 1 - 1/RR.
  expect_equal(
    calculate_log_linear_paf(rr = 1.037, effective_conc_change = 10, conc_change_ref = 10),
    1 - 1 / 1.037,
    tolerance = 1e-12
  )
})

test_that("apply_crf_log_linear computes PAF from weighted concentration delta",{
  crf <- tibble::tibble(
    crf_id = "legacy_no2_ncdlri_deaths_v1",
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    form = CRF_FORM_LOG_LINEAR,
    rr_low = 1.021,
    rr_central = 1.037,
    rr_high = 1.08,
    conc_change = 10,
    counterfact = 20,
    units_multiplier = 1
  )

  conc_base <- c(20, 20, 20)
  conc_perm <- c(30, 30, 30)
  pop <- c(100, 200, 300)

  result <- apply_crf_log_linear(
    crf = crf,
    conc_base = conc_base,
    conc_perm = conc_perm,
    pop = pop,
    region_id = "TEST"
  )

  expect_equal(result$central, 1 - 1 / 1.037, tolerance = 1e-12)
  expect_named(
    result,
    c("pollutant", "cause", "outcome", "region_id", "low", "central", "high")
  )
})

test_that("apply_crf_log_linear correctly routes parameters to internal PAF calculators", {
  crf <- tibble::tibble(
    crf_id = "legacy_no2_ncdlri_deaths_v1",
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    form = CRF_FORM_LOG_LINEAR,
    rr_low = 1.021,
    rr_central = 1.037,
    rr_high = 1.08,
    conc_change = 10,
    counterfact = 20,
    units_multiplier = 1
  )

  conc_base <- c(20, 20, 20)
  conc_perm <- c(30, 30, 30)
  pop <- c(100, 200, 300)

  with_mocked_bindings(
    calculate_effective_conc_change = function(...) 10,
    calculate_log_linear_paf = function(rr, effective_conc_change, conc_change_ref) {
      # Check that the effective_conc_change is being passed correctly to the log-linear PAF calculator
      expect_equal(effective_conc_change, 10)
      return(1 - 1 / rr)
    },
    {
      new_result <- apply_crf_log_linear(crf, conc_base, conc_perm, pop, "TEST")

      # The central PAF should be 1 - 1/RR_central based on our mock calculate_log_linear_paf
      expected_central <- 1 - 1 / crf$rr_central
      
      expect_equal(new_result$central, expected_central, tolerance = 1e-12)
      expect_named(
        new_result,
        c("pollutant", "cause", "outcome", "region_id", "low", "central", "high")
      )
    }
  )
})

test_that("apply_crf_log_linear produces numerical results consistent with legacy HIA helpers", {
  conc_region <- data.frame(
    conc_baseline_no2 = c(10, 12, 15),
    conc_scenario_no2 = c(8, 10, 12),
    pop = c(1000, 1500, 2000)
  )

  conc_map <- list(
    scenario1 = list(
      BGD = conc_region
    )
  )

  regions <- data.frame(
    region_id = "BGD",
    region_name = "Bangladesh",
    country_id = "BGD"
  )

  legacy_crf <- tibble::tibble(
    pollutant = "NO2",
    cause = "Asthma.1to18",
    outcome = "AsthmaIncidence",
    counterfact = 0,
    conc_change = 10,
    units_multiplier = 1,
    rr_low = 1.01,
    rr_central = 1.05,
    rr_high = 1.09,
    double_counted = FALSE
  )

  registry_crf <- legacy_crf %>%
    dplyr::mutate(
      crf_id = "test_no2_asthma_log_linear_v1",
      form = CRF_FORM_LOG_LINEAR
    )

  legacy_result <- compute_hia_paf_crfs(
    species = "no2",
    conc_map = conc_map,
    regions = regions,
    crfs = legacy_crf
  )$scenario1 %>%
    dplyr::select(low, central, high) %>%
    unlist() %>%
    unname()

  new_result <- apply_crf_log_linear(
    crf = registry_crf,
    conc_base = conc_region$conc_baseline_no2,
    conc_perm = conc_region$conc_scenario_no2,
    pop = conc_region$pop,
    region_id = "BGD"
  ) %>%
    dplyr::select(low, central, high) %>%
    unlist() %>%
    unname()

  expect_equal(new_result, legacy_result, tolerance = 1e-12)
  expect_true(all(!is.na(new_result)))
  expect_true(all(!is.na(legacy_result)))
  expect_true(any(new_result != 0))
  expect_true(any(legacy_result != 0))
  
})