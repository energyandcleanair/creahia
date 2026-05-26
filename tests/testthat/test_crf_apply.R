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