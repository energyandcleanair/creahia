test_that("GBD 2019 only reproduces through the activated Registry route", {
  configuration_id <- "legacy_default_gbd2019_only_2020"
  results <- run_historical_contract_parity_fixture(
    configuration_id = configuration_id,
    calc_causes = "GBD only",
    activate_route = TRUE
  )

  expect_historical_contract_parity(
    results = results,
    expected_rr_source = RR_ORIGINAL,
    expected_paf_rows = 31L
  )
})
