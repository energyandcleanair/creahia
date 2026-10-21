test_that("GEMM and GBD 2021 reproduce through the activated Registry route", {
  results <- run_historical_contract_parity_fixture(
    configuration_id = "legacy_default_gemm_gbd2021_2020",
    calc_causes = "GEMM and GBD",
    activate_route = TRUE
  )

  expect_historical_contract_parity(
    results = results,
    expected_rr_source = c(RR_GEMM, RR_ORIGINAL),
    expected_paf_rows = 33L
  )
})
