test_that("GBD 2021 Fusion reproduces as one routed Registry Set", {
  results <- run_historical_contract_parity_fixture(
    configuration_id = "legacy_default_fusion_gbd2021_2021",
    activate_route = TRUE
  )

  expect_historical_contract_parity(
    results = results,
    expected_rr_source = RR_FUSION,
    expected_paf_rows = 33L,
    expected_warnings = character()
  )

  rr_components <- results$specification$components$rr
  expect_equal(nrow(rr_components), 18L)
  expect_setequal(
    unique(rr_components$outcome),
    c(MEASURE_DEATHS, MEASURE_YLLS, MEASURE_YLDS)
  )
  expect_true(CAUSE_LRICHILD %in% rr_components$cause)
})
