test_that("GBD 2023 RR reproduces with GBD 2021 EPI and 2021 population", {
  results <- run_historical_contract_parity_fixture(
    configuration_id = "legacy_default_gbd2023_gbd2021_2021",
    activate_route = TRUE
  )

  expect_historical_contract_parity(
    results = results,
    expected_rr_source = RR_GBD2023,
    expected_paf_rows = 34L,
    expected_warnings = character()
  )

  expect_identical(
    results$specification$components$epi$selection_id,
    "gbd2021"
  )

  rr_components <- results$specification$components$rr %>%
    dplyr::transmute(cause, outcome)
  expected_components <- tibble::tribble(
    ~cause, ~outcome,
    CAUSE_COPD, MEASURE_DEATHS,
    CAUSE_COPD, MEASURE_YLLS,
    CAUSE_COPD, MEASURE_YLDS,
    CAUSE_DEMENTIA, MEASURE_DEATHS,
    CAUSE_DEMENTIA, MEASURE_YLLS,
    CAUSE_DEMENTIA, MEASURE_YLDS,
    CAUSE_DIABETES, MEASURE_DEATHS,
    CAUSE_DIABETES, MEASURE_YLDS,
    CAUSE_IHD, MEASURE_DEATHS,
    CAUSE_IHD, MEASURE_YLLS,
    CAUSE_LUNGCANCER, MEASURE_DEATHS,
    CAUSE_LUNGCANCER, MEASURE_YLLS,
    CAUSE_LRI, MEASURE_DEATHS,
    CAUSE_LRI, MEASURE_YLLS,
    CAUSE_LRICHILD, MEASURE_DEATHS,
    CAUSE_LRICHILD, MEASURE_YLLS,
    CAUSE_STROKE, MEASURE_DEATHS,
    CAUSE_STROKE, MEASURE_YLLS,
    CAUSE_STROKE, MEASURE_YLDS
  )

  expect_equal(
    rr_components %>% dplyr::arrange(cause, outcome),
    expected_components %>% dplyr::arrange(cause, outcome)
  )
})
