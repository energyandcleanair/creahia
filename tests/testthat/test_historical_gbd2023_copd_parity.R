test_that("internal GBD 2023 COPD rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_GBD2023,
    CAUSE_COPD,
    "legacy_default_gbd2023_gbd2023_2023"
  )
})
