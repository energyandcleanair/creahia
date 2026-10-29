test_that("internal GBD 2023 Dementia rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_GBD2023,
    CAUSE_DEMENTIA,
    "legacy_default_gbd2023_gbd2023_2023"
  )
})
