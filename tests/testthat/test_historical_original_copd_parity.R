test_that("internal original COPD rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_ORIGINAL,
    "COPD",
    "legacy_default_gbd2019_only_2020"
  )
})
