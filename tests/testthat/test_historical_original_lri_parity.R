test_that("internal original LRI rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_ORIGINAL,
    "LRI",
    "legacy_default_gbd2019_only_2020"
  )
})
