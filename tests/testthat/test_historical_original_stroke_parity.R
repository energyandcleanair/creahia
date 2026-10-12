test_that("internal original Stroke rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_ORIGINAL,
    "Stroke",
    "legacy_default_gbd2019_only_2020"
  )
})
