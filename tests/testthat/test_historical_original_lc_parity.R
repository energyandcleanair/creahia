test_that("internal original LC rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_ORIGINAL,
    "LC",
    "legacy_default_gbd2019_only_2020"
  )
})
