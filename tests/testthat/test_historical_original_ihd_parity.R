test_that("internal original IHD rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_ORIGINAL,
    "IHD",
    "legacy_default_gbd2019_only_2020"
  )
})
