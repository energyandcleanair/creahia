test_that("internal GBD 2023 LC rows reproduce PAF and impacts", {
  expect_historical_rr_parity(
    RR_GBD2023,
    "LC",
    "legacy_default_gbd2023_gbd2023_2023"
  )
})
