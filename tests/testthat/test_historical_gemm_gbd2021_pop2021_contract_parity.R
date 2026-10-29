test_that("GBD 2021 GEMM and GBD preserve 2021 population scaling", {
  configuration_id <- "legacy_default_gemm_gbd2021_2021"
  results <- run_historical_contract_parity_fixture(
    configuration_id = configuration_id,
    calc_causes = "GEMM and GBD",
    activate_route = TRUE
  )

  expect_historical_contract_parity(
    results = results,
    expected_rr_source = c(RR_GEMM, RR_ORIGINAL),
    expected_paf_rows = 33L
  )

  fingerprints <- read_legacy_historical_configurations_hia_fingerprint()
  pop2020 <- fingerprints %>%
    dplyr::filter(
      configuration_id == "legacy_default_gemm_gbd2021_2020"
    ) %>%
    dplyr::select(-configuration_id, number_2020 = number)
  pop2021 <- fingerprints %>%
    dplyr::filter(configuration_id == .env$configuration_id) %>%
    dplyr::select(-configuration_id, number_2021 = number)
  comparison <- dplyr::inner_join(
    pop2020,
    pop2021,
    by = setdiff(names(pop2020), "number_2020")
  )

  expect_equal(nrow(comparison), 33L)
  expect_equal(
    comparison$number_2021,
    comparison$number_2020 * 1.1,
    tolerance = 1e-12
  )
})
