test_that("GBD 2023 RR reproduces EPI and population scaling", {
  configuration_id <- "legacy_default_gbd2023_gbd2023_2023"
  results <- run_historical_contract_parity_fixture(
    configuration_id = configuration_id,
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
    "gbd2023"
  )

  gbd2021_specification <- historical_contract_specification(
    configuration_id = "legacy_default_gbd2023_gbd2021_2021"
  )
  expect_setequal(
    results$specification$components$rr$component_id,
    gbd2021_specification$components$rr$component_id
  )

  fingerprints <- read_legacy_historical_configurations_hia_fingerprint()
  gbd2021 <- fingerprints %>%
    dplyr::filter(
      configuration_id == "legacy_default_gbd2023_gbd2021_2021"
    ) %>%
    dplyr::select(-configuration_id, number_2021 = number)
  gbd2023 <- fingerprints %>%
    dplyr::filter(configuration_id == .env$configuration_id) %>%
    dplyr::select(-configuration_id, number_2023 = number)
  comparison <- dplyr::inner_join(
    gbd2021,
    gbd2023,
    by = setdiff(names(gbd2021), "number_2021")
  )
  expected_ratio <- (1.4 / 1.2) * (1.3 / 1.1)

  expect_equal(nrow(comparison), 34L)
  expect_equal(
    comparison$number_2023,
    comparison$number_2021 * expected_ratio,
    tolerance = 1e-12
  )
})
