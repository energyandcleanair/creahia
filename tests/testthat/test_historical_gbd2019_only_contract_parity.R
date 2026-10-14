test_that("GBD 2019 only reproduces through the activated Registry route", {
  configuration_id <- "legacy_default_gbd2019_only_2020"
  results <- run_historical_contract_parity_fixture(
    configuration_id = configuration_id,
    calc_causes = "GBD only",
    activate_route = TRUE
  )
  registry_set <- results$registry_crfs
  internal_rr <- .load_historical_rr_registry() %>%
    dplyr::filter(crf_id %in% registry_set$crf_id)

  expect_true(results$specification$readiness$ready)
  expect_s3_class(registry_set, "creahia_crf_set")
  expect_equal(nrow(registry_set), 31L)
  expect_equal(nrow(internal_rr), 16L)
  expect_setequal(unique(internal_rr$source), RR_ORIGINAL)
  expect_false(any(internal_rr$source == RR_GEMM))

  expected_warnings <- c(
    "PAF estimates are not properly ordered or of the same sign for Diabetes Deaths",
    "PAF estimates are not properly ordered or of the same sign for Diabetes YLDs"
  )
  observed_warning_sets <- list(
    legacy_paf = results$legacy_paf$warnings,
    registry_paf = results$registry_paf$warnings,
    legacy_impacts = results$legacy_impacts$warnings,
    registry_impacts = results$registry_impacts$warnings,
    activated_impacts = results$activated_impacts$warnings
  )
  for (warning_name in names(observed_warning_sets)) {
    expect_equal(
      sort(observed_warning_sets[[warning_name]]),
      sort(expected_warnings),
      info = warning_name
    )
  }

  legacy_paf <- results$legacy_paf$value %>%
    arrange_historical_first_contract_paf()
  registry_paf <- results$registry_paf$value %>%
    arrange_historical_first_contract_paf()
  expect_equal(nrow(legacy_paf), 31L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)

  legacy_impacts <- results$legacy_impacts$value %>%
    arrange_historical_first_contract_impacts()
  registry_impacts <- results$registry_impacts$value %>%
    arrange_historical_first_contract_impacts()
  activated_impacts <- results$activated_impacts$value %>%
    arrange_historical_first_contract_impacts()
  expect_equal(nrow(legacy_impacts), 93L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_equal(activated_impacts, legacy_impacts, tolerance = 1e-12)

  actual_fingerprint <- legacy_hia_fingerprint(
    activated_impacts,
    configuration_id
  )
  expected_fingerprint <-
    read_legacy_historical_configurations_hia_fingerprint() %>%
    dplyr::filter(configuration_id == .env$configuration_id)

  expect_equal(nrow(actual_fingerprint), 31L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})
