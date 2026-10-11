test_that("the first historical contract reproduces as one Registry Set", {
  results <- run_historical_first_contract_parity_fixture()
  registry_crfs <- results$registry_crfs
  historical_crf_ids <- .load_historical_crf_registry()$crf_id
  internal_rr_ids <- .load_historical_rr_registry()$crf_id
  public_crf_ids <- load_crf_registry()$crf_id

  expect_s3_class(registry_crfs, "creahia_crf_set")
  expect_equal(nrow(registry_crfs), 33L)
  expect_equal(sum(registry_crfs$crf_id %in% historical_crf_ids), 15L)
  expect_equal(sum(registry_crfs$crf_id %in% internal_rr_ids), 12L)
  expect_equal(sum(registry_crfs$crf_id %in% public_crf_ids), 6L)
  expect_equal(dplyr::n_distinct(registry_crfs$crf_id), 33L)

  expected_warnings <- c(
    "PAF estimates are not properly ordered or of the same sign for Diabetes Deaths",
    "PAF estimates are not properly ordered or of the same sign for Diabetes YLDs"
  )
  observed_warning_sets <- list(
    legacy_paf = results$legacy_paf$warnings,
    registry_paf = results$registry_paf$warnings,
    legacy_impacts = results$legacy_impacts$warnings,
    registry_impacts = results$registry_impacts$warnings
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

  expect_equal(nrow(legacy_paf), 33L)
  expect_equal(nrow(registry_paf), 33L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  expect_true(all(!is.na(registry_paf$central)))
  expect_true(any(registry_paf$central != 0))

  legacy_impacts <- results$legacy_impacts$value %>%
    arrange_historical_first_contract_impacts()
  registry_impacts <- results$registry_impacts$value %>%
    arrange_historical_first_contract_impacts()

  expect_equal(nrow(legacy_impacts), 99L)
  expect_equal(nrow(registry_impacts), 99L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  expect_true(all(!is.na(registry_impacts$number)))
  expect_true(any(registry_impacts$number != 0))

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    results$configuration_id
  )
  expected_fingerprint <-
    read_legacy_historical_configurations_hia_fingerprint() %>%
    dplyr::filter(configuration_id == results$configuration_id)

  expect_equal(nrow(actual_fingerprint), 33L)
  expect_equal(nrow(expected_fingerprint), 33L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})
