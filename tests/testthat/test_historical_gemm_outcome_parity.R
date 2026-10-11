test_that("internal GEMM YLL and YLD rows reproduce historical PAF and impacts", {
  results <- run_historical_gemm_outcome_parity_fixture()
  policy_hold <- paste(
    "Source, outcome, age, PAF, and impact parity are verified;",
    "historical RR remains not ready pending methodology and readiness review."
  )
  internal_registry <- .load_historical_rr_registry()
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% internal_registry$component_id)

  expect_true(all(internal_registry$reproduction_status == "not_verified"))
  expect_true(all(manifest$reproduction_status == "not_verified"))
  expect_true(all(
    internal_registry$reproduction_blocking_reason == policy_hold
  ))
  expect_true(all(manifest$reproduction_blocking_reason == policy_hold))

  legacy_paf <- results$legacy_paf %>%
    filter_historical_gemm_outcomes() %>%
    arrange_historical_gemm_paf()
  registry_paf <- results$registry_paf %>%
    filter_historical_gemm_outcomes() %>%
    arrange_historical_gemm_paf()

  expect_equal(nrow(legacy_paf), 8L)
  expect_equal(nrow(registry_paf), 8L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  expect_true(all(!is.na(registry_paf$central)))
  expect_true(any(registry_paf$central != 0))

  legacy_impacts <- results$legacy_impacts %>%
    filter_historical_gemm_outcomes() %>%
    arrange_historical_gemm_impacts()
  registry_impacts <- results$registry_impacts %>%
    filter_historical_gemm_outcomes() %>%
    arrange_historical_gemm_impacts()

  expect_equal(nrow(legacy_impacts), 24L)
  expect_equal(nrow(registry_impacts), 24L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  expect_setequal(unique(registry_impacts$outcome), c("YLLs", "YLDs"))
  expect_true(all(!is.na(registry_impacts$number)))
  expect_true(any(registry_impacts$number != 0))
  double_counted <- registry_impacts %>%
    dplyr::filter(estimate == "central", double_counted) %>%
    dplyr::pull(cause)
  expect_setequal(double_counted, c("COPD", "IHD", "LC", "LRI", "Stroke"))

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    results$configuration_id
  )
  expected_fingerprint <-
    read_legacy_historical_configurations_hia_fingerprint() %>%
    dplyr::filter(configuration_id == results$configuration_id) %>%
    filter_historical_gemm_outcomes()

  expect_equal(nrow(actual_fingerprint), 8L)
  expect_equal(nrow(expected_fingerprint), 8L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})
