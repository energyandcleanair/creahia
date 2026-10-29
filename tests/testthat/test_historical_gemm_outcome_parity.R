test_that("promoted GEMM YLL and YLD rows reproduce historical PAF and impacts", {
  results <- run_historical_gemm_outcome_parity_fixture()
  manifest <- promoted_rr_manifest(RR_GEMM, child = FALSE)

  expect_gt(nrow(manifest), 0)
  expect_true(all(
    manifest$public_registry_crf_ids %in% load_crf_registry()$crf_id
  ))

  expect_true(all(manifest$reproduction_status == "parity_verified"))
  expect_true(all(is.na(manifest$reproduction_blocking_reason)))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))

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
