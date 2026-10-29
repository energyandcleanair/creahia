test_that("promoted GEMM child LRI rows reproduce PAF and impacts", {
  results <- run_historical_gemm_child_parity_fixture()
  manifest <- promoted_rr_manifest(RR_GEMM, child = TRUE)

  expect_gt(nrow(manifest), 0)
  expect_true(all(
    manifest$public_registry_crf_ids %in% load_crf_registry()$crf_id
  ))

  expect_equal(nrow(manifest), 2L)
  expect_true(all(manifest$reproduction_status == "parity_verified"))
  expect_true(all(is.na(manifest$reproduction_blocking_reason)))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))

  legacy_paf <- results$legacy_paf %>%
    filter_historical_gemm_child_paf() %>%
    arrange_historical_gemm_child_paf()
  registry_paf <- results$registry_paf %>%
    filter_historical_gemm_child_paf() %>%
    arrange_historical_gemm_child_paf()

  expect_equal(nrow(legacy_paf), 2L)
  expect_equal(nrow(registry_paf), 2L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  expect_true(all(!is.na(registry_paf$central)))
  expect_true(any(registry_paf$central != 0))

  legacy_impacts <- results$legacy_impacts %>%
    filter_historical_gemm_child_impacts() %>%
    arrange_historical_gemm_child_impacts()
  registry_impacts <- results$registry_impacts %>%
    filter_historical_gemm_child_impacts() %>%
    arrange_historical_gemm_child_impacts()

  expect_equal(nrow(legacy_impacts), 6L)
  expect_equal(nrow(registry_impacts), 6L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  expect_true(all(!is.na(registry_impacts$number)))
  expect_true(any(registry_impacts$number != 0))

  central_impacts <- registry_impacts %>%
    dplyr::filter(estimate == "central")
  expect_setequal(central_impacts$outcome, c("Deaths.child", "YLLs"))
  expect_true(all(central_impacts$age_group == "0-4"))
  expect_false(any(central_impacts$double_counted))

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    results$configuration_id
  )
  expected_fingerprint <- read_legacy_actual_default_hia_fingerprint() %>%
    filter_historical_gemm_child_impacts()

  expect_equal(nrow(actual_fingerprint), 2L)
  expect_equal(nrow(expected_fingerprint), 2L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})
