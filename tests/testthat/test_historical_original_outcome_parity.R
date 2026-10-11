test_that("internal original RR rows reproduce warnings, PAF, and impacts", {
  results <- run_historical_original_outcome_parity_fixture()
  internal_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_ORIGINAL)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% internal_registry$component_id)

  expect_true(all(internal_registry$reproduction_status == "parity_verified"))
  expect_true(all(manifest$reproduction_status == "parity_verified"))
  expect_true(all(is.na(internal_registry$reproduction_blocking_reason)))
  expect_true(all(is.na(manifest$reproduction_blocking_reason)))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))

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
    filter_historical_original_paf() %>%
    arrange_historical_original_paf()
  registry_paf <- results$registry_paf$value %>%
    filter_historical_original_paf() %>%
    arrange_historical_original_paf()

  expect_equal(nrow(legacy_paf), 4L)
  expect_equal(nrow(registry_paf), 4L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  expect_true(all(!is.na(registry_paf$central)))
  expect_true(any(registry_paf$central != 0))

  legacy_impacts <- results$legacy_impacts$value %>%
    filter_historical_original_impacts() %>%
    arrange_historical_original_impacts()
  registry_impacts <- results$registry_impacts$value %>%
    filter_historical_original_impacts() %>%
    arrange_historical_original_impacts()

  expect_equal(nrow(legacy_impacts), 12L)
  expect_equal(nrow(registry_impacts), 12L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  expect_true(all(!is.na(registry_impacts$number)))
  expect_true(any(registry_impacts$number != 0))

  central_impacts <- registry_impacts %>%
    dplyr::filter(estimate == "central")
  expect_true(all(
    central_impacts$age_group[central_impacts$cause == "Diabetes"] == "25+"
  ))
  expect_equal(
    central_impacts$outcome[central_impacts$cause == CAUSE_LRICHILD],
    c("Deaths.child", "YLLs")
  )
  expect_true(all(
    central_impacts$age_group[central_impacts$cause == CAUSE_LRICHILD] == "0-4"
  ))
  expect_equal(
    central_impacts$double_counted,
    c(TRUE, FALSE, FALSE, FALSE)
  )

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    results$configuration_id
  )
  expected_fingerprint <-
    read_legacy_historical_configurations_hia_fingerprint() %>%
    dplyr::filter(configuration_id == results$configuration_id) %>%
    filter_historical_original_impacts()

  expect_equal(nrow(actual_fingerprint), 4L)
  expect_equal(nrow(expected_fingerprint), 4L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})
