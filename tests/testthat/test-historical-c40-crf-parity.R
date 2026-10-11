test_that("the C40 historical CRFs reproduce as one Registry Set", {
  results <- run_historical_crf_profile_parity_fixture(
    profile_version = "C40",
    selection_id = "legacy_profile_c40"
  )

  expect_s3_class(results$registry_crfs, "creahia_historical_crf_set")
  expect_equal(nrow(results$registry_crfs), 19L)

  legacy_paf <- arrange_historical_crf_profile_paf(results$legacy_paf)
  registry_paf <- arrange_historical_crf_profile_paf(results$registry_paf)
  expect_equal(nrow(legacy_paf), 19L)
  expect_equal(nrow(registry_paf), 19L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  expect_true(all(!is.na(registry_paf$central)))
  expect_true(any(registry_paf$central != 0))

  legacy_impacts <- arrange_historical_crf_profile_impacts(
    results$legacy_impacts
  )
  registry_impacts <- arrange_historical_crf_profile_impacts(
    results$registry_impacts
  )
  expect_equal(nrow(legacy_impacts), 57L)
  expect_equal(nrow(registry_impacts), 57L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  expect_true(all(!is.na(registry_impacts$number)))
  expect_true(any(registry_impacts$number != 0))

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    configuration_id = "C40"
  )
  expected_fingerprint <- read_legacy_crf_profiles_hia_fingerprint() %>%
    dplyr::filter(.data$configuration_id == "C40")

  expect_equal(nrow(actual_fingerprint), 19L)
  expect_equal(nrow(expected_fingerprint), 19L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})


test_that("C40 reproduction evidence remains separate from science review", {
  c40_ids <- sprintf("historical_l%02d_v1", 16:23)
  registry <- .load_historical_crf_registry() %>%
    dplyr::filter(.data$crf_id %in% c40_ids)
  manifest <- .load_hia_component_manifest() %>%
    dplyr::filter(
      .data$component_type == "crf",
      .data$selection_id == "legacy_profile_c40"
    )

  expect_equal(nrow(registry), 8L)
  expect_true(all(registry$reproduction_status == "parity_verified"))
  expect_true(all(is.na(registry$reproduction_blocking_reason)))
  expect_true(all(registry$scientific_review_status == "not_reviewed"))
  expect_true(all(!is.na(registry$scientific_review_reason)))

  expect_equal(nrow(manifest), 19L)
  expect_true(all(manifest$reproduction_status == "parity_verified"))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(manifest$scientific_review_status == "not_reviewed"))
})
