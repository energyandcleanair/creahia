test_that("the Krewski South Africa CRFs reproduce as one Registry Set", {
  results <- run_historical_crf_profile_parity_fixture(
    profile_version = "Krewski-South Africa",
    selection_id = "legacy_profile_krewski_south_africa"
  )

  expect_s3_class(results$registry_crfs, "creahia_historical_crf_set")
  expect_equal(nrow(results$registry_crfs), 21L)

  legacy_paf <- arrange_historical_crf_profile_paf(results$legacy_paf)
  registry_paf <- arrange_historical_crf_profile_paf(results$registry_paf)
  expect_equal(nrow(legacy_paf), 21L)
  expect_equal(nrow(registry_paf), 21L)
  expect_equal(registry_paf, legacy_paf, tolerance = 1e-12)
  expect_true(all(!is.na(registry_paf$central)))
  expect_true(any(registry_paf$central != 0))

  legacy_impacts <- arrange_historical_crf_profile_impacts(
    results$legacy_impacts
  )
  registry_impacts <- arrange_historical_crf_profile_impacts(
    results$registry_impacts
  )
  expect_equal(nrow(legacy_impacts), 63L)
  expect_equal(nrow(registry_impacts), 63L)
  expect_equal(registry_impacts, legacy_impacts, tolerance = 1e-12)
  expect_setequal(
    unique(registry_impacts$estimate),
    c("low", "central", "high")
  )
  expect_true(all(!is.na(registry_impacts$number)))
  expect_true(any(registry_impacts$number != 0))

  actual_fingerprint <- legacy_hia_fingerprint(
    registry_impacts,
    configuration_id = "Krewski-South Africa"
  )
  expected_fingerprint <- read_legacy_crf_profiles_hia_fingerprint() %>%
    dplyr::filter(.data$configuration_id == "Krewski-South Africa")

  expect_equal(nrow(actual_fingerprint), 21L)
  expect_equal(nrow(expected_fingerprint), 21L)
  expect_equal(actual_fingerprint, expected_fingerprint, tolerance = 1e-12)
})


test_that("Krewski reproduction remains separate from science review", {
  krewski_ids <- sprintf("historical_l%02d_v1", 24:25)
  registry <- .load_historical_crf_registry() %>%
    dplyr::filter(.data$crf_id %in% krewski_ids)
  manifest <- .load_hia_component_manifest() %>%
    dplyr::filter(
      .data$component_type == "crf",
      .data$selection_id == "legacy_profile_krewski_south_africa"
    )

  expect_equal(nrow(registry), 2L)
  expect_true(all(registry$reproduction_status == "parity_verified"))
  expect_true(all(is.na(registry$reproduction_blocking_reason)))
  expect_true(all(registry$scientific_review_status == "not_reviewed"))
  expect_true(all(!is.na(registry$scientific_review_reason)))

  expect_equal(nrow(manifest), 21L)
  expect_true(all(manifest$reproduction_status == "parity_verified"))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(manifest$scientific_review_status == "not_reviewed"))
})
