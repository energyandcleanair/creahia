test_that("Fusion adult death presets reproduce legacy impacts end to end", {
  config <- adult_mortality_hia_config("fusion")
  expected_counts <- c(
    fusion_adult_deaths_aggregate = 3L,
    fusion_adult_deaths_components = 18L
  )
  fingerprints <- list()

  for (preset in config$presets) {
    results <- run_adult_mortality_hia_parity_fixture("fusion", preset)
    legacy_deaths <- adult_mortality_death_results(
      results$legacy,
      results$causes
    )
    registry_deaths <- adult_mortality_death_results(
      results$registry,
      results$causes
    )

    expect_s3_class(results$legacy, "data.frame")
    expect_s3_class(results$registry, "data.frame")
    expect_equal(nrow(legacy_deaths), expected_counts[[preset]], info = preset)
    expect_equal(nrow(registry_deaths), expected_counts[[preset]], info = preset)
    expect_setequal(unique(registry_deaths$cause), results$causes)
    expect_equal(
      registry_deaths,
      legacy_deaths,
      tolerance = 1e-12,
      info = preset
    )
    expect_true(all(!is.na(registry_deaths$number)), info = preset)
    expect_true(any(registry_deaths$number != 0), info = preset)
    expect_false(any(registry_deaths$double_counted), info = preset)
    expect_true(all(registry_deaths$age_group == AGE_ADULTS), info = preset)

    fingerprints[[preset]] <- adult_mortality_hia_fingerprint(
      registry_deaths,
      preset
    )
  }

  actual <- dplyr::bind_rows(fingerprints)
  expected <- read_adult_mortality_hia_fingerprint("fusion")

  expect_equal(nrow(actual), 7L)
  expect_equal(actual, expected, tolerance = 1e-12)
  expect_setequal(unique(actual$preset), config$presets)
})
