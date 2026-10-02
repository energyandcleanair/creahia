test_that("GBD 2019 death preset reproduces legacy impacts end to end", {
  results <- run_gbd2019_hia_parity_fixture()
  legacy_deaths <- results$legacy %>%
    dplyr::filter(outcome %in% c(MEASURE_DEATHS, "Deaths.child")) %>%
    arrange_gbd2019_death_results()
  registry_deaths <- arrange_gbd2019_death_results(results$registry)

  expect_s3_class(results$legacy, "data.frame")
  expect_s3_class(results$registry, "data.frame")
  expect_equal(nrow(legacy_deaths), 21L)
  expect_equal(nrow(registry_deaths), 21L)
  expect_setequal(unique(registry_deaths$cause), results$death_causes)
  expect_equal(registry_deaths, legacy_deaths, tolerance = 1e-12)
  expect_true(all(!is.na(registry_deaths$number)))
  expect_true(any(registry_deaths$number != 0))
  expect_false(any(registry_deaths$double_counted))

  child_lri <- registry_deaths %>%
    dplyr::filter(cause == CAUSE_LRICHILD)
  expect_equal(nrow(child_lri), 3L)
  expect_true(all(child_lri$outcome == "Deaths.child"))
  expect_true(all(child_lri$age_group == "0-4"))
})

test_that("GBD 2019 death preset matches its fixed HIA fingerprint", {
  results <- run_gbd2019_hia_parity_fixture()
  actual <- gbd2019_deaths_hia_fingerprint(results$registry)
  expected <- readr::read_csv(
    test_path("fixtures", "gbd2019_deaths_hia_fingerprint.csv"),
    show_col_types = FALSE
  ) %>%
    tibble::as_tibble()

  expect_equal(nrow(actual), 7L)
  expect_equal(actual, expected, tolerance = 1e-12)
  expect_true(all(!is.na(actual$number)))
  expect_true(any(actual$number != 0))
})
