legacy_crf_migration_key <- function(data) {
  data %>%
    dplyr::mutate(
      pollutant = gsub("PM2\\.5", "PM25", pollutant),
      migration_slot = paste(
        pollutant,
        cause,
        outcome,
        sep = "|"
      )
    )
}

test_that("legacy migration inventory accounts for all historical source rows", {
  inventory <- readr::read_csv(
    testthat::test_path("fixtures", "legacy_crf_migration_manifest.csv"),
    col_types = readr::cols()
  )

  expect_equal(nrow(inventory), 25)
  expect_equal(length(unique(inventory$manifest_id)), 25)

  expected_profile_counts <- inventory %>%
    tidyr::separate_rows(profiles, sep = ";") %>%
    dplyr::count(profiles, name = "n_rows") %>%
    tibble::deframe()

  expect_equal(
    expected_profile_counts[c("historical_default", "c40", "krewski_south_africa")],
    c(historical_default = 15, c40 = 19, krewski_south_africa = 21)
  )
  expect_equal(sum(expected_profile_counts), 55)

  actual <- dplyr::bind_rows(
    get_crfs("default") %>% dplyr::mutate(profile = "historical_default"),
    get_crfs("C40") %>% dplyr::mutate(profile = "c40"),
    get_crfs("Krewski-South Africa") %>% dplyr::mutate(profile = "krewski_south_africa")
  ) %>%
    legacy_crf_migration_key() %>%
    dplyr::count(profile, migration_slot, name = "n_source_rows")

  expected <- inventory %>%
    tidyr::separate_rows(profiles, sep = ";") %>%
    dplyr::mutate(
      pollutant = gsub("PM2\\.5", "PM25", pollutant),
      migration_slot = paste(pollutant, cause, outcome, sep = "|")
    ) %>%
    dplyr::count(profiles, migration_slot, name = "n_manifest_rows")

  expect_equal(
    actual %>%
      dplyr::rename(profiles = profile, n_manifest_rows = n_source_rows) %>%
      dplyr::arrange(profiles, migration_slot),
    expected %>% dplyr::arrange(profiles, migration_slot)
  )
})
