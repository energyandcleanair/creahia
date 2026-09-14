test_that("historical fingerprint configurations preserve the current legacy test matrix", {
  configurations <- load_historical_fingerprint_configurations()

  required_columns <- c(
    "mapping_id",
    "legacy_crfs_version",
    "legacy_calc_causes",
    "legacy_gbd_causes",
    "legacy_rr_sources",
    "epi_version",
    "pop_year",
    "target_selection_id",
    "mapping_status",
    "blocking_reason"
  )

  expect_named(configurations, required_columns)
  expect_equal(nrow(configurations), 9)
  expect_equal(n_distinct(configurations$mapping_id), nrow(configurations))
  expect_true(all(configurations$legacy_crfs_version == "default"))

  expected <- tibble::tribble(
    ~mapping_id, ~legacy_calc_causes, ~legacy_rr_sources, ~epi_version, ~pop_year,
    "legacy_default_gbd2019_only_2020", "GBD only", NA_character_, "gbd2019", 2020L,
    "legacy_default_gemm_gbd2019_2020", "GEMM and GBD", NA_character_, "gbd2019", 2020L,
    "legacy_default_gbd2021_only_2020", "GBD only", NA_character_, "gbd2021", 2020L,
    "legacy_default_gemm_gbd2021_2020", "GEMM and GBD", NA_character_, "gbd2021", 2020L,
    "legacy_default_gemm_gbd2021_2021", "GEMM and GBD", NA_character_, "gbd2021", 2021L,
    "legacy_default_fusion_gbd2021_2021", NA_character_, "fusion", "gbd2021", 2021L,
    "legacy_default_gbd2023_gbd2021_2021", NA_character_, "gbd2023", "gbd2021", 2021L,
    "legacy_default_fusion_gbd2023_2023", NA_character_, "fusion", "gbd2023", 2023L,
    "legacy_default_gbd2023_gbd2023_2023", NA_character_, "gbd2023", "gbd2023", 2023L
  )

  expect_equal(
    configurations %>%
      dplyr::select(names(expected)) %>%
      dplyr::arrange(mapping_id),
    expected %>% dplyr::arrange(mapping_id)
  )
})

test_that("incomplete historical mappings cannot be mistaken for Registry targets", {
  configurations <- load_historical_fingerprint_configurations()

  incomplete <- configurations %>%
    dplyr::filter(mapping_status == "legacy_defined")

  expect_true(nrow(incomplete) > 0)
  expect_true(all(is.na(incomplete$target_selection_id)))
  expect_true(all(!is.na(incomplete$blocking_reason)))
  expect_false(any(incomplete$target_selection_id == "experimental_default", na.rm = TRUE))
})
