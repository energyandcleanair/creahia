historical_fingerprint_configuration_path <- function() {
  testthat::test_path("fixtures", "historical_fingerprint_configurations.csv")
}

load_historical_fingerprint_configurations <- function() {
  readr::read_csv(
    historical_fingerprint_configuration_path(),
    col_types = readr::cols(
      mapping_id = readr::col_character(),
      legacy_crfs_version = readr::col_character(),
      legacy_calc_causes = readr::col_character(),
      legacy_gbd_causes = readr::col_character(),
      legacy_rr_sources = readr::col_character(),
      epi_version = readr::col_character(),
      pop_year = readr::col_integer(),
      target_selection_id = readr::col_character(),
      mapping_status = readr::col_character(),
      blocking_reason = readr::col_character()
    )
  )
}
