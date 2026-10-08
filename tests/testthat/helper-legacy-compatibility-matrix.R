legacy_compatibility_matrix_path <- function() {
  testthat::test_path("fixtures", "legacy_compatibility_matrix.csv")
}

load_legacy_compatibility_matrix <- function() {
  readr::read_csv(
    legacy_compatibility_matrix_path(),
    col_types = readr::cols(
      compatibility_id = readr::col_character(),
      historical_mapping_id = readr::col_character(),
      entry_point = readr::col_character(),
      method = readr::col_character(),
      legacy_crfs_version = readr::col_character(),
      legacy_calc_causes = readr::col_character(),
      legacy_gbd_causes = readr::col_character(),
      legacy_rr_sources = readr::col_character(),
      epi_version = readr::col_character(),
      pop_year = readr::col_character(),
      pollutants = readr::col_character(),
      expected_outcome_scope = readr::col_character(),
      return_mode = readr::col_character(),
      contract_status = readr::col_character(),
      mapping_status = readr::col_character(),
      target_selection_id = readr::col_character(),
      blocking_reason = readr::col_character(),
      evidence_path = readr::col_character(),
      decision_owner = readr::col_character(),
      notes = readr::col_character()
    )
  )
}
