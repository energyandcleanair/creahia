test_that("legacy compatibility matrix has a stable auditable contract", {
  matrix <- load_legacy_compatibility_matrix()

  required_columns <- c(
    "compatibility_id",
    "historical_mapping_id",
    "entry_point",
    "method",
    "legacy_crfs_version",
    "legacy_calc_causes",
    "legacy_gbd_causes",
    "legacy_rr_sources",
    "epi_version",
    "pop_year",
    "pollutants",
    "expected_outcome_scope",
    "return_mode",
    "contract_status",
    "mapping_status",
    "target_selection_id",
    "blocking_reason",
    "evidence_path",
    "decision_owner",
    "notes"
  )

  expect_named(matrix, required_columns)
  expect_gt(nrow(matrix), 0)
  expect_false(any(is.na(matrix$compatibility_id)))
  expect_equal(dplyr::n_distinct(matrix$compatibility_id), nrow(matrix))
  expect_true(all(matrix$contract_status %in% c(
    "required",
    "observed_but_unconfirmed",
    "unsupported_invalid"
  )))
  expect_true(all(matrix$mapping_status %in% c(
    "legacy_defined",
    "registry_ready",
    "not_applicable"
  )))
  expect_true(all(matrix$method %in% c("default", "character", "direct")))
  expect_true(all(matrix$return_mode %in% c(
    "data_frame",
    "list_hia_concentrations",
    "saved_rds_list_hia_concentrations",
    "error"
  )))
  expect_false(any(is.na(matrix$entry_point)))
  expect_false(any(is.na(matrix$evidence_path)))
  expect_false(any(is.na(matrix$decision_owner)))

  project_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  expect_true(all(file.exists(file.path(project_root, matrix$evidence_path))))
})

test_that("legacy compatibility readiness states cannot hide incomplete targets", {
  matrix <- load_legacy_compatibility_matrix()

  legacy_defined <- matrix %>%
    dplyr::filter(mapping_status == "legacy_defined")
  registry_ready <- matrix %>%
    dplyr::filter(mapping_status == "registry_ready")
  not_applicable <- matrix %>%
    dplyr::filter(mapping_status == "not_applicable")

  expect_gt(nrow(legacy_defined), 0)
  expect_true(all(is.na(legacy_defined$target_selection_id)))
  expect_true(all(!is.na(legacy_defined$blocking_reason)))

  expected_ready <- tibble::tribble(
    ~compatibility_id, ~target_selection_id,
    "legacy_default_gbd2019_only_2020", "component_resolved|legacy_profile_default|legacy_cause_selectors|gbd2019",
    "legacy_default_gemm_gbd2019_2020", "component_resolved|legacy_profile_default|legacy_cause_selectors|gbd2019",
    "legacy_default_gbd2021_only_2020", "component_resolved|legacy_profile_default|legacy_cause_selectors|gbd2021",
    "legacy_default_gemm_gbd2021_2020", "component_resolved|legacy_profile_default|legacy_cause_selectors|gbd2021",
    "legacy_default_gemm_gbd2021_2021", "component_resolved|legacy_profile_default|legacy_cause_selectors|gbd2021",
    "legacy_default_fusion_gbd2021_2021", "component_resolved|legacy_profile_default|rr_sources|gbd2021",
    "c40_project_cause_vector_epi_c40_pop2022", "component_resolved|legacy_profile_c40|legacy_cause_selectors|C40",
    "c40_character_template_epi_c40_pop2019", "component_resolved|legacy_profile_c40|legacy_cause_selectors|C40",
    "krewski_sa_gbd_only_all_epi_c40_pop2021", "component_resolved|legacy_profile_krewski_south_africa|legacy_cause_selectors|C40"
  )

  expect_equal(
    registry_ready %>%
      dplyr::select(compatibility_id, target_selection_id) %>%
      dplyr::arrange(compatibility_id),
    expected_ready %>% dplyr::arrange(compatibility_id)
  )
  expect_true(all(is.na(registry_ready$blocking_reason)))

  expect_true(all(is.na(not_applicable$target_selection_id)))
  expect_true(all(!is.na(not_applicable$blocking_reason)))
  expect_false(any(matrix$target_selection_id == "experimental_default", na.rm = TRUE))
  expect_true(all(
    matrix$mapping_status[matrix$contract_status == "required"] %in%
      c("legacy_defined", "registry_ready")
  ))
})

test_that("compatibility matrix covers the historical fingerprint inventory exactly", {
  matrix <- load_legacy_compatibility_matrix()
  historical <- load_historical_fingerprint_configurations()

  mapped <- matrix %>%
    dplyr::filter(!is.na(historical_mapping_id))

  expect_setequal(mapped$historical_mapping_id, historical$mapping_id)
  expect_equal(dplyr::n_distinct(mapped$historical_mapping_id), nrow(historical))

  comparison <- mapped %>%
    dplyr::select(
      historical_mapping_id,
      legacy_crfs_version,
      legacy_calc_causes,
      legacy_gbd_causes,
      legacy_rr_sources,
      epi_version,
      pop_year,
      mapping_status,
      target_selection_id
    ) %>%
    dplyr::left_join(
      historical %>%
        dplyr::transmute(
          historical_mapping_id = mapping_id,
          expected_crfs_version = legacy_crfs_version,
          expected_calc_causes = legacy_calc_causes,
          expected_gbd_causes = legacy_gbd_causes,
          expected_rr_sources = legacy_rr_sources,
          expected_epi_version = epi_version,
          expected_pop_year = as.character(pop_year),
          expected_mapping_status = mapping_status,
          expected_target_selection_id = target_selection_id
        ),
      by = "historical_mapping_id"
    )

  expect_equal(comparison$legacy_crfs_version, comparison$expected_crfs_version)
  expect_equal(comparison$legacy_calc_causes, comparison$expected_calc_causes)
  expect_equal(comparison$legacy_gbd_causes, comparison$expected_gbd_causes)
  expect_equal(comparison$legacy_rr_sources, comparison$expected_rr_sources)
  expect_equal(comparison$epi_version, comparison$expected_epi_version)
  expect_equal(comparison$pop_year, comparison$expected_pop_year)
  expect_equal(comparison$mapping_status, comparison$expected_mapping_status)
  expect_equal(
    comparison$target_selection_id,
    comparison$expected_target_selection_id
  )
})

test_that("required legacy profiles and wrapper modes remain in scope", {
  matrix <- load_legacy_compatibility_matrix()
  required <- matrix %>%
    dplyr::filter(contract_status == "required")

  expect_setequal(
    unique(required$legacy_crfs_version),
    c("default", "C40", "Krewski-South Africa")
  )
  expect_true(all(c("default", "character") %in% required$method))
  expect_true("list_hia_concentrations" %in% required$return_mode)
  expect_true("saved_rds_list_hia_concentrations" %in% required$return_mode)
  expect_true(any(required$legacy_calc_causes == "explicit_cause_vector_deaths_yld"))
  expect_true(any(required$legacy_gbd_causes == "all"))
})

test_that("invalid legacy combinations remain an explicit error contract", {
  matrix <- load_legacy_compatibility_matrix()
  invalid <- matrix %>%
    dplyr::filter(contract_status == "unsupported_invalid")

  expect_gt(nrow(invalid), 0)
  expect_true(all(invalid$mapping_status == "not_applicable"))
  expect_true(all(invalid$return_mode == "error"))
  expect_true(all(!is.na(invalid$blocking_reason)))
})
