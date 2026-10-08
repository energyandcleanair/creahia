test_that("historical RR inventory covers the nine configurations exactly", {
  inventory <- load_historical_rr_requirements()
  derived <- derive_historical_rr_requirements()
  identity_columns <- c(
    "requirement_id",
    "source",
    "cause",
    "outcome",
    "age_contracts",
    "required_by"
  )

  expect_equal(nrow(inventory), 67)
  expect_equal(dplyr::n_distinct(inventory$requirement_id), nrow(inventory))
  expect_equal(
    inventory %>%
      dplyr::select(dplyr::all_of(identity_columns)) %>%
      dplyr::arrange(requirement_id) %>%
      tibble::as_tibble(),
    derived %>%
      dplyr::select(dplyr::all_of(identity_columns)) %>%
      dplyr::arrange(requirement_id) %>%
      tibble::as_tibble()
  )
  expect_equal(
    dplyr::n_distinct(derive_historical_rr_age_components()$component_id),
    350
  )
  expect_setequal(
    unique(inventory$source),
    c(RR_ORIGINAL, RR_GEMM, RR_FUSION, RR_GBD2023)
  )
  expect_setequal(unique(inventory$outcome), c("Deaths", "YLLs", "YLDs"))
})


test_that("historical RR inventory records current public Registry candidates", {
  inventory <- load_historical_rr_requirements()
  candidates <- derive_public_rr_registry_candidates()
  expected <- derive_historical_rr_requirements() %>%
    dplyr::left_join(candidates, by = c("source", "cause", "outcome")) %>%
    dplyr::mutate(
      coverage_status = dplyr::if_else(
        is.na(public_registry_crf_ids),
        "historical_internal_required",
        "public_registry_candidate"
      )
    )

  expect_equal(
    inventory %>%
      dplyr::select(
        requirement_id,
        public_registry_crf_ids,
        coverage_status
      ) %>%
      dplyr::arrange(requirement_id) %>%
      tibble::as_tibble(),
    expected %>%
      dplyr::select(
        requirement_id,
        public_registry_crf_ids,
        coverage_status
      ) %>%
      dplyr::arrange(requirement_id) %>%
      tibble::as_tibble()
  )
  expect_equal(
    sum(inventory$coverage_status == "public_registry_candidate"),
    13
  )
  expect_equal(
    sum(inventory$coverage_status == "historical_internal_required"),
    54
  )
})


test_that("historical RR requirements remain fail-closed", {
  inventory <- load_historical_rr_requirements()
  configurations <- load_historical_fingerprint_configurations()
  represented_configurations <- inventory %>%
    tidyr::separate_rows(required_by, sep = ";") %>%
    dplyr::pull(required_by) %>%
    unique()

  expect_true(all(inventory$readiness_status == "not_ready"))
  expect_true(all(!is.na(inventory$blocking_reason)))
  expect_true(all(inventory$blocking_reason != ""))
  expect_setequal(represented_configurations, configurations$mapping_id)
  expect_true(all(is.na(
    inventory$public_registry_crf_ids[
      inventory$coverage_status == "historical_internal_required"
    ]
  )))
})
