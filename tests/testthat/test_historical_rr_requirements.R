test_that("historical RR manifest covers named configurations and actual default", {
  inventory <- .load_historical_rr_manifest()
  derived <- derive_historical_rr_requirements()
  identity_columns <- c(
    "component_id",
    "source",
    "cause",
    "outcome",
    "age_contract",
    "required_by"
  )

  expect_equal(nrow(inventory), 69)
  expect_equal(dplyr::n_distinct(inventory$component_id), nrow(inventory))
  expect_equal(
    inventory %>%
      dplyr::select(dplyr::all_of(identity_columns)) %>%
      dplyr::arrange(component_id) %>%
      tibble::as_tibble(),
    derived %>%
      dplyr::select(dplyr::all_of(identity_columns)) %>%
      dplyr::arrange(component_id) %>%
      tibble::as_tibble()
  )
  expect_equal(
    sum(lengths(strsplit(inventory$age_contract, ";", fixed = TRUE))),
    333
  )
  expect_false(any(grepl("25\\+;25-29", inventory$age_contract)))
  expect_setequal(
    unique(inventory$source),
    c(RR_ORIGINAL, RR_GEMM, RR_FUSION, RR_GBD2023)
  )
  expect_setequal(unique(inventory$outcome), c("Deaths", "YLLs", "YLDs"))
})


test_that("historical RR inventory records current public Registry candidates", {
  inventory <- .load_historical_rr_manifest()
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
        component_id,
        public_registry_crf_ids,
        coverage_status
      ) %>%
      dplyr::arrange(component_id) %>%
      tibble::as_tibble(),
    expected %>%
      dplyr::select(
        component_id,
        public_registry_crf_ids,
        coverage_status
      ) %>%
      dplyr::arrange(component_id) %>%
      tibble::as_tibble()
  )
  expect_equal(
    sum(inventory$coverage_status == "public_registry_candidate"),
    13
  )
  expect_equal(
    sum(inventory$coverage_status == "historical_internal_required"),
    56
  )
})


test_that("historical RR requirements remain fail-closed", {
  inventory <- .load_historical_rr_manifest()
  configurations <- load_historical_fingerprint_configurations()
  represented_configurations <- inventory %>%
    tidyr::separate_rows(required_by, sep = ";") %>%
    dplyr::pull(required_by) %>%
    unique()

  expect_true(all(inventory$reproduction_status == "not_verified"))
  expect_true(all(!is.na(inventory$reproduction_blocking_reason)))
  expect_true(all(inventory$reproduction_blocking_reason != ""))
  expect_true(all(inventory$scientific_review_status == "not_reviewed"))
  expect_true(all(!is.na(inventory$scientific_review_reason)))
  expect_true(all(inventory$scientific_review_reason != ""))
  expect_setequal(
    represented_configurations,
    c(configurations$mapping_id, "actual_no_argument_default")
  )
  expect_true(all(is.na(
    inventory$public_registry_crf_ids[
      inventory$coverage_status == "historical_internal_required"
    ]
  )))
})


test_that("historical RR manifest enforces canonical internal identities", {
  manifest <- .load_historical_rr_manifest()

  expect_named(manifest, .historical_rr_manifest_columns())
  expect_invisible(.validate_historical_rr_manifest(manifest))
  expect_true(all(manifest$component_type == "rr"))
  expect_true(all(manifest$selection_id == "historical_rr"))
  expect_true(all(manifest$lifecycle == "historical_reproduction"))
  expect_true(all(manifest$visibility == "internal_compatibility"))

  expect_error(
    .validate_historical_rr_manifest(
      dplyr::mutate(manifest, component_id = paste0(component_id, "|25+"))
    ),
    "must match source/cause/outcome"
  )

  split_row <- which(grepl("25-29", manifest$age_contract))[[1]]
  noncanonical <- manifest
  noncanonical$age_contract[[split_row]] <- paste0(
    "25+;",
    noncanonical$age_contract[[split_row]]
  )
  expect_error(
    .validate_historical_rr_manifest(noncanonical),
    "canonical calculation ages"
  )
})


test_that("runtime component manifest combines historical CRF and RR identities", {
  manifest <- .load_hia_component_manifest()
  rr_manifest <- .load_historical_rr_manifest()
  crf_memberships <- .load_historical_crf_memberships()

  expect_equal(nrow(manifest), 124)
  expect_equal(dplyr::n_distinct(manifest$component_id), nrow(manifest))
  expect_setequal(
    manifest$component_id[manifest$component_type == "rr"],
    rr_manifest$component_id
  )
  expect_setequal(
    manifest$component_id[manifest$component_type == "crf"],
    crf_memberships$component_id
  )

  default_rr <- .normalise_rr_components(c(RR_GEMM, RR_ORIGINAL))
  assessed <- .assess_hia_component_readiness(default_rr, manifest)

  expect_false(assessed$ready)
  expect_equal(nrow(assessed$blocking_components), nrow(default_rr))
  expect_false(any(grepl(
    "absent from the readiness manifest",
    assessed$blocking_components$blocking_reason,
    fixed = TRUE
  )))
})
