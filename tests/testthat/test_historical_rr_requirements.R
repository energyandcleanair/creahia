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


test_that("historical RR inventory records current Registry data coverage", {
  inventory <- .load_historical_rr_manifest()
  candidates <- derive_public_rr_registry_candidates()
  internal_candidates <- .load_historical_rr_registry() %>%
    dplyr::select(component_id) %>%
    dplyr::mutate(has_internal_candidate = TRUE)
  expected <- derive_historical_rr_requirements() %>%
    dplyr::left_join(candidates, by = c("source", "cause", "outcome")) %>%
    dplyr::left_join(internal_candidates, by = "component_id") %>%
    dplyr::mutate(
      coverage_status = dplyr::case_when(
        !is.na(public_registry_crf_ids) ~ "public_registry_candidate",
        !is.na(has_internal_candidate) ~ "historical_internal_candidate",
        TRUE ~ "historical_internal_required"
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
    sum(inventory$coverage_status == "historical_internal_candidate"),
    27
  )
  expect_equal(
    sum(inventory$coverage_status == "historical_internal_required"),
    29
  )
})


test_that("Fusion death requirements satisfy the routing checklist", {
  manifest <- .load_historical_rr_manifest()
  fusion_deaths <- manifest %>%
    dplyr::filter(
      source == RR_FUSION,
      outcome == MEASURE_DEATHS,
      coverage_status == "public_registry_candidate"
    )
  expect_equal(nrow(fusion_deaths), 7L)
  expect_setequal(
    fusion_deaths$cause,
    c("NCD.LRI", "LRI", "COPD", "Diabetes", "IHD", "LC", "Stroke")
  )
  expect_true(all(
    fusion_deaths$coverage_status == "public_registry_candidate"
  ))
  expect_true(all(
    fusion_deaths$reproduction_status == "parity_verified"
  ))
  expect_true(all(is.na(fusion_deaths$reproduction_blocking_reason)))
  expect_true(all(fusion_deaths$routing_checklist_complete))
  expect_true(all(fusion_deaths$routing_status == "ready"))
  expect_true(all(is.na(fusion_deaths$routing_blocking_reason)))
  expect_true(all(fusion_deaths$scientific_review_status == "not_reviewed"))
  expect_true(all(grepl(
    "test_crf_fusion_migration[.]R",
    fusion_deaths$provenance_notes
  )))
  expect_true(all(grepl(
    "test_fusion_registry_hia_parity[.]R",
    fusion_deaths$provenance_notes
  )))

  registry <- load_crf_registry()
  legacy_fusion <- get_rr(RR_FUSION)
  for (i in seq_len(nrow(fusion_deaths))) {
    component <- fusion_deaths[i, , drop = FALSE]
    candidate <- registry %>%
      dplyr::filter(crf_id == component$public_registry_crf_ids)

    expect_equal(nrow(candidate), 1L, info = component$component_id)
    expect_equal(candidate$cause, component$cause)
    expect_equal(candidate$outcome, component$outcome)

    candidate_data <- load_crf_tabular(candidate) %>%
      dplyr::arrange(age, exposure) %>%
      tibble::as_tibble()
    legacy_data <- legacy_fusion %>%
      dplyr::filter(cause == component$cause, source == RR_FUSION) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      dplyr::arrange(age, exposure) %>%
      tibble::as_tibble()

    expect_identical(
      .canonical_rr_age_contract(candidate_data$age),
      component$age_contract,
      info = component$component_id
    )
    expect_equal(
      candidate_data,
      legacy_data,
      tolerance = 1e-12,
      info = component$component_id
    )
  }
})


test_that("Fusion YLL and YLD outcome reuse is represented only internally", {
  historical_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_FUSION, cause != CAUSE_LRICHILD)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% historical_registry$component_id)

  expect_equal(nrow(historical_registry), 9L)
  expect_setequal(unique(historical_registry$outcome), c("YLLs", "YLDs"))
  expect_false(any(historical_registry$cause == CAUSE_LRICHILD))
  expect_true(all(historical_registry$lifecycle == "historical_reproduction"))
  expect_true(all(
    historical_registry$visibility == "internal_compatibility"
  ))
  expect_true(all(
    historical_registry$reproduction_status == "parity_verified"
  ))
  expect_true(all(is.na(
    historical_registry$reproduction_blocking_reason
  )))
  expect_true(all(
    historical_registry$scientific_review_status == "not_reviewed"
  ))
  expect_true(all(manifest$coverage_status == "historical_internal_candidate"))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))
  expect_true(all(is.na(manifest$public_registry_crf_ids)))
  expect_false(any(
    historical_registry$crf_id %in% load_crf_registry()$crf_id
  ))
  expect_identical(
    historical_registry$double_counted,
    historical_registry$outcome == MEASURE_YLLS &
      historical_registry$cause != "NCD.LRI"
  )

  legacy_fusion <- get_rr(RR_FUSION)
  for (i in seq_len(nrow(historical_registry))) {
    candidate <- historical_registry[i, , drop = FALSE]
    component <- manifest %>%
      dplyr::filter(component_id == candidate$component_id)
    candidate_data <- load_crf_tabular(candidate) %>%
      dplyr::arrange(age, exposure) %>%
      tibble::as_tibble()
    legacy_data <- legacy_fusion %>%
      dplyr::filter(cause == candidate$cause, source == RR_FUSION) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      dplyr::arrange(age, exposure) %>%
      tibble::as_tibble()

    expect_equal(nrow(component), 1L, info = candidate$component_id)
    expect_identical(
      .canonical_rr_age_contract(candidate_data$age),
      component$age_contract,
      info = candidate$component_id
    )
    expect_equal(
      candidate_data,
      legacy_data,
      tolerance = 1e-12,
      info = candidate$component_id
    )
  }
})


test_that("Fusion child LRI keeps one exact internal copy of duplicate rows", {
  historical_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_FUSION, cause == CAUSE_LRICHILD)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% historical_registry$component_id)
  legacy_child <- get_rr(RR_FUSION) %>%
    dplyr::filter(cause == CAUSE_LRICHILD, age == AGE_CHILDREN)
  duplicate_contract <- legacy_child %>%
    dplyr::group_by(age, exposure) %>%
    dplyr::summarise(
      row_count = dplyr::n(),
      distinct_values = dplyr::n_distinct(paste(low, central, high)),
      .groups = "drop"
    )

  expect_equal(nrow(historical_registry), 2L)
  expect_setequal(historical_registry$outcome, c("Deaths", "YLLs"))
  expect_equal(dplyr::n_distinct(historical_registry$data_path), 1L)
  expect_false(any(historical_registry$double_counted))
  expect_true(all(
    historical_registry$reproduction_status == "parity_verified"
  ))
  expect_true(all(is.na(
    historical_registry$reproduction_blocking_reason
  )))
  expect_true(all(
    historical_registry$scientific_review_status == "not_reviewed"
  ))
  expect_true(all(manifest$coverage_status == "historical_internal_candidate"))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))
  expect_true(all(is.na(manifest$public_registry_crf_ids)))
  expect_false(any(
    historical_registry$crf_id %in% load_crf_registry()$crf_id
  ))

  expect_equal(nrow(legacy_child), 6002L)
  expect_equal(nrow(duplicate_contract), 3001L)
  expect_true(all(duplicate_contract$row_count == 2L))
  expect_true(all(duplicate_contract$distinct_values == 1L))

  expected_curve <- legacy_child %>%
    dplyr::select(exposure, age, low, central, high) %>%
    dplyr::distinct(exposure, age, .keep_all = TRUE) %>%
    dplyr::arrange(age, exposure) %>%
    tibble::as_tibble()
  for (i in seq_len(nrow(historical_registry))) {
    candidate <- historical_registry[i, , drop = FALSE]
    candidate_data <- load_crf_tabular(candidate) %>%
      dplyr::arrange(age, exposure) %>%
      tibble::as_tibble()

    expect_identical(
      .canonical_rr_age_contract(candidate_data$age),
      AGE_CHILDREN,
      info = candidate$component_id
    )
    expect_equal(
      candidate_data,
      expected_curve,
      tolerance = 0,
      info = candidate$component_id
    )
  }
})


test_that("Fusion child LRI internal curve is reproducible and fail-closed", {
  output_dir <- withr::local_tempdir()
  output_paths <- .compile_historical_fusion_rr_curves(
    output_dir = output_dir
  )
  committed_dir <- get_hia_path(
    "rr/compatibility/tabular",
    error_if_not_exists = TRUE
  )

  expect_setequal(
    basename(output_paths),
    .historical_fusion_rr_curve_specs()$filename
  )
  for (output_path in output_paths) {
    generated <- readr::read_csv(output_path, show_col_types = FALSE)
    committed <- readr::read_csv(
      file.path(committed_dir, basename(output_path)),
      show_col_types = FALSE
    )
    expect_equal(
      generated,
      committed,
      tolerance = 0,
      info = basename(output_path)
    )
  }

  conflicting_source <- readr::read_csv(
    get_hia_path("rr/processed/rr_fusion.csv", error_if_not_exists = TRUE),
    show_col_types = FALSE
  )
  child_rows <- which(
    conflicting_source$source == RR_FUSION &
      conflicting_source$cause == CAUSE_LRICHILD &
      conflicting_source$age == AGE_CHILDREN
  )
  conflicting_source$central[[child_rows[[1]]]] <-
    conflicting_source$central[[child_rows[[1]]]] + 0.01
  conflicting_path <- file.path(withr::local_tempdir(), "rr_fusion.csv")
  readr::write_csv(conflicting_source, conflicting_path)

  expect_error(
    .compile_historical_fusion_rr_curves(
      source_path = conflicting_path,
      output_dir = withr::local_tempdir()
    ),
    "duplicate exposure rows contain different RR values"
  )
})


test_that("GEMM child LRI keeps one exact internal copy of duplicate rows", {
  historical_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(source == RR_GEMM, cause == CAUSE_LRICHILD)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% historical_registry$component_id)
  legacy_child <- get_rr(RR_GEMM) %>%
    dplyr::filter(cause == CAUSE_LRICHILD, age == AGE_CHILDREN)
  duplicate_contract <- legacy_child %>%
    dplyr::group_by(age, exposure) %>%
    dplyr::summarise(
      row_count = dplyr::n(),
      distinct_values = dplyr::n_distinct(paste(low, central, high)),
      .groups = "drop"
    )

  expect_equal(nrow(historical_registry), 2L)
  expect_setequal(historical_registry$outcome, c("Deaths", "YLLs"))
  expect_equal(dplyr::n_distinct(historical_registry$data_path), 1L)
  expect_false(any(historical_registry$double_counted))
  expect_true(all(
    historical_registry$reproduction_status == "parity_verified"
  ))
  expect_true(all(is.na(
    historical_registry$reproduction_blocking_reason
  )))
  expect_true(all(
    historical_registry$scientific_review_status == "not_reviewed"
  ))
  expect_true(all(manifest$coverage_status == "historical_internal_candidate"))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))
  expect_true(all(is.na(manifest$public_registry_crf_ids)))
  expect_false(any(
    historical_registry$crf_id %in% load_crf_registry()$crf_id
  ))

  expect_equal(nrow(legacy_child), 39013L)
  expect_equal(nrow(duplicate_contract), 3001L)
  expect_true(all(duplicate_contract$row_count == 13L))
  expect_true(all(duplicate_contract$distinct_values == 1L))

  expected_curve <- legacy_child %>%
    dplyr::select(exposure, age, low, central, high) %>%
    dplyr::distinct(exposure, age, .keep_all = TRUE) %>%
    dplyr::arrange(age, exposure) %>%
    tibble::as_tibble()
  for (i in seq_len(nrow(historical_registry))) {
    candidate <- historical_registry[i, , drop = FALSE]
    candidate_data <- load_crf_tabular(candidate) %>%
      dplyr::arrange(age, exposure) %>%
      tibble::as_tibble()

    expect_identical(
      .canonical_rr_age_contract(candidate_data$age),
      AGE_CHILDREN,
      info = candidate$component_id
    )
    expect_equal(
      candidate_data,
      expected_curve,
      tolerance = 0,
      info = candidate$component_id
    )
  }
})


test_that("GEMM child LRI internal curve is reproducible and fail-closed", {
  output_dir <- withr::local_tempdir()
  output_paths <- .compile_historical_gemm_rr_curves(
    output_dir = output_dir
  )
  committed_dir <- get_hia_path(
    "rr/compatibility/tabular",
    error_if_not_exists = TRUE
  )

  expect_setequal(
    basename(output_paths),
    .historical_gemm_rr_curve_specs()$filename
  )
  for (output_path in output_paths) {
    generated <- readr::read_csv(output_path, show_col_types = FALSE)
    committed <- readr::read_csv(
      file.path(committed_dir, basename(output_path)),
      show_col_types = FALSE
    )
    expect_equal(
      generated,
      committed,
      tolerance = 0,
      info = basename(output_path)
    )
  }

  conflicting_source <- readr::read_csv(
    get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE),
    show_col_types = FALSE
  )
  child_rows <- which(
    conflicting_source$source == RR_GEMM &
      conflicting_source$cause == CAUSE_LRICHILD &
      conflicting_source$age == AGE_CHILDREN
  )
  conflicting_source$central[[child_rows[[1]]]] <-
    conflicting_source$central[[child_rows[[1]]]] + 0.01
  conflicting_path <- file.path(withr::local_tempdir(), "rr_gemm.csv")
  readr::write_csv(conflicting_source, conflicting_path)

  expect_error(
    .compile_historical_gemm_rr_curves(
      source_path = conflicting_path,
      output_dir = withr::local_tempdir()
    ),
    "duplicate exposure rows contain different RR values"
  )
})


test_that("historical RR routing checklist remains fail-closed", {
  inventory <- .load_historical_rr_manifest()
  configurations <- load_historical_fingerprint_configurations()
  represented_configurations <- inventory %>%
    tidyr::separate_rows(required_by, sep = ";") %>%
    dplyr::pull(required_by) %>%
    unique()

  expect_equal(
    inventory %>%
      dplyr::ungroup() %>%
      dplyr::count(reproduction_status, name = "n") %>%
      tibble::as_tibble(),
    tibble::tribble(
      ~reproduction_status, ~n,
      "not_verified", 29L,
      "parity_verified", 40L
    )
  )
  expect_true(all(
    !is.na(inventory$reproduction_blocking_reason[
      inventory$reproduction_status == "not_verified"
    ])
  ))
  expect_true(all(is.na(
    inventory$reproduction_blocking_reason[
      inventory$reproduction_status == "parity_verified"
    ]
  )))
  expect_equal(
    inventory %>%
      dplyr::ungroup() %>%
      dplyr::count(routing_status, name = "n") %>%
      tibble::as_tibble(),
    tibble::tribble(
      ~routing_status, ~n,
      "not_ready", 29L,
      "ready", 40L
    )
  )
  expect_true(all(inventory$routing_checklist_complete ==
    (inventory$routing_status == "ready")))
  expect_true(all(is.na(
    inventory$routing_blocking_reason[inventory$routing_status == "ready"]
  )))
  expect_true(all(
    !is.na(inventory$routing_blocking_reason[
      inventory$routing_status == "not_ready"
    ])
  ))
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


test_that("selected RR rows record complete routing evidence", {
  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = c(RR_GEMM, RR_ORIGINAL),
    calc_causes = "GEMM and GBD",
    gbd_causes = NULL
  )
  selected_components <- .normalise_rr_components(
    parse_rr_sources(rr_selection$rr_sources)
  )
  manifest <- .load_historical_rr_manifest()
  selected_manifest <- manifest %>%
    dplyr::semi_join(selected_components, by = "component_id")
  gemm_deaths <- selected_manifest %>%
    dplyr::filter(source == RR_GEMM, outcome == MEASURE_DEATHS)
  expect_equal(nrow(selected_manifest), 18L)
  expect_equal(nrow(gemm_deaths), 6L)
  expect_setequal(
    gemm_deaths$cause,
    c("NCD.LRI", "COPD", "IHD", "LC", "LRI", "Stroke")
  )
  expect_true(all(gemm_deaths$coverage_status == "public_registry_candidate"))
  expect_true(all(gemm_deaths$reproduction_status == "parity_verified"))
  expect_true(all(is.na(gemm_deaths$reproduction_blocking_reason)))
  expect_true(all(gemm_deaths$routing_checklist_complete))
  expect_true(all(gemm_deaths$routing_status == "ready"))
  expect_true(all(is.na(gemm_deaths$routing_blocking_reason)))
  expect_true(all(gemm_deaths$scientific_review_status == "not_reviewed"))

  registry <- load_crf_registry()
  legacy_gemm <- get_rr(RR_GEMM)
  for (i in seq_len(nrow(gemm_deaths))) {
    component <- gemm_deaths[i, , drop = FALSE]
    candidate_ids <- strsplit(
      component$public_registry_crf_ids,
      ";",
      fixed = TRUE
    )[[1]]
    candidates <- registry %>%
      dplyr::filter(crf_id %in% candidate_ids)
    age_matches <- vapply(seq_len(nrow(candidates)), function(j) {
      candidate_data <- load_crf_tabular(candidates[j, , drop = FALSE])
      identical(
        .canonical_rr_age_contract(candidate_data$age),
        component$age_contract
      )
    }, logical(1))
    matching_candidate <- candidates[age_matches, , drop = FALSE]

    expect_equal(nrow(matching_candidate), 1L, info = component$component_id)
    expect_equal(
      matching_candidate$cause,
      component$cause,
      info = component$component_id
    )
    expect_equal(
      matching_candidate$outcome,
      component$outcome,
      info = component$component_id
    )

    candidate_data <- load_crf_tabular(matching_candidate) %>%
      dplyr::arrange(exposure, age) %>%
      tibble::as_tibble()
    legacy_data <- legacy_gemm %>%
      dplyr::filter(cause == component$cause) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      dplyr::arrange(exposure, age) %>%
      tibble::as_tibble()
    expect_equal(
      candidate_data,
      legacy_data,
      tolerance = 1e-12,
      info = component$component_id
    )
  }

  remaining <- selected_manifest %>%
    dplyr::anti_join(gemm_deaths, by = "component_id")
  expect_equal(nrow(remaining), 12L)
  expect_true(all(remaining$reproduction_status == "parity_verified"))
  expect_true(all(is.na(remaining$reproduction_blocking_reason)))
  expect_true(all(remaining$routing_checklist_complete))
  expect_true(all(remaining$routing_status == "ready"))
  expect_true(all(is.na(remaining$routing_blocking_reason)))
})


test_that("historical RR routing fields must match the engineering checklist", {
  manifest <- .load_historical_rr_manifest()
  ready_row <- which(manifest$routing_status == "ready")[[1]]
  mismatched <- manifest
  mismatched$routing_status[ready_row] <- "not_ready"
  mismatched$routing_blocking_reason[ready_row] <- "Manual routing hold."

  expect_error(
    .validate_historical_rr_manifest(mismatched),
    "must match the engineering checklist"
  )
})


test_that("GEMM YLL and YLD outcome reuse is represented only internally", {
  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = c(RR_GEMM, RR_ORIGINAL),
    calc_causes = "GEMM and GBD",
    gbd_causes = NULL
  )
  selected_components <- .normalise_rr_components(
    parse_rr_sources(rr_selection$rr_sources)
  )
  expected_components <- selected_components %>%
    dplyr::filter(source == RR_GEMM, outcome != MEASURE_DEATHS)
  complete_historical_registry <- .load_historical_rr_registry()
  historical_registry <- complete_historical_registry %>%
    dplyr::filter(source == RR_GEMM, cause != CAUSE_LRICHILD)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% historical_registry$component_id)

  expect_equal(nrow(expected_components), 8L)
  expect_equal(nrow(complete_historical_registry), 27L)
  expect_equal(nrow(historical_registry), 8L)
  expect_setequal(
    historical_registry$component_id,
    expected_components$component_id
  )
  expect_setequal(unique(historical_registry$outcome), c("YLLs", "YLDs"))
  expect_true(all(historical_registry$lifecycle == "historical_reproduction"))
  expect_true(all(historical_registry$visibility == "internal_compatibility"))
  expect_true(all(
    historical_registry$reproduction_status == "parity_verified"
  ))
  expect_true(all(is.na(historical_registry$reproduction_blocking_reason)))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))
  expect_true(all(
    historical_registry$scientific_review_status == "not_reviewed"
  ))
  expect_true(all(manifest$coverage_status == "historical_internal_candidate"))
  expect_true(all(is.na(manifest$public_registry_crf_ids)))
  expect_false(any(
    historical_registry$crf_id %in% load_crf_registry()$crf_id
  ))

  legacy_gemm <- get_rr(RR_GEMM)
  for (i in seq_len(nrow(historical_registry))) {
    candidate <- historical_registry[i, , drop = FALSE]
    component <- manifest %>%
      dplyr::filter(component_id == candidate$component_id)
    candidate_data <- load_crf_tabular(candidate) %>%
      dplyr::arrange(exposure, age) %>%
      tibble::as_tibble()
    legacy_data <- legacy_gemm %>%
      dplyr::filter(cause == candidate$cause) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      dplyr::arrange(exposure, age) %>%
      tibble::as_tibble()

    expect_equal(nrow(component), 1L, info = candidate$component_id)
    expect_identical(
      .canonical_rr_age_contract(candidate_data$age),
      component$age_contract,
      info = candidate$component_id
    )
    expect_equal(
      candidate_data,
      legacy_data,
      tolerance = 1e-12,
      info = candidate$component_id
    )
  }
})


test_that("original RR rows retain exact internal curve representations", {
  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = c(RR_GEMM, RR_ORIGINAL),
    calc_causes = "GEMM and GBD",
    gbd_causes = NULL
  )
  expected_components <- .normalise_rr_components(
    parse_rr_sources(rr_selection$rr_sources)
  ) %>%
    dplyr::filter(source == RR_ORIGINAL)
  historical_registry <- .load_historical_rr_registry() %>%
    dplyr::filter(component_id %in% expected_components$component_id)
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(component_id %in% historical_registry$component_id)
  expect_equal(nrow(expected_components), 4L)
  expect_equal(nrow(historical_registry), 4L)
  expect_setequal(
    historical_registry$component_id,
    expected_components$component_id
  )
  expect_equal(dplyr::n_distinct(historical_registry$data_path), 2L)
  expect_true(all(historical_registry$lifecycle == "historical_reproduction"))
  expect_true(all(historical_registry$visibility == "internal_compatibility"))
  expect_true(all(
    historical_registry$reproduction_status == "parity_verified"
  ))
  expect_true(all(is.na(historical_registry$reproduction_blocking_reason)))
  expect_true(all(manifest$routing_checklist_complete))
  expect_true(all(manifest$routing_status == "ready"))
  expect_true(all(is.na(manifest$routing_blocking_reason)))
  expect_true(all(
    historical_registry$scientific_review_status == "not_reviewed"
  ))
  expect_true(all(manifest$coverage_status == "historical_internal_candidate"))
  expect_true(all(is.na(manifest$public_registry_crf_ids)))
  expect_false(any(
    historical_registry$crf_id %in% load_crf_registry()$crf_id
  ))

  expected_double_counted <- c(TRUE, FALSE, FALSE, FALSE)
  names(expected_double_counted) <- c(
    "rr|original|Diabetes|Deaths",
    "rr|original|Diabetes|YLDs",
    "rr|original|LRI.child|Deaths",
    "rr|original|LRI.child|YLLs"
  )
  expect_identical(
    stats::setNames(
      historical_registry$double_counted,
      historical_registry$component_id
    )[names(expected_double_counted)],
    expected_double_counted
  )

  legacy_original <- get_rr(RR_ORIGINAL)
  for (i in seq_len(nrow(historical_registry))) {
    candidate <- historical_registry[i, , drop = FALSE]
    component <- manifest %>%
      dplyr::filter(component_id == candidate$component_id)
    candidate_data <- load_crf_tabular(candidate) %>%
      dplyr::arrange(exposure, age) %>%
      tibble::as_tibble()
    legacy_data <- legacy_original %>%
      dplyr::filter(cause == candidate$cause) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      dplyr::arrange(exposure, age) %>%
      tibble::as_tibble()

    expect_equal(nrow(component), 1L, info = candidate$component_id)
    expect_identical(
      .canonical_rr_age_contract(candidate_data$age),
      component$age_contract,
      info = candidate$component_id
    )
    expect_equal(
      candidate_data,
      legacy_data,
      tolerance = 1e-12,
      info = candidate$component_id
    )
  }
})


test_that("original RR internal curves can be regenerated from the legacy source", {
  output_dir <- withr::local_tempdir()
  output_paths <- .compile_historical_original_rr_curves(
    output_dir = output_dir
  )
  committed_dir <- get_hia_path(
    "rr/compatibility/tabular",
    error_if_not_exists = TRUE
  )

  expect_setequal(
    basename(output_paths),
    .historical_original_rr_curve_specs()$filename
  )
  for (output_path in output_paths) {
    generated <- readr::read_csv(output_path, show_col_types = FALSE)
    committed <- readr::read_csv(
      file.path(committed_dir, basename(output_path)),
      show_col_types = FALSE
    )
    expect_equal(
      generated,
      committed,
      tolerance = 0,
      info = basename(output_path)
    )
  }

  duplicate_source <- readr::read_csv(
    get_hia_path("rr/processed/rr_original.csv", error_if_not_exists = TRUE),
    show_col_types = FALSE
  )
  duplicate_source <- dplyr::bind_rows(
    duplicate_source,
    duplicate_source %>%
      dplyr::filter(cause == "LRI", age == AGE_ADULTS) %>%
      dplyr::slice(1)
  )
  duplicate_path <- file.path(withr::local_tempdir(), "rr_original.csv")
  readr::write_csv(duplicate_source, duplicate_path)
  expect_error(
    .compile_historical_original_rr_curves(
      source_path = duplicate_path,
      output_dir = withr::local_tempdir()
    ),
    "duplicate age/exposure rows for LRI / 25\\+"
  )
})


test_that("historical RR manifest enforces canonical internal identities", {
  manifest <- .load_historical_rr_manifest()
  historical_registry <- .load_historical_rr_registry()

  expect_named(manifest, .historical_rr_manifest_columns())
  expect_invisible(.validate_historical_rr_manifest(manifest))
  expect_named(historical_registry, .historical_rr_registry_columns())
  expect_invisible(.validate_historical_rr_registry(historical_registry))
  expect_true(all(manifest$component_type == "rr"))
  expect_true(all(manifest$selection_id == "historical_rr"))
  expect_true(all(manifest$lifecycle == "historical_reproduction"))
  expect_true(all(manifest$visibility == "internal_compatibility"))
  expect_true(all(
    manifest$routing_status == manifest$routing_checklist_status
  ))
  expect_true(all(
    manifest$routing_checklist_complete ==
      (
        manifest$routing_check_data_available &
        manifest$routing_check_reproduction_verified &
        manifest$routing_check_visibility_guarded
      )
  ))

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
  expect_error(
    .validate_historical_rr_registry(
      dplyr::mutate(
        historical_registry,
        component_id = paste0(component_id, "|invalid")
      )
    ),
    "must match source/cause/outcome"
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

  expect_true(assessed$ready)
  expect_equal(nrow(assessed$blocking_components), 0L)
})
