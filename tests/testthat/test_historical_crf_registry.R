make_historical_registry_row <- function(
  reproduction_status = "parity_verified",
  reproduction_blocking_reason = NA_character_,
  scientific_review_status = "not_reviewed",
  scientific_review_reason = "Method review is pending."
) {
  load_crf_registry() %>%
    dplyr::filter(form == CRF_FORM_LOG_LINEAR) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      crf_id = "historical_test_crf_v1",
      lifecycle = "historical_reproduction",
      visibility = "internal_compatibility",
      reproduction_status = reproduction_status,
      reproduction_blocking_reason = reproduction_blocking_reason,
      scientific_review_status = scientific_review_status,
      scientific_review_reason = scientific_review_reason,
      question_ids = "Q-test",
      provenance_notes = "Synthetic historical row used only for boundary tests."
    )
}


make_historical_membership <- function(registry) {
  tibble::tibble(
    component_id = "crf_profile|legacy_profile_default|NO2|TestCause|Deaths",
    selection_id = "legacy_profile_default",
    crf_id = registry$crf_id,
    notes = "Synthetic membership used only for boundary tests."
  )
}


historical_profile_cases <- function() {
  tibble::tribble(
    ~version, ~selection_id, ~expected_rows,
    "default", "legacy_profile_default", 15L,
    "C40", "legacy_profile_c40", 19L,
    "Krewski-South Africa", "legacy_profile_krewski_south_africa", 21L
  )
}


historical_numeric_contract_columns <- function() {
  c(
    "pollutant",
    "cause",
    "outcome",
    "double_counted",
    "rr_central",
    "rr_low",
    "rr_high",
    "conc_change",
    "counterfact",
    "unit",
    "units_multiplier"
  )
}


test_that("packaged historical stores account for all active CRF profiles", {
  registry <- .load_historical_crf_registry()
  memberships <- .load_historical_crf_memberships()

  expect_equal(nrow(registry), 25)
  expect_equal(nrow(memberships), 55)
  expect_true(all(c(
    CRF_REGISTRY_REQUIRED_COLUMNS,
    .historical_registry_metadata_columns()
  ) %in% names(registry)))
  expect_named(memberships, .historical_membership_columns())
  expect_true(all(registry$lifecycle == "historical_reproduction"))
  expect_true(all(registry$visibility == "internal_compatibility"))
  default_rows <- registry %>%
    dplyr::filter(grepl("^historical_default_", crf_id))
  other_rows <- registry %>%
    dplyr::filter(!grepl("^historical_default_", crf_id))
  expect_equal(nrow(default_rows), 15L)
  expect_true(all(default_rows$reproduction_status == "parity_verified"))
  expect_true(all(is.na(default_rows$reproduction_blocking_reason)))
  expect_equal(nrow(other_rows), 10L)
  expect_true(all(other_rows$reproduction_status == "not_verified"))
  expect_true(all(!is.na(other_rows$reproduction_blocking_reason)))
  expect_true(all(registry$scientific_review_status == "not_reviewed"))
  expect_true(all(!is.na(registry$scientific_review_reason)))
  expect_true(all(!is.na(registry$question_ids)))
  expect_setequal(
    registry$crf_id,
    c(
      sprintf("historical_default_l%02d_v1", seq_len(15)),
      sprintf("historical_l%02d_v1", 16:25)
    )
  )
})


test_that("packaged definitions preserve every profile's numeric contract", {
  registry <- .load_historical_crf_registry()
  memberships <- .load_historical_crf_memberships()
  contract_columns <- historical_numeric_contract_columns()

  purrr::pwalk(historical_profile_cases(), function(
    version,
    selection_id,
    expected_rows
  ) {
    stored_contract <- memberships %>%
      dplyr::filter(.data$selection_id == !!selection_id) %>%
      dplyr::inner_join(registry, by = "crf_id") %>%
      dplyr::select(dplyr::all_of(contract_columns)) %>%
      dplyr::arrange(pollutant, cause, outcome) %>%
      tibble::as_tibble()
    legacy_contract <- get_crfs(version) %>%
      dplyr::select(dplyr::all_of(contract_columns)) %>%
      dplyr::arrange(pollutant, cause, outcome) %>%
      tibble::as_tibble()

    expect_equal(nrow(stored_contract), expected_rows, info = selection_id)
    expect_equal(stored_contract, legacy_contract, info = selection_id)
  })

  default_definitions <- registry %>%
    dplyr::filter(grepl("^historical_default_", .data$crf_id))
  added_definitions <- registry %>%
    dplyr::filter(grepl("^historical_l", .data$crf_id))

  expect_true(all(is.na(default_definitions$age_low)))
  expect_true(all(is.na(default_definitions$age_high)))
  expect_true(all(added_definitions$age_low == 25))
  expect_true(all(added_definitions$age_high == 99))
  expect_true(all(registry$form == CRF_FORM_LOG_LINEAR))
  expect_true(all(registry$reference_id %in% load_crf_references()$reference_id))
})


test_that("packaged memberships cover every stable profile identity", {
  registry <- .load_historical_crf_registry()
  memberships <- .load_historical_crf_memberships()

  purrr::pwalk(historical_profile_cases(), function(
    version,
    selection_id,
    expected_rows
  ) {
    expected_components <- .normalise_crf_components(
      get_crfs(version),
      input_mode = "legacy_profile",
      selection_id = selection_id
    )
    stored_components <- memberships %>%
      dplyr::filter(.data$selection_id == !!selection_id)

    expect_equal(nrow(stored_components), expected_rows, info = selection_id)
    expect_setequal(
      stored_components$component_id,
      expected_components$component_id
    )
  })

  expect_setequal(memberships$crf_id, registry$crf_id)
  default_memberships <- memberships %>%
    dplyr::filter(selection_id == "legacy_profile_default")
  resolved_default <- .resolve_historical_crf_components(
    component_ids = default_memberships$component_id,
    registry = registry,
    memberships = memberships
  )
  expect_s3_class(resolved_default, "creahia_historical_crf_set")
  expect_equal(nrow(resolved_default), 15L)
  expect_error(
    .resolve_historical_crf_components(
      component_ids = memberships$component_id,
      registry = registry,
      memberships = memberships
    ),
    "not parity-verified"
  )
})


test_that("the C40 NO2 mortality reference is fully registered", {
  reference <- load_crf_references() %>%
    dplyr::filter(.data$reference_id == "faustini_2014_no2_mortality")

  expect_equal(nrow(reference), 1)
  expect_equal(reference$year, 2014)
  expect_equal(reference$doi, "10.1183/09031936.00114713")
})


test_that("historical registry requires internal lifecycle and visibility", {
  registry <- make_historical_registry_row()

  expect_invisible(.validate_historical_crf_registry(registry))
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(registry, lifecycle = "reviewed_public")
    ),
    "lifecycle 'historical_reproduction'"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(registry, visibility = "public")
    ),
    "visibility 'internal_compatibility'"
  )
})


test_that("historical reproduction and scientific review validate separately", {
  verified <- make_historical_registry_row()
  not_verified <- make_historical_registry_row(
    reproduction_status = "not_verified",
    reproduction_blocking_reason = "Registry parity is incomplete."
  )

  expect_invisible(.validate_historical_crf_registry(verified))
  expect_invisible(.validate_historical_crf_registry(not_verified))
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(
        not_verified,
        reproduction_blocking_reason = NA_character_
      )
    ),
    "must have a reproduction_blocking_reason"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(verified, scientific_review_reason = NA_character_)
    ),
    "must have a scientific_review_reason"
  )
  expect_invisible(
    .validate_historical_crf_registry(
      dplyr::mutate(
        verified,
        scientific_review_status = "reviewed",
        scientific_review_reason = NA_character_
      )
    )
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(verified, provenance_notes = NA_character_)
    ),
    "must include provenance_notes"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(verified, reference_id = "registry_fixture")
    ),
    "Validation fixtures cannot be stored"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(verified, reference_id = "missing_reference")
    ),
    "unknown reference_id"
  )
})


test_that("historical memberships map stable components to stored CRFs", {
  registry <- make_historical_registry_row()
  memberships <- make_historical_membership(registry)

  expect_invisible(
    .validate_historical_crf_memberships(memberships, registry = registry)
  )
  expect_error(
    .validate_historical_crf_memberships(
      dplyr::mutate(memberships, crf_id = "missing_crf"),
      registry = registry
    ),
    "unknown crf_id"
  )
  expect_error(
    .validate_historical_crf_memberships(
      dplyr::bind_rows(memberships, memberships),
      registry = registry
    ),
    "component_id values must be unique"
  )
})


test_that("internal resolver uses parity without requiring scientific approval", {
  ready_registry <- make_historical_registry_row()
  memberships <- make_historical_membership(ready_registry)

  resolved <- .resolve_historical_crf_components(
    component_ids = memberships$component_id,
    registry = ready_registry,
    memberships = memberships
  )
  expect_s3_class(resolved, "creahia_historical_crf_set")
  expect_true(is_crfs_set(resolved))
  expect_equal(resolved$crf_id, ready_registry$crf_id)

  blocked_registry <- make_historical_registry_row(
    reproduction_status = "not_verified",
    reproduction_blocking_reason = "Registry parity is incomplete."
  )
  expect_error(
    .resolve_historical_crf_components(
      component_ids = memberships$component_id,
      registry = blocked_registry,
      memberships = memberships
    ),
    "not parity-verified.*Registry parity is incomplete"
  )
  expect_error(
    .resolve_historical_crf_components(
      component_ids = "rr|missing|Cause|Deaths",
      registry = ready_registry,
      memberships = memberships
    ),
    "absent from internal storage"
  )
})


test_that("historical storage is invisible to public Registry selection", {
  historical_registry <- make_historical_registry_row()
  historical_id <- historical_registry$crf_id
  packaged_historical_ids <- .load_historical_crf_registry()$crf_id

  expect_false(historical_id %in% load_crf_registry()$crf_id)
  expect_false(any(packaged_historical_ids %in% load_crf_registry()$crf_id))
  expect_false(historical_id %in% available_crf_presets())
  expect_equal(nrow(search_crf_registry(query = historical_id)), 0)

  override_options <- crf_override_options(
    presets = "experimental_default"
  )
  expect_false(historical_id %in% override_options$crf_id)

  preview <- preview_crf_set(presets = "experimental_default")
  expect_false(historical_id %in% preview$crf_id)
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      add = list(list(crf_id = historical_id))
    ),
    "Unknown addition crf_id"
  )
  expect_error(
    crfs_set(
      presets = "experimental_default",
      add = list(list(crf_id = historical_id))
    ),
    "Unknown addition crf_id"
  )
})
