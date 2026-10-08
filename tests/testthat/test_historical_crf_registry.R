make_historical_registry_row <- function(
  readiness_status = "registry_ready",
  blocking_reason = NA_character_
) {
  load_crf_registry() %>%
    dplyr::filter(form == CRF_FORM_LOG_LINEAR) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      crf_id = "historical_test_crf_v1",
      lifecycle = "historical_reproduction",
      visibility = "internal_compatibility",
      readiness_status = readiness_status,
      blocking_reason = blocking_reason,
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


test_that("packaged historical stores contain the blocked default profile", {
  registry <- .load_historical_crf_registry()
  memberships <- .load_historical_crf_memberships()

  expect_equal(nrow(registry), 15)
  expect_equal(nrow(memberships), 15)
  expect_true(all(c(
    CRF_REGISTRY_REQUIRED_COLUMNS,
    .historical_registry_metadata_columns()
  ) %in% names(registry)))
  expect_named(memberships, .historical_membership_columns())
  expect_true(all(registry$lifecycle == "historical_reproduction"))
  expect_true(all(registry$visibility == "internal_compatibility"))
  expect_true(all(registry$readiness_status == "not_ready"))
  expect_true(all(!is.na(registry$blocking_reason)))
  expect_true(all(!is.na(registry$question_ids)))
  expect_setequal(
    registry$crf_id,
    sprintf("historical_default_l%02d_v1", seq_len(15))
  )
})


test_that("packaged default definitions preserve the legacy numeric contract", {
  registry <- .load_historical_crf_registry()
  legacy <- get_crfs("default")
  contract_columns <- c(
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

  stored_contract <- registry %>%
    dplyr::select(dplyr::all_of(contract_columns)) %>%
    dplyr::arrange(pollutant, cause, outcome)
  legacy_contract <- legacy %>%
    dplyr::select(dplyr::all_of(contract_columns)) %>%
    dplyr::arrange(pollutant, cause, outcome)

  expect_equal(stored_contract, legacy_contract)
  expect_true(all(is.na(registry$age_low)))
  expect_true(all(is.na(registry$age_high)))
  expect_true(all(registry$form == CRF_FORM_LOG_LINEAR))
  expect_true(all(registry$reference_id %in% load_crf_references()$reference_id))
})


test_that("packaged default memberships cover every stable component identity", {
  registry <- .load_historical_crf_registry()
  memberships <- .load_historical_crf_memberships()
  expected_components <- .normalise_crf_components(
    get_crfs("default"),
    input_mode = "legacy_profile",
    selection_id = "legacy_profile_default"
  )

  expect_setequal(memberships$component_id, expected_components$component_id)
  expect_true(all(memberships$selection_id == "legacy_profile_default"))
  expect_setequal(memberships$crf_id, registry$crf_id)
  expect_error(
    .resolve_historical_crf_components(
      component_ids = memberships$component_id,
      registry = registry,
      memberships = memberships
    ),
    "not Registry-ready"
  )
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


test_that("historical readiness and provenance fail closed", {
  ready <- make_historical_registry_row()
  not_ready <- make_historical_registry_row(
    readiness_status = "not_ready",
    blocking_reason = "Reference mapping is unresolved."
  )

  expect_invisible(.validate_historical_crf_registry(ready))
  expect_invisible(.validate_historical_crf_registry(not_ready))
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(not_ready, blocking_reason = NA_character_)
    ),
    "must have a blocking_reason"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(ready, provenance_notes = NA_character_)
    ),
    "must include provenance_notes"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(ready, reference_id = "registry_fixture")
    ),
    "Validation fixtures cannot be stored"
  )
  expect_error(
    .validate_historical_crf_registry(
      dplyr::mutate(ready, reference_id = "missing_reference")
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


test_that("internal resolver returns only complete historical components", {
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
    readiness_status = "not_ready",
    blocking_reason = "Method review is pending."
  )
  expect_error(
    .resolve_historical_crf_components(
      component_ids = memberships$component_id,
      registry = blocked_registry,
      memberships = memberships
    ),
    "not Registry-ready.*Method review is pending"
  )
  expect_error(
    .resolve_historical_crf_components(
      component_ids = "rr|missing|Cause|Deaths|25-29",
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
