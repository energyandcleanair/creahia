.historical_rr_manifest_columns <- function() {
  c(
    "component_id",
    "component_type",
    "selection_id",
    "source",
    "cause",
    "outcome",
    "age_contract",
    "lifecycle",
    "visibility",
    "reproduction_status",
    "reproduction_blocking_reason",
    "routing_status",
    "routing_blocking_reason",
    "scientific_review_status",
    "scientific_review_reason",
    "required_by",
    "public_registry_crf_ids",
    "coverage_status",
    "provenance_notes"
  )
}


.historical_rr_registry_columns <- function() {
  c(
    "component_id",
    "source",
    CRF_REGISTRY_REQUIRED_COLUMNS,
    .historical_registry_metadata_columns()
  )
}


.load_historical_rr_registry <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path(
      "rr/compatibility/historical_registry.csv",
      error_if_not_exists = TRUE
    )
  }

  registry <- readr::read_csv(path, col_types = readr::cols())
  if (validate) {
    .validate_historical_rr_registry(registry)
  }
  registry
}


.validate_historical_rr_registry <- function(registry) {
  missing_columns <- setdiff(.historical_rr_registry_columns(), names(registry))
  if (length(missing_columns) > 0) {
    stop(
      "Historical RR registry is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  .validate_historical_crf_registry(
    registry %>%
      dplyr::select(-component_id, -source)
  )

  duplicated_components <- unique(
    registry$component_id[duplicated(registry$component_id)]
  )
  if (length(duplicated_components) > 0) {
    stop(
      "Historical RR registry contains duplicate component_id values: ",
      paste(duplicated_components, collapse = ", "),
      call. = FALSE
    )
  }

  expected_ids <- .component_id(
    "rr",
    registry$source,
    registry$cause,
    registry$outcome
  )
  if (!identical(registry$component_id, expected_ids)) {
    stop(
      "Historical RR Registry component_id values must match source/cause/outcome.",
      call. = FALSE
    )
  }
  if (any(registry$form != CRF_FORM_TABULAR)) {
    stop("Historical RR Registry rows must use tabular CRFs.", call. = FALSE)
  }

  invisible(TRUE)
}


.load_historical_rr_manifest <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path(
      "rr/compatibility/historical_manifest.csv",
      error_if_not_exists = TRUE
    )
  }

  manifest <- readr::read_csv(path, col_types = readr::cols())
  if (validate) {
    .validate_historical_rr_manifest(manifest)
  }
  manifest
}


.validate_historical_rr_manifest <- function(
  manifest,
  historical_registry = .load_historical_rr_registry()
) {
  missing_columns <- setdiff(
    .historical_rr_manifest_columns(),
    names(manifest)
  )
  if (length(missing_columns) > 0) {
    stop(
      "Historical RR manifest is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  .validate_component_manifest(manifest)

  if (any(manifest$component_type != "rr")) {
    stop("Historical RR components must use component_type 'rr'.", call. = FALSE)
  }
  if (any(manifest$selection_id != "historical_rr")) {
    stop(
      "Historical RR components must use selection_id 'historical_rr'.",
      call. = FALSE
    )
  }
  if (any(manifest$lifecycle != "historical_reproduction")) {
    stop(
      "Historical RR components must use lifecycle 'historical_reproduction'.",
      call. = FALSE
    )
  }
  if (any(manifest$visibility != "internal_compatibility")) {
    stop(
      "Historical RR components must use visibility 'internal_compatibility'.",
      call. = FALSE
    )
  }

  expected_ids <- .component_id(
    "rr",
    manifest$source,
    manifest$cause,
    manifest$outcome
  )
  if (!identical(manifest$component_id, expected_ids)) {
    stop(
      "Historical RR component_id values must match source/cause/outcome.",
      call. = FALSE
    )
  }

  canonical_age_contracts <- vapply(
    strsplit(manifest$age_contract, ";", fixed = TRUE),
    .canonical_rr_age_contract,
    character(1)
  )
  if (!identical(manifest$age_contract, canonical_age_contracts)) {
    stop(
      "Historical RR age_contract values must use canonical calculation ages.",
      call. = FALSE
    )
  }

  required_text_columns <- c("required_by", "provenance_notes")
  missing_text <- vapply(required_text_columns, function(column) {
    any(is.na(manifest[[column]]) | manifest[[column]] == "")
  }, logical(1))
  if (any(missing_text)) {
    stop(
      "Historical RR manifest has missing values in: ",
      paste(required_text_columns[missing_text], collapse = ", "),
      call. = FALSE
    )
  }

  allowed_coverage <- c(
    "public_registry_candidate",
    "historical_internal_candidate",
    "historical_internal_required"
  )
  invalid_coverage <- setdiff(unique(manifest$coverage_status), allowed_coverage)
  if (length(invalid_coverage) > 0) {
    stop(
      "Historical RR manifest contains invalid coverage_status values: ",
      paste(invalid_coverage, collapse = ", "),
      call. = FALSE
    )
  }

  internal_with_public_ids <-
    manifest$coverage_status != "public_registry_candidate" &
    !is.na(manifest$public_registry_crf_ids)
  candidate_without_public_ids <-
    manifest$coverage_status == "public_registry_candidate" &
    (is.na(manifest$public_registry_crf_ids) |
      manifest$public_registry_crf_ids == "")
  if (any(internal_with_public_ids)) {
    stop(
      "Internal-only historical RR components cannot reference public candidates.",
      call. = FALSE
    )
  }
  if (any(candidate_without_public_ids)) {
    stop(
      "Public-candidate historical RR components must name a Registry CRF.",
      call. = FALSE
    )
  }
  .validate_historical_rr_registry(historical_registry)
  internal_candidate_ids <- manifest %>%
    dplyr::filter(coverage_status == "historical_internal_candidate") %>%
    dplyr::pull(component_id)
  if (!setequal(internal_candidate_ids, historical_registry$component_id)) {
    stop(
      paste(
        "Historical RR manifest component links must match the internal",
        "RR Registry."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.load_hia_component_manifest <- function(
  historical_crf_registry = .load_historical_crf_registry(),
  historical_crf_memberships = .load_historical_crf_memberships(),
  historical_rr_manifest = .load_historical_rr_manifest()
) {
  .validate_historical_crf_registry(historical_crf_registry)
  .validate_historical_crf_memberships(
    historical_crf_memberships,
    registry = historical_crf_registry
  )
  .validate_historical_rr_manifest(historical_rr_manifest)

  crf_components <- historical_crf_memberships %>%
    dplyr::inner_join(historical_crf_registry, by = "crf_id") %>%
    dplyr::transmute(
      component_id,
      component_type = "crf",
      selection_id,
      source = pollutant,
      cause,
      outcome,
      age_contract = NA_character_,
      lifecycle,
      reproduction_status,
      reproduction_blocking_reason,
      routing_status = dplyr::if_else(
        reproduction_status == "parity_verified",
        "ready",
        "not_ready"
      ),
      routing_blocking_reason = dplyr::if_else(
        reproduction_status == "parity_verified",
        NA_character_,
        reproduction_blocking_reason
      ),
      scientific_review_status,
      scientific_review_reason
    )

  rr_components <- historical_rr_manifest %>%
    dplyr::select(
      component_id,
      component_type,
      selection_id,
      source,
      cause,
      outcome,
      age_contract,
      lifecycle,
      reproduction_status,
      reproduction_blocking_reason,
      routing_status,
      routing_blocking_reason,
      scientific_review_status,
      scientific_review_reason
    )

  manifest <- dplyr::bind_rows(crf_components, rr_components)
  .validate_component_manifest(manifest)
  manifest
}
