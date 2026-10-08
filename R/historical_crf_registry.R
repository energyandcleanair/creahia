.historical_registry_metadata_columns <- function() {
  c(
    "lifecycle",
    "visibility",
    "readiness_status",
    "blocking_reason",
    "question_ids",
    "provenance_notes"
  )
}


.historical_membership_columns <- function() {
  c("component_id", "selection_id", "crf_id", "notes")
}


.require_historical_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      label, " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


.load_historical_crf_registry <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path(
      "crf/compatibility/historical_registry.csv",
      error_if_not_exists = TRUE
    )
  }

  registry <- readr::read_csv(path, col_types = readr::cols())
  if (validate) {
    .validate_historical_crf_registry(registry)
  }
  registry
}


.load_historical_crf_memberships <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path(
      "crf/compatibility/historical_memberships.csv",
      error_if_not_exists = TRUE
    )
  }

  memberships <- readr::read_csv(path, col_types = readr::cols())
  if (validate) {
    .validate_historical_crf_memberships(memberships)
  }
  memberships
}


.validate_historical_crf_registry <- function(registry) {
  required <- c(
    CRF_REGISTRY_REQUIRED_COLUMNS,
    .historical_registry_metadata_columns()
  )
  .require_historical_columns(registry, required, "Historical CRF registry")

  if (nrow(registry) == 0) {
    return(invisible(TRUE))
  }

  duplicated_ids <- unique(registry$crf_id[duplicated(registry$crf_id)])
  if (length(duplicated_ids) > 0) {
    stop(
      "Historical CRF registry contains duplicate crf_id values: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(is.na(registry$crf_id) | registry$crf_id == "")) {
    stop("Historical CRF registry contains an empty crf_id.", call. = FALSE)
  }
  if (any(registry$lifecycle != "historical_reproduction")) {
    stop(
      "Historical CRF rows must use lifecycle 'historical_reproduction'.",
      call. = FALSE
    )
  }
  if (any(registry$visibility != "internal_compatibility")) {
    stop(
      "Historical CRF rows must use visibility 'internal_compatibility'.",
      call. = FALSE
    )
  }

  allowed_statuses <- c("not_ready", "registry_ready")
  if (!all(registry$readiness_status %in% allowed_statuses)) {
    stop(
      "Historical CRF readiness_status must be 'not_ready' or 'registry_ready'.",
      call. = FALSE
    )
  }

  ready_with_blocker <- registry$readiness_status == "registry_ready" &
    !is.na(registry$blocking_reason)
  not_ready_without_blocker <- registry$readiness_status == "not_ready" &
    (is.na(registry$blocking_reason) | registry$blocking_reason == "")
  if (any(ready_with_blocker)) {
    stop(
      "Registry-ready historical CRFs must not retain a blocking_reason.",
      call. = FALSE
    )
  }
  if (any(not_ready_without_blocker)) {
    stop(
      "Not-ready historical CRFs must have a blocking_reason.",
      call. = FALSE
    )
  }

  missing_provenance <- is.na(registry$provenance_notes) |
    registry$provenance_notes == ""
  if (any(missing_provenance)) {
    stop(
      "Historical CRFs must include provenance_notes.",
      call. = FALSE
    )
  }

  fixture_rows <- registry$reference_id %in% CRF_INTERNAL_REFERENCE_IDS
  if (any(fixture_rows)) {
    stop(
      "Validation fixtures cannot be stored as historical-reproduction CRFs: ",
      paste(registry$crf_id[fixture_rows], collapse = ", "),
      call. = FALSE
    )
  }

  invalid_forms <- setdiff(unique(registry$form), CRF_FORM_VALUES)
  if (length(invalid_forms) > 0) {
    stop(
      "Historical CRF registry contains invalid form values: ",
      paste(invalid_forms, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(is.na(registry$double_counted))) {
    stop(
      "Historical CRFs must have non-missing double_counted values.",
      call. = FALSE
    )
  }

  validate_log_linear_crfs(registry)
  validate_tabular_crfs(registry)
  invisible(TRUE)
}


.validate_historical_crf_memberships <- function(
  memberships,
  registry = NULL
) {
  .require_historical_columns(
    memberships,
    .historical_membership_columns(),
    "Historical CRF memberships"
  )

  if (nrow(memberships) == 0) {
    return(invisible(TRUE))
  }

  required_values <- c("component_id", "selection_id", "crf_id")
  missing_value <- Reduce(
    `|`,
    lapply(required_values, function(column) {
      is.na(memberships[[column]]) | memberships[[column]] == ""
    })
  )
  if (any(missing_value)) {
    stop(
      "Historical CRF memberships contain an empty identity field.",
      call. = FALSE
    )
  }

  duplicated_components <- unique(
    memberships$component_id[duplicated(memberships$component_id)]
  )
  if (length(duplicated_components) > 0) {
    stop(
      "Historical component_id values must be unique: ",
      paste(duplicated_components, collapse = ", "),
      call. = FALSE
    )
  }

  valid_component_id <- grepl(
    "^(crf_profile|rr|crf)\\|",
    memberships$component_id
  )
  if (!all(valid_component_id)) {
    stop(
      "Historical memberships contain invalid component_id values: ",
      paste(memberships$component_id[!valid_component_id], collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(registry)) {
    .validate_historical_crf_registry(registry)
    unknown_ids <- setdiff(memberships$crf_id, registry$crf_id)
    if (length(unknown_ids) > 0) {
      stop(
        "Historical memberships reference unknown crf_id values: ",
        paste(unknown_ids, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}


.resolve_historical_crf_components <- function(
  component_ids,
  registry = .load_historical_crf_registry(),
  memberships = .load_historical_crf_memberships()
) {
  .validate_historical_crf_registry(registry)
  .validate_historical_crf_memberships(memberships, registry = registry)

  component_ids <- unique(as.character(component_ids))
  if (length(component_ids) == 0 || any(is.na(component_ids))) {
    stop("component_ids must contain at least one non-missing ID.", call. = FALSE)
  }

  missing_components <- setdiff(component_ids, memberships$component_id)
  if (length(missing_components) > 0) {
    stop(
      "Historical components are absent from internal storage: ",
      paste(missing_components, collapse = ", "),
      call. = FALSE
    )
  }

  selected_memberships <- memberships %>%
    dplyr::filter(component_id %in% component_ids) %>%
    dplyr::select(component_id, selection_id, crf_id)
  selected <- selected_memberships %>%
    dplyr::inner_join(registry, by = "crf_id")

  blocked <- selected %>%
    dplyr::filter(readiness_status != "registry_ready")
  if (nrow(blocked) > 0) {
    details <- paste0(
      blocked$component_id,
      ": ",
      blocked$blocking_reason
    )
    stop(
      "Historical components are not Registry-ready: ",
      paste(details, collapse = "; "),
      call. = FALSE
    )
  }

  crfs <- selected %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    dplyr::distinct(crf_id, .keep_all = TRUE)
  validate_crf_selection(crfs)

  crfs <- new_crfs_set(crfs)
  class(crfs) <- unique(c("creahia_historical_crf_set", class(crfs)))
  crfs
}
