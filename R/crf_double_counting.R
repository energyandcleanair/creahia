CRF_DC_GROUP_REQUIRED_COLUMNS <- c(
  "dc_group",
  "cause",
  "role",
  "notes"
)

CRF_DC_ROLE_AGGREGATE <- "aggregate"
CRF_DC_ROLE_COMPONENT <- "component"


check_dc_conflicts <- function(crfs, dc_groups = load_dc_groups()) {
  conflicts <- find_dc_conflicts(crfs, dc_groups = dc_groups)

  if(nrow(conflicts) == 0) {
    return(invisible(TRUE))
  }

  messages <- purrr::pmap_chr(
    conflicts,
    function(pollutant, outcome, dc_group, aggregates, components,
             n_aggregates, n_components) {
      paste0(
        "Conflict in pollutant/outcome group '",
        pollutant, "/", outcome,
        "' for dc_group '", dc_group, "': ",
        "aggregate cause(s) [", paste(aggregates, collapse = ", "), "] ",
        "selected together with component cause(s) [",
        paste(components, collapse = ", "),
        "]. Pick either the aggregate or its components, not both."
      )
    }
  )

  stop(
    paste(messages, collapse = "\n"),
    call. = FALSE
  )
}

find_dc_conflicts <-  function(crfs, dc_groups = load_dc_groups()) {
  validate_dc_groups(dc_groups)

  if (nrow(crfs) == 0) {
    return(tibble::tibble())
  }

  required_crf_cols <- c("pollutant", "cause", "outcome")
  missing_crf_cols <- setdiff(required_crf_cols, names(crfs))

  if (length(missing_crf_cols) > 0) {
    stop(
      "CRF selection is missing required columns for double-counting checks: ",
      paste(missing_crf_cols, collapse = ", "),
      call. = FALSE
    )
  }

  selected_groups <- crfs %>%
    dplyr::select(dplyr::all_of(required_crf_cols)) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      dc_groups,
      by = "cause",
      relationship = "many-to-many"
    )

  if (nrow(selected_groups) == 0) {
    return(tibble::tibble())
  }

  # For each pollutant/outcome/dc_group, check whether an aggregate and
  # one or more component causes were selected together.
  selected_groups %>%
    dplyr::group_by(pollutant, outcome, dc_group) %>%
    dplyr::summarise(
      aggregates = list(sort(unique(cause[role == CRF_DC_ROLE_AGGREGATE]))),
      components = list(sort(unique(cause[role == CRF_DC_ROLE_COMPONENT]))),
      n_aggregates = length(unique(cause[role == CRF_DC_ROLE_AGGREGATE])),
      n_components = length(unique(cause[role == CRF_DC_ROLE_COMPONENT])),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_aggregates >= 1 & n_components >= 1)

}

describe_dc_group <- function(dc_group, dc_groups = load_dc_groups()) {
  validate_dc_groups(dc_groups)

  result <- dc_groups %>%
    dplyr::filter(dc_group == !!dc_group) %>%
    dplyr::arrange(role, cause)

  if (nrow(result) == 0) {
    stop("Unknown dc_group: ", dc_group, call. = FALSE)
  }

  result
}

load_dc_groups <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path("crf/dc_groups.csv", error_if_not_exists = TRUE)
  }

  dc_groups <- readr::read_csv(path, col_types = readr::cols())

  if (validate) {
    validate_dc_groups(dc_groups)
  }

  dc_groups
}

validate_dc_groups <- function(dc_groups) {
  missing_cols <- setdiff(CRF_DC_GROUP_REQUIRED_COLUMNS, names(dc_groups))

  if (length(missing_cols) > 0) {
    stop(
      "CRF double-counting groups file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  deprecated_cols <- intersect(c("pollutant", "outcome"), names(dc_groups))

  if (length(deprecated_cols) > 0) {
    stop(
      "CRF double-counting groups file should not contain deprecated columns: ",
      paste(deprecated_cols, collapse = ", "),
      call. = FALSE
    )
  }

  valid_roles <- c(
    CRF_DC_ROLE_AGGREGATE,
    CRF_DC_ROLE_COMPONENT
  )

  invalid_roles <- setdiff(unique(dc_groups$role), valid_roles)

  if (length(invalid_roles) > 0) {
    stop(
      "CRF double-counting groups file has invalid role values: ",
      paste(invalid_roles, collapse = ", "),
      ". Valid values are: ",
      paste(valid_roles, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_rows <- dc_groups %>%
    dplyr::count(dc_group, cause, role) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicated_rows) > 0) {
    stop(
      "CRF double-counting groups file has duplicated rows.",
      call. = FALSE
    )
  }

  group_summary <- dc_groups %>%
    dplyr::group_by(dc_group) %>%
    dplyr::summarise(
      n_aggregates = sum(role == CRF_DC_ROLE_AGGREGATE),
      n_components = sum(role == CRF_DC_ROLE_COMPONENT),
      .groups = "drop"
    )

  invalid_groups <- group_summary %>%
    dplyr::filter(n_aggregates != 1 | n_components < 1)

  if (nrow(invalid_groups) > 0) {
    stop(
      "Each CRF double-counting group must have exactly one aggregate and at least one component.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
