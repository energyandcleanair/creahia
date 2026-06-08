CRF_DC_GROUP_REQUIRED_COLUMNS <- c(
  "dc_group",
  "pollutant",
  "cause",
  "outcome",
  "role",
  "notes"
)

CRF_DC_ROLE_AGGREGATE <- "aggregate"
CRF_DC_ROLE_COMPONENT <- "component"
CRF_DC_ROLE_MEMBER <- "member"


check_dc_conflicts <- function(crfs, dc_groups = load_dc_groups()) {
  validate_dc_groups(dc_groups)

  if (nrow(crfs) == 0) {
    return(invisible(TRUE))
  }

  selected_groups <- crfs %>%
    dplyr::inner_join(
      dc_groups,
      by = c("pollutant", "cause", "outcome"),
      relationship = "many-to-many"
    )

  if (nrow(selected_groups) == 0) {
    return(invisible(TRUE))
  }

  conflicts <- selected_groups %>%
    dplyr::group_by(dc_group) %>%
    dplyr::summarise(
      aggregates = list(unique(cause[role == CRF_DC_ROLE_AGGREGATE])),
      components = list(unique(cause[role == CRF_DC_ROLE_COMPONENT])),
      n_aggregates = length(unique(cause[role == CRF_DC_ROLE_AGGREGATE])),
      n_components = length(unique(cause[role == CRF_DC_ROLE_COMPONENT])),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      n_aggregates > 1 |
        (n_aggregates >= 1 & n_components >= 1)
    )

  if (nrow(conflicts) == 0) {
    return(invisible(TRUE))
  }

  messages <- purrr::pmap_chr(
    conflicts,
    function(dc_group, aggregates, components, n_aggregates, n_components) {
      paste0(
        "Conflict in dc_group '", dc_group, "': ",
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

  valid_roles <- c(
    CRF_DC_ROLE_AGGREGATE,
    CRF_DC_ROLE_COMPONENT,
    CRF_DC_ROLE_MEMBER
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
    dplyr::count(dc_group, pollutant, cause, outcome, role) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicated_rows) > 0) {
    stop(
      "CRF double-counting groups file has duplicated rows.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
