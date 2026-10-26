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


# A calculation decision, never CRF metadata. Rebuild from the final selection
# on every invocation so filtered/extended Sets cannot retain stale flags.
.resolve_hia_overlap_plan <- function(slots, dc_groups = load_dc_groups()) {
  # Build a plain table: Set classes/provenance must not leak into the plan.
  slots <- tibble::tibble(
    pollutant = slots$pollutant, cause = slots$cause, outcome = slots$outcome
  ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(pollutant, cause, outcome)
  conflicts <- find_dc_conflicts(slots, dc_groups = dc_groups)
  excluded <- slots[0, ]
  if (nrow(conflicts) > 0) {
    excluded <- conflicts %>%
      dplyr::select(pollutant, outcome, components) %>%
      tidyr::unnest_longer(components, values_to = "cause") %>%
      dplyr::distinct(pollutant, cause, outcome)
  }
  plan <- slots %>%
    dplyr::left_join(
      dplyr::mutate(excluded, double_counted = TRUE),
      by = c("pollutant", "cause", "outcome"),
      relationship = "one-to-one"
    ) %>%
    dplyr::mutate(double_counted = dplyr::coalesce(double_counted, FALSE))
  coverage <- slots[0, ] %>% dplyr::mutate(covered_by = character())
  if (nrow(conflicts) > 0) {
    coverage <- conflicts %>%
      dplyr::select(pollutant, outcome, aggregates, components) %>%
      tidyr::unnest_longer(aggregates, values_to = "covered_by") %>%
      tidyr::unnest_longer(components, values_to = "cause") %>%
      dplyr::semi_join(
        dplyr::filter(plan, !double_counted),
        by = c("pollutant", "outcome", "covered_by" = "cause")
      ) %>%
      dplyr::distinct(pollutant, cause, outcome, covered_by) %>%
      dplyr::arrange(pollutant, cause, outcome, covered_by)
  }
  attr(plan, "coverage") <- coverage
  plan
}


.attach_hia_overlap_plan <- function(hia, overlap_plan) {
  keys <- c("pollutant", "cause", "outcome")
  unknown <- dplyr::anti_join(hia, overlap_plan, by = keys)
  if (nrow(unknown) > 0) {
    stop("HIA results contain cause slots absent from the overlap plan.", call. = FALSE)
  }
  # Validation only: never change the initial selection decision or substitute
  # components when an expected ensemble is missing in a region/estimate.
  coverage <- attr(overlap_plan, "coverage", exact = TRUE)
  if (!is.null(coverage) && nrow(coverage) > 0 && "number" %in% names(hia)) {
    context <- intersect(c("scenario", "region_id", "estimate"), names(hia))
    valid <- dplyr::filter(hia, !is.na(number))
    required <- valid %>%
      dplyr::select(dplyr::all_of(c(context, keys))) %>%
      dplyr::inner_join(coverage, by = keys, relationship = "many-to-many")
    covered <- required %>%
      dplyr::semi_join(
        valid,
        by = c(stats::setNames(context, context),
               pollutant = "pollutant", outcome = "outcome", covered_by = "cause")
      )
    missing <- dplyr::anti_join(required, covered, by = c(context, keys))
    if (nrow(missing) > 0) {
      stop("A selected ensemble is missing for excluded component results; cannot produce a complete total.",
           call. = FALSE)
    }
  }
  attr(overlap_plan, "coverage") <- NULL
  hia %>%
    dplyr::select(-dplyr::any_of("double_counted")) %>%
    dplyr::left_join(overlap_plan, by = keys, relationship = "many-to-one")
}
