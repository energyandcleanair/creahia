
upsert_crf_registry_row <- function(registry_row) {
  upsert_crf_registry_rows(registry_row)
}

upsert_crf_registry_rows <- function(registry_rows) {
  # Update or insert Registry rows as one validated write.

  if (nrow(registry_rows) == 0) {
    stop("registry_rows must contain at least one row.", call. = FALSE)
  }

  duplicated_ids <- unique(
    registry_rows$crf_id[duplicated(registry_rows$crf_id)]
  )
  if (length(duplicated_ids) > 0) {
    stop(
      "registry_rows contains duplicate crf_ids: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  registry_path <- get_hia_path("crf/registry.csv", error_if_not_exists = TRUE)

  registry <- readr::read_csv(registry_path, col_types = readr::cols())

  registry <- registry %>%
    dplyr::filter(!crf_id %in% registry_rows$crf_id) %>%
    dplyr::bind_rows(registry_rows)

  validate_crf_registry(registry)

  readr::write_csv(registry, registry_path)

  invisible(TRUE)
}

upsert_crf_reference_row <- function(reference_row) {
  references_path <- get_hia_path("crf/references.csv", error_if_not_exists = TRUE)

  references <- readr::read_csv(references_path, col_types = readr::cols())

  references <- references %>%
    dplyr::filter(reference_id != reference_row$reference_id) %>%
    dplyr::bind_rows(reference_row)

  validate_crf_references(references)

  readr::write_csv(references, references_path)

  invisible(TRUE)
}


validate_compiled_tabular_crf <- function(
  tbl,
  expected_ages,
  data_name = "Compiled tabular CRF"
) {
  required_cols <- c("exposure", "age", "low", "central", "high")
  missing_cols <- setdiff(required_cols, names(tbl))

  if (length(missing_cols) > 0) {
    stop(
      data_name, " is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(tbl) == 0) {
    stop(data_name, " has no rows.", call. = FALSE)
  }

  if (any(is.na(tbl$age))) {
    stop(data_name, " has missing age values.", call. = FALSE)
  }

  actual_ages <- unique(tbl$age)
  missing_ages <- setdiff(expected_ages, actual_ages)
  unexpected_ages <- setdiff(actual_ages, expected_ages)
  if (length(missing_ages) > 0 || length(unexpected_ages) > 0) {
    details <- c(
      if (length(missing_ages) > 0) {
        paste0("missing: ", paste(missing_ages, collapse = ", "))
      },
      if (length(unexpected_ages) > 0) {
        paste0("unexpected: ", paste(unexpected_ages, collapse = ", "))
      }
    )
    stop(
      data_name, " has invalid age coverage (",
      paste(details, collapse = "; "),
      ").",
      call. = FALSE
    )
  }

  if (any(is.na(tbl$exposure))) {
    stop(data_name, " has missing exposure values.", call. = FALSE)
  }

  rr_cols <- c("low", "central", "high")
  if (any(is.na(as.matrix(tbl[rr_cols])))) {
    stop(data_name, " has missing RR values.", call. = FALSE)
  }

  duplicate_rows <- duplicated(tbl[c("age", "exposure")])
  if (any(duplicate_rows)) {
    stop(data_name, " has duplicate age-exposure rows.", call. = FALSE)
  }

  exposure_by_age <- split(tbl$exposure, tbl$age)
  unsorted_ages <- names(exposure_by_age)[
    vapply(exposure_by_age, is.unsorted, logical(1))
  ]
  if (length(unsorted_ages) > 0) {
    stop(
      data_name, " exposure values must be sorted within each age. ",
      "Problem ages: ", paste(unsorted_ages, collapse = ", "),
      call. = FALSE
    )
  }

  reference_grid <- exposure_by_age[[expected_ages[[1]]]]
  mismatched_grids <- names(exposure_by_age)[
    !vapply(exposure_by_age, identical, logical(1), reference_grid)
  ]
  if (length(mismatched_grids) > 0) {
    stop(
      data_name, " ages must use the same exposure grid. Problem ages: ",
      paste(mismatched_grids, collapse = ", "),
      call. = FALSE
    )
  }

  bad_rr <- tbl$low <= 0 | tbl$central <= 0 | tbl$high <= 0
  if (any(bad_rr)) {
    stop(data_name, " has non-positive RR values.", call. = FALSE)
  }

  bad_order <- !(tbl$low <= tbl$central & tbl$central <= tbl$high)
  if (any(bad_order)) {
    stop(data_name, " has invalid RR ordering.", call. = FALSE)
  }

  invisible(TRUE)
}
