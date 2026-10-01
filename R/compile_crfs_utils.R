
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
