
upsert_crf_registry_row <- function(registry_row) {
  # This function takes a single row of CRF registry data (as a tibble) 
  # and either updates the existing entry in the CRF registry with the same crf_id 
  # or inserts it if it doesn't exist. It validates the updated registry and saves it back to the CSV file.

  registry_path <- get_hia_path("crf/registry.csv", error_if_not_exists = TRUE)

  registry <- readr::read_csv(registry_path, col_types = readr::cols())

  registry <- registry %>%
    dplyr::filter(crf_id != registry_row$crf_id) %>%
    dplyr::bind_rows(registry_row)

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
