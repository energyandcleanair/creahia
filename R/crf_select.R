CRF_SELECTION_REQUIRED_COLUMNS <- c("pollutant", "cause", "crf_id")

# specs for testing purposes, not intended for general use
crf_bundle_specs <- function() {
  tibble::tribble(
    ~bundle, ~pollutant, ~cause,     ~crf_id,
    "experimental_default", "PM25", "IHD",     "gemm_pm25_ihd_25plus_deaths_v1",
    "experimental_default", "NO2",  "NCD.LRI", "legacy_no2_ncdlri_deaths_v1",

    # Temporary alias while the registry is still incomplete.
    "default", "PM25", "IHD",     "gemm_pm25_ihd_25plus_deaths_v1",
    "default", "NO2",  "NCD.LRI", "legacy_no2_ncdlri_deaths_v1"
  )
}


select_crfs <- function(selection, registry = load_crf_registry()) {
  missing_cols <- setdiff(CRF_SELECTION_REQUIRED_COLUMNS, colnames(selection))
  if (length(missing_cols) > 0) {
    stop(
      "Selection data frame is missing the following required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  unknown_ids <- setdiff(selection$crf_id, registry$crf_id)
  if (length(unknown_ids) > 0) {
    stop(
      "The following crf_id values in the selection are not present in the registry: ",
      paste(unknown_ids, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_keys <- selection %>%
    dplyr::count(pollutant, cause) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicated_keys) > 0) {
    stop(
      "Each pollutant/cause pair must select exactly one CRF source. Duplicated pairs: ",
      paste(
        paste(duplicated_keys$pollutant, duplicated_keys$cause, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  selected <- registry %>%
    dplyr::inner_join(
      selection, 
      by = c("pollutant", "cause", "crf_id")
    )
  
  missing_matches <- dplyr::anti_join(
    selection,
    registry,
    by = c("pollutant", "cause", "crf_id")
  )

  if (nrow(missing_matches) > 0) {
    stop(
      "Some selections do not match registry pollutant/cause/crf_id rows: ",
      paste(
        paste(missing_matches$pollutant, missing_matches$cause, missing_matches$crf_id, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # validate_crf_selection(selected)

  selected
}


# validate_crf_selection <- function(){

# }