CRF_SELECTION_REQUIRED_COLUMNS <- c("pollutant", "cause", "crf_id")

# specs for testing purposes, not intended for general use
crf_bundle_specs <- function() {
  tibble::tribble(
    ~bundle, ~pollutant, ~cause,     ~crf_id,
    # The "experimental_default" bundle is intended for testing and experimentation, and may change without warning. It is not intended for general use.
    "experimental_default", "PM25", "IHD",     "gemm_pm25_ihd_25plus_deaths_v1",
    "experimental_default", "NO2",  "NCD.LRI", "legacy_no2_ncdlri_deaths_v1",

    # Temporary alias while the registry is still incomplete.
    "default", "PM25", "IHD",     "gemm_pm25_ihd_25plus_deaths_v1",
    "default", "NO2",  "NCD.LRI", "legacy_no2_ncdlri_deaths_v1"
  )
}

crfs_bundle <- function(name = "experimental_default", registry = load_crf_registry()) {
  specs <- crf_bundle_specs()

  available_bundles <- sort(unique(specs$bundle))

  if (!name %in% available_bundles) {
    stop(
      "Unknown CRF bundle: ",
      name,
      ". Available bundles: ",
      paste(available_bundles, collapse = ", "),
      call. = FALSE
    )
  }

  selection <- specs %>%
    dplyr::filter(bundle == name) %>%
    dplyr::select(pollutant, cause, crf_id)

  select_crfs(selection, registry = registry)
}

crfs_override <- function(crfs, pollutant, cause, crf_id, registry = load_crf_registry()) {
  replacement <- select_crfs(
    tibble::tibble(
      pollutant = pollutant,
      cause = cause,
      crf_id = crf_id
    ),
    registry = registry
  )

  updated <- crfs %>%
    dplyr::filter(!(pollutant == !!pollutant & cause == !!cause)) %>%
    dplyr::bind_rows(replacement)

  validate_crf_selection(updated)

  updated
}

select_crfs <- function(selection, registry = load_crf_registry()) {
  missing_cols <- setdiff(CRF_SELECTION_REQUIRED_COLUMNS, names(selection))
  if (length(missing_cols) > 0) {
    stop(
      "CRF selection is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  unknown_ids <- setdiff(selection$crf_id, registry$crf_id)
  if (length(unknown_ids) > 0) {
    stop(
      "Unknown crf_id values: ",
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

  validate_crf_selection(selected)

  selected
}

validate_crf_selection <- function(
  crfs,
  check_double_counting = TRUE,
  dc_groups = load_dc_groups()
) {
  if (nrow(crfs) == 0) {
    stop("CRF selection is empty.", call. = FALSE)
  }

  duplicated_ids <- crfs$crf_id[duplicated(crfs$crf_id)]
  if (length(duplicated_ids) > 0) {
    stop(
      "CRF selection contains duplicated crf_id values: ",
      paste(unique(duplicated_ids), collapse = ", "),
      call. = FALSE
    )
  }

  source_count <- crfs %>%
    dplyr::distinct(pollutant, cause, crf_id) %>%
    dplyr::count(pollutant, cause) %>%
    dplyr::filter(n > 1)

  if (nrow(source_count) > 0) {
    stop(
      "CRF selection has multiple sources for the same pollutant/cause pair: ",
      paste(
        paste(source_count$pollutant, source_count$cause, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (check_double_counting) {
    check_dc_conflicts(crfs, dc_groups = dc_groups)
  }

  invisible(TRUE)
}d


