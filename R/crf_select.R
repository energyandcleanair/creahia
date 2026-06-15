CRF_SELECTION_REQUIRED_COLUMNS <- c("pollutant", "cause", "crf_id")
CRF_PRESET_REQUIRED_COLUMNS <- c("pollutant", "cause", "crf_id", "notes")

available_crf_presets <- function() {
  preset_dir <- get_hia_path("crf/presets", error_if_not_exists = TRUE)
  preset_files <- list.files(preset_dir, pattern = "\\.csv$", full.names = FALSE)

  sort(sub("\\.csv$", "", preset_files))
}

load_crf_preset <- function(name = "experimental_default") {
  path <- get_hia_path(
    file.path("crf/presets", paste0(name, ".csv")),
    error_if_not_exists = TRUE
  )

  preset <- readr::read_csv(path, col_types = readr::cols())

  missing_cols <- setdiff(CRF_PRESET_REQUIRED_COLUMNS, names(preset))
  if (length(missing_cols) > 0) {
    stop(
      "CRF preset is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  preset
}

crfs_preset <- function(name = "experimental_default", registry = load_crf_registry()) {
  available_presets <- available_crf_presets()

  if (!name %in% available_presets) {
    stop(
      "Unknown CRF preset: ", name,
      ". Available presets: ", paste(available_presets, collapse = ", "),
      call. = FALSE
    )
  }

  selection <- load_crf_preset(name) %>%
    dplyr::select(dplyr::all_of(CRF_SELECTION_REQUIRED_COLUMNS))


  resolve_crf_selection(selection, registry = registry)
  
}

resolve_crf_selection <- function(selection, registry = load_crf_registry()) {
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

crfs_override <- function(crfs, pollutant, cause, crf_id, registry = load_crf_registry()) {
  replacement <- resolve_crf_selection(
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
}


