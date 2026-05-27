CRF_REGISTRY_REQUIRED_COLUMNS <- c(
  "crf_id",
  "pollutant",
  "cause",
  "outcome",
  "age_low",
  "age_high",
  "region_applicability",
  "form",
  "rr_central",
  "rr_low",
  "rr_high",
  "conc_ref",
  "counterfact",
  "unit",
  "units_multiplier",
  "data_path",
  "reference_id",
  "notes"
)

CRF_REFERENCE_REQUIRED_COLUMNS <- c(
  "reference_id",
  "author",
  "year",
  "title",
  "journal",
  "volume",
  "pages",
  "doi",
  "url",
  "notes"
)


load_crf_registry <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path("crf/registry.csv", error_if_not_exists = TRUE)
  }

  registry <- readr::read_csv(path, col_types = readr::cols())

  if (validate) {
    validate_crf_registry(registry)
  }

  registry
}


load_crf_references <- function(path = NULL, validate = TRUE) {
  if (is.null(path)) {
    path <- get_hia_path("crf/references.csv", error_if_not_exists = TRUE)
  }

  references <- readr::read_csv(path, col_types = readr::cols())

  if (validate) {
    validate_crf_references(references)
  }

  references
}

validate_crf_registry <- function(registry, references = load_crf_references()) {
  # Check that all required columns are present
  missing_cols <- setdiff(CRF_REGISTRY_REQUIRED_COLUMNS, names(registry))
  if (length(missing_cols) > 0) {
    stop(
      "CRF registry file is missing the following required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Check that there are no duplicate crf_ids
  duplicated_ids <- registry$crf_id[duplicated(registry$crf_id)]
  if (length(duplicated_ids) > 0) {
    stop(
      "CRF registry file contains duplicate crf_ids: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  # Check that column form is one of "log-linear" and "tabular"
  if (!all(registry$form %in% c(CRF_FORM_LOG_LINEAR, CRF_FORM_TABULAR))) {
    invalid_forms <- unique(registry$form[!registry$form %in% c(CRF_FORM_LOG_LINEAR, CRF_FORM_TABULAR)])
    stop(
      "CRF registry file contains invalid form values: ",
      paste(invalid_forms, collapse = ", "),
      ". Valid values are 'log_linear' and 'tabular'.",
      call. = FALSE
    )
  }

  # Check that reference_id exists both in registry and in references table
  missing_reference_ids <- setdiff(registry$reference_id, references$reference_id)
  if (length(missing_reference_ids) > 0) {
    stop(
      "CRF registry file contains reference_ids that are not present in the references file: ",
      paste(missing_reference_ids, collapse = ", "),
      call. = FALSE
    )
  }

  validate_log_linear_crfs(registry)
  validate_tabular_crfs(registry)
}

validate_crf_references <- function(references) {
  # Check that all required columns are present
  missing_cols <- setdiff(CRF_REFERENCE_REQUIRED_COLUMNS, names(references))

  if (length(missing_cols) > 0) {
    stop(
      "CRF references file is missing the following required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Check that there are no duplicate reference_ids
  duplicated_ids <- references$reference_id[duplicated(references$reference_id)]
  if (length(duplicated_ids) > 0) {
    stop(
      "CRF references file contains duplicate reference_ids: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_log_linear_crfs <- function(registry) {
  # Check that for log-linear CRFs, the form is log-linear
  log_linear <- registry %>% filter(form == CRF_FORM_LOG_LINEAR) 

  if (nrow(log_linear) == 0) {
    return(invisible(TRUE))
  }

  required_numeric_cols <- c("rr_low", "rr_central", "rr_high", "conc_ref", "counterfact", "units_multiplier")

  for(col in required_numeric_cols){
    bad_rows <- is.na(log_linear[[col]])
    if (any(bad_rows)) {
      stop(
        "Log-linear CRFs must have non-missing ", col, ". Problem crf_id values: ",
        paste(log_linear$crf_id[bad_rows], collapse = ", "),
        call. = FALSE
      )
    }  
  }
  

  # Check that rr_low, rr_central, and rr_high are all > 0
  bad_rr <- log_linear$rr_central <= 0 |
    log_linear$rr_low <= 0 |
    log_linear$rr_high <= 0

  if (any(bad_rr)) {
    stop(
      "Log-linear CRFs must have positive RR values. Problem crf_id values: ",
      paste(log_linear$crf_id[bad_rr], collapse = ", "),
      call. = FALSE
    )
  }

  # Check that rr_low <= rr_central <= rr_high for all rows
  invalid_rr_rows <- registry %>%
    filter(!(rr_low <= rr_central & rr_central <= rr_high)) %>%
    select(crf_id, rr_low, rr_central, rr_high)
  if (nrow(invalid_rr_rows) > 0) {
    stop(
      "CRF registry file contains rows where rr_low, rr_central, and rr_high are not properly ordered (rr_low <= rr_central <= rr_high). Problematic rows: ",
      paste(apply(invalid_rr_rows, 1, paste, collapse = ", "), collapse = "; "),
      call. = FALSE
    )
  }

  # Check that conc_ref, counterfact, and units_multiplier are all >= 0
  bad_conc <- log_linear$conc_ref < 0 |
    log_linear$counterfact < 0 |
    log_linear$units_multiplier < 0
  if (any(bad_conc)) {
    stop(
      "Log-linear CRFs must have non-negative concentration values and units_multiplier. Problem crf_id values: ",
      paste(log_linear$crf_id[bad_conc], collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_tabular_crfs <- function(registry) {
  # Check that for tabular CRFs, the form is tabular and that data_path is not NA
  tabular <- registry %>% filter(form == "tabular")

  if (nrow(tabular) == 0) {
    return(invisible(TRUE))
  }

  if (any(is.na(tabular$data_path))) {
    stop(
      "Tabular CRFs must have a non-missing data_path. Problem crf_id values: ",
      paste(tabular$crf_id[is.na(tabular$data_path)], collapse = ", "),
      call. = FALSE
    )
  }

  paths <- vapply(
    tabular$data_path,
    get_hia_path,
    character(1),
    error_if_not_exists = FALSE
  )

  missing_files <- !file.exists(paths)
  if (any(missing_files)) {
    stop(
      "Tabular CRF data_path files do not exist. Problem crf_id values: ",
      paste(tabular$crf_id[missing_files], collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
