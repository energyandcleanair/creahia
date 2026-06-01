compile_gemm_ihd_25plus_deaths <- function(
  overwrite = FALSE, 
  update_registry = FALSE,
  update_references = FALSE
  ) {
  # This function compiles the GEMM IHD 25+ deaths CRF from the source CSV file, validates it, 
  # saves it to the appropriate location, and optionally updates the CRF registry with a new entry for this CRF.

  crf_id <- "gemm_pm25_ihd_25plus_deaths_v1"
  source_path <- get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE)

  output_tabular_file <- "crf/tabular/gemm_pm25_ihd_25plus_deaths_v1.csv"
  output_tabular_path <- get_hia_path(output_tabular_file, error_if_not_exists = FALSE)

  if (file.exists(output_tabular_path) && !overwrite) {
    stop(
      "Output already exists: ", output_tabular_path,
      ". Use overwrite = TRUE to regenerate it.",
      call. = FALSE
    )
  }

  rr <- readr::read_csv(source_path, col_types = readr::cols())

  gemm_ihd <- rr %>%
    dplyr::filter(cause == "IHD", age == "25+", source == "gemm") %>%
    dplyr::arrange(exposure) %>%
    dplyr::select(exposure, age, low, central, high)

  validate_compiled_gemm_ihd(gemm_ihd)

  dir.create(dirname(output_tabular_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(gemm_ihd, output_tabular_path)

  registry_row <- tibble::tibble(
    crf_id = crf_id,
    pollutant = "PM25",
    cause = "IHD",
    outcome = "Deaths",
    age_low = 25,
    age_high = Inf,
    region_applicability = "global",
    form = CRF_FORM_TABULAR,
    rr_central = NA_real_,
    rr_low = NA_real_,
    rr_high = NA_real_,
    conc_change = NA_real_,
    counterfact = NA_real_,
    unit = "ug",
    units_multiplier = 1,
    data_path = output_tabular_file,
    reference_id = "burnett_2018_gemm",
    notes = "Migrated from inst/extdata/rr/processed/rr_gemm.csv; IHD 25+ GEMM curve"
  )

  if (update_references) {
    upsert_crf_reference_row(gemm_reference_row())
  }
  
  if (update_registry) {
    upsert_crf_registry_row(registry_row)
  }

  

  registry_row
}

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

validate_compiled_gemm_ihd <- function(tbl) {
  required_cols <- c("exposure", "age", "low", "central", "high")
  missing_cols <- setdiff(required_cols, names(tbl))

  if (length(missing_cols) > 0) {
    stop(
      "Compiled GEMM table is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(tbl) == 0) {
    stop("Compiled GEMM table has no rows.", call. = FALSE)
  }

  if (any(is.na(tbl$exposure))) {
    stop("Compiled GEMM table has missing exposure values.", call. = FALSE)
  }

  if (is.unsorted(tbl$exposure)) {
    stop("Compiled GEMM exposure values must be sorted.", call. = FALSE)
  }

  bad_rr <- tbl$low <= 0 | tbl$central <= 0 | tbl$high <= 0
  if (any(bad_rr)) {
    stop("Compiled GEMM table has non-positive RR values.", call. = FALSE)
  }

  bad_order <- !(tbl$low <= tbl$central & tbl$central <= tbl$high)
  if (any(bad_order)) {
    stop("Compiled GEMM table has invalid RR ordering.", call. = FALSE)
  }

  invisible(TRUE)
}

gemm_reference_row <- function() {
  tibble::tibble(
    reference_id = "burnett_2018_gemm",
    author = "Burnett R, Chen H, Szyszkowicz M, et al.",
    year = 2018,
    title = "Global estimates of mortality associated with long-term exposure to outdoor fine particulate matter",
    journal = "PNAS",
    volume = "115",
    pages = "9592-9597",
    doi = "10.1073/pnas.1803222115",
    url = NA_character_,
    notes = "GEMM source for PM2.5 mortality concentration-response functions"
  )
}