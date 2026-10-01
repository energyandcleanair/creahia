gemm_adult_death_causes <- function() {
  c(
    CAUSE_NCDLRI,
    CAUSE_LRI,
    CAUSE_COPD,
    CAUSE_IHD,
    CAUSE_LUNGCANCER,
    CAUSE_STROKE
  )
}


compile_gemm_ihd_25plus_deaths <- function(
  overwrite = FALSE,
  update_registry = FALSE,
  update_references = FALSE
) {
  compile_gemm_deaths_artifact(
    cause = CAUSE_IHD,
    crf_id = "gemm_pm25_ihd_25plus_deaths_v1",
    ages = AGE_ADULTS,
    output_tabular_file = "crf/tabular/gemm_pm25_ihd_25plus_deaths_v1.csv",
    notes = paste(
      "Migrated from inst/extdata/rr/processed/rr_gemm.csv;",
      "IHD 25+ GEMM curve"
    ),
    overwrite = overwrite,
    update_registry = update_registry,
    update_references = update_references
  )
}


compile_gemm_ihd_adult_deaths <- function(
  overwrite = FALSE,
  update_registry = FALSE,
  update_references = FALSE
) {
  compile_gemm_deaths_artifact(
    cause = CAUSE_IHD,
    crf_id = "gemm_pm25_ihd_adult_deaths_v1",
    ages = c(AGE_ADULTS, AGE_ADULTS_SPLIT),
    output_tabular_file = "crf/tabular/gemm_pm25_ihd_adult_deaths_v1.csv",
    notes = gemm_adult_deaths_notes(CAUSE_IHD),
    overwrite = overwrite,
    update_registry = update_registry,
    update_references = update_references
  )
}


compile_gemm_adult_deaths <- function(
  overwrite = FALSE,
  update_registry = FALSE,
  update_references = FALSE
) {
  source_path <- get_hia_path(
    "rr/processed/rr_gemm.csv",
    error_if_not_exists = TRUE
  )
  rr <- readr::read_csv(source_path, col_types = readr::cols())
  specs <- gemm_adult_deaths_specs()
  expected_ages <- c(AGE_ADULTS, AGE_ADULTS_SPLIT)

  compiled <- stats::setNames(
    lapply(
      specs$cause,
      function(cause) {
        compile_gemm_deaths_data(
          rr,
          cause = cause,
          ages = expected_ages
        )
      }
    ),
    specs$cause
  )

  crf_root <- dirname(
    get_hia_path("crf/registry.csv", error_if_not_exists = TRUE)
  )
  output_paths <- file.path(dirname(crf_root), specs$data_path)
  existing_paths <- output_paths[file.exists(output_paths)]
  if (length(existing_paths) > 0 && !overwrite) {
    stop(
      "GEMM output files already exist: ",
      paste(existing_paths, collapse = ", "),
      ". Use overwrite = TRUE to regenerate the complete set.",
      call. = FALSE
    )
  }

  dir.create(
    dirname(output_paths[[1]]),
    recursive = TRUE,
    showWarnings = FALSE
  )
  for (i in seq_len(nrow(specs))) {
    readr::write_csv(compiled[[specs$cause[[i]]]], output_paths[[i]])
  }

  registry_rows <- dplyr::bind_rows(lapply(
    seq_len(nrow(specs)),
    function(i) {
      gemm_deaths_registry_row(
        cause = specs$cause[[i]],
        crf_id = specs$crf_id[[i]],
        data_path = specs$data_path[[i]],
        notes = gemm_adult_deaths_notes(specs$cause[[i]])
      )
    }
  ))

  if (update_references) {
    upsert_crf_reference_row(gemm_reference_row())
  }

  if (update_registry) {
    upsert_crf_registry_rows(registry_rows)
  }

  registry_rows
}


compile_gemm_ihd_deaths_artifact <- function(
  crf_id,
  ages,
  output_tabular_file,
  notes,
  overwrite,
  update_registry,
  update_references
) {
  compile_gemm_deaths_artifact(
    cause = CAUSE_IHD,
    crf_id = crf_id,
    ages = ages,
    output_tabular_file = output_tabular_file,
    notes = notes,
    overwrite = overwrite,
    update_registry = update_registry,
    update_references = update_references
  )
}


compile_gemm_deaths_artifact <- function(
  cause,
  crf_id,
  ages,
  output_tabular_file,
  notes,
  overwrite,
  update_registry,
  update_references
) {
  source_path <- get_hia_path(
    "rr/processed/rr_gemm.csv",
    error_if_not_exists = TRUE
  )
  output_tabular_path <- get_hia_path(
    output_tabular_file,
    error_if_not_exists = FALSE
  )

  if (file.exists(output_tabular_path) && !overwrite) {
    stop(
      "Output already exists: ", output_tabular_path,
      ". Use overwrite = TRUE to regenerate it.",
      call. = FALSE
    )
  }

  rr <- readr::read_csv(source_path, col_types = readr::cols())
  gemm_cause <- compile_gemm_deaths_data(rr, cause = cause, ages = ages)

  dir.create(
    dirname(output_tabular_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  readr::write_csv(gemm_cause, output_tabular_path)

  registry_row <- gemm_deaths_registry_row(
    cause = cause,
    crf_id = crf_id,
    data_path = output_tabular_file,
    notes = notes
  )

  if (update_references) {
    upsert_crf_reference_row(gemm_reference_row())
  }

  if (update_registry) {
    upsert_crf_registry_row(registry_row)
  }

  registry_row
}


compile_gemm_ihd_deaths_data <- function(
  rr,
  ages = c(AGE_ADULTS, AGE_ADULTS_SPLIT)
) {
  compile_gemm_deaths_data(rr, cause = CAUSE_IHD, ages = ages)
}


compile_gemm_deaths_data <- function(
  rr,
  cause,
  ages = c(AGE_ADULTS, AGE_ADULTS_SPLIT)
) {
  required_source_cols <- c(
    "cause", "source", "exposure", "age", "low", "central", "high"
  )
  missing_source_cols <- setdiff(required_source_cols, names(rr))

  if (length(missing_source_cols) > 0) {
    stop(
      "GEMM source table is missing columns: ",
      paste(missing_source_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!cause %in% gemm_adult_death_causes()) {
    stop(
      "Unsupported GEMM adult death cause: ", cause,
      ". Supported causes: ",
      paste(gemm_adult_death_causes(), collapse = ", "),
      call. = FALSE
    )
  }

  gemm_cause <- rr %>%
    dplyr::filter(
      .data$cause == .env$cause,
      source == RR_GEMM,
      age %in% ages
    ) %>%
    dplyr::mutate(.age_order = match(age, ages)) %>%
    dplyr::arrange(.age_order, exposure) %>%
    dplyr::select(exposure, age, low, central, high) %>%
    tibble::as_tibble()

  validate_compiled_gemm(
    gemm_cause,
    expected_ages = ages,
    data_name = paste("Compiled GEMM", cause, "table")
  )
  gemm_cause
}


validate_compiled_gemm_ihd <- function(
  tbl,
  expected_ages = AGE_ADULTS
) {
  validate_compiled_gemm(
    tbl,
    expected_ages = expected_ages,
    data_name = "Compiled GEMM IHD table"
  )
}


validate_compiled_gemm <- function(
  tbl,
  expected_ages,
  data_name = "Compiled GEMM table"
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


gemm_adult_deaths_specs <- function() {
  causes <- gemm_adult_death_causes()
  slugs <- vapply(causes, gemm_cause_slug, character(1))

  tibble::tibble(
    cause = causes,
    crf_id = paste0("gemm_pm25_", slugs, "_adult_deaths_v1"),
    data_path = paste0(
      "crf/tabular/gemm_pm25_", slugs, "_adult_deaths_v1.csv"
    )
  )
}


gemm_cause_slug <- function(cause) {
  slug <- gsub("[^a-z0-9]+", "_", tolower(cause))
  gsub("^_|_$", "", slug)
}


gemm_adult_deaths_notes <- function(cause) {
  paste(
    "Opt-in Registry migration from inst/extdata/rr/processed/rr_gemm.csv;",
    "includes the", cause,
    "25+ aggregate and all adult split-age GEMM curves"
  )
}


gemm_ihd_deaths_registry_row <- function(crf_id, data_path, notes) {
  gemm_deaths_registry_row(
    cause = CAUSE_IHD,
    crf_id = crf_id,
    data_path = data_path,
    notes = notes
  )
}


gemm_deaths_registry_row <- function(cause, crf_id, data_path, notes) {
  tibble::tibble(
    crf_id = crf_id,
    pollutant = "PM25",
    cause = cause,
    outcome = MEASURE_DEATHS,
    double_counted = FALSE,
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
    data_path = data_path,
    reference_id = "burnett_2018_gemm",
    notes = notes
  )
}


gemm_reference_row <- function() {
  tibble::tibble(
    reference_id = "burnett_2018_gemm",
    author = "Burnett R, Chen H, Szyszkowicz M, et al.",
    year = 2018,
    title = paste(
      "Global estimates of mortality associated with long-term exposure",
      "to outdoor fine particulate matter"
    ),
    journal = "PNAS",
    volume = "115",
    pages = "9592-9597",
    doi = "10.1073/pnas.1803222115",
    url = NA_character_,
    notes = "GEMM source for PM2.5 mortality concentration-response functions"
  )
}
