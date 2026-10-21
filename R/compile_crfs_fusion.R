fusion_adult_death_causes <- function() {
  c(
    CAUSE_NCDLRI,
    CAUSE_LRI,
    CAUSE_COPD,
    CAUSE_DIABETES,
    CAUSE_IHD,
    CAUSE_LUNGCANCER,
    CAUSE_STROKE
  )
}


fusion_split_age_causes <- function() {
  c(CAUSE_IHD, CAUSE_STROKE)
}


fusion_expected_ages <- function(cause) {
  if (!cause %in% fusion_adult_death_causes()) {
    stop(
      "Unsupported Fusion adult death cause: ", cause,
      ". Supported causes: ",
      paste(fusion_adult_death_causes(), collapse = ", "),
      call. = FALSE
    )
  }

  if (cause %in% fusion_split_age_causes()) {
    return(c(AGE_ADULTS, AGE_ADULTS_SPLIT))
  }

  AGE_ADULTS
}


fusion_adult_deaths_specs <- function() {
  causes <- fusion_adult_death_causes()
  age_structure <- ifelse(
    causes %in% fusion_split_age_causes(),
    "adult",
    "25plus"
  )
  slugs <- vapply(causes, fusion_cause_slug, character(1))
  ids <- paste0(
    "fusion_pm25_", slugs, "_", age_structure, "_deaths_v1"
  )

  tibble::tibble(
    cause = causes,
    outcome = MEASURE_DEATHS,
    age_structure = age_structure,
    crf_id = ids,
    data_path = paste0("crf/tabular/", ids, ".csv")
  )
}


compile_fusion_adult_deaths <- function(
  overwrite = FALSE,
  update_registry = FALSE,
  update_references = FALSE
) {
  source_path <- get_hia_path(
    "rr/processed/rr_fusion.csv",
    error_if_not_exists = TRUE
  )
  rr <- readr::read_csv(source_path, col_types = readr::cols())
  specs <- fusion_adult_deaths_specs()

  compiled <- stats::setNames(
    lapply(
      specs$cause,
      function(cause) compile_fusion_deaths_data(rr, cause = cause)
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
      "Fusion output files already exist: ",
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
      fusion_deaths_registry_row(
        cause = specs$cause[[i]],
        crf_id = specs$crf_id[[i]],
        data_path = specs$data_path[[i]],
        notes = fusion_adult_deaths_notes(specs$cause[[i]])
      )
    }
  ))

  if (update_references) {
    upsert_crf_reference_row(fusion_reference_row())
  }

  if (update_registry) {
    upsert_crf_registry_rows(registry_rows)
  }

  registry_rows
}


compile_fusion_deaths_data <- function(rr, cause) {
  required_source_cols <- c(
    "cause", "source", "exposure", "age", "low", "central", "high"
  )
  missing_source_cols <- setdiff(required_source_cols, names(rr))

  if (length(missing_source_cols) > 0) {
    stop(
      "Fusion source table is missing columns: ",
      paste(missing_source_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ages <- fusion_expected_ages(cause)
  fusion_cause <- rr %>%
    dplyr::filter(
      .data$cause == .env$cause,
      source == RR_FUSION
    ) %>%
    dplyr::mutate(.age_order = match(age, ages)) %>%
    dplyr::arrange(.age_order, exposure) %>%
    dplyr::select(exposure, age, low, central, high) %>%
    tibble::as_tibble()

  validate_compiled_tabular_crf(
    fusion_cause,
    expected_ages = ages,
    data_name = paste("Compiled Fusion", cause, "table")
  )
  fusion_cause
}


fusion_cause_slug <- function(cause) {
  slug <- gsub("[^a-z0-9]+", "_", tolower(cause))
  gsub("^_|_$", "", slug)
}


fusion_adult_deaths_notes <- function(cause) {
  age_description <- if (cause %in% fusion_split_age_causes()) {
    "25+ aggregate and all adult split-age curves"
  } else {
    "25+ curve"
  }

  paste(
    "Opt-in Registry migration from inst/extdata/rr/processed/rr_fusion.csv;",
    "includes the", cause, age_description
  )
}


fusion_deaths_registry_row <- function(cause, crf_id, data_path, notes) {
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
    reference_id = "burnett_2022_fusion",
    notes = notes
  )
}


fusion_reference_row <- function() {
  tibble::tibble(
    reference_id = "burnett_2022_fusion",
    author = "Burnett RT, Spadaro JV, Garcia GR, Pope CA III",
    year = 2022,
    title = paste(
      "Designing health impact functions to assess marginal changes",
      "in outdoor fine particulate matter"
    ),
    journal = "Environmental Research",
    volume = "204",
    pages = "112245",
    doi = "10.1016/j.envres.2021.112245",
    url = "https://doi.org/10.1016/j.envres.2021.112245",
    notes = paste(
      "Primary methods paper for the Fusion PM2.5 mortality model.",
      "The current generation code returns curves from",
      "generate_rr_fusion_from_code(), which uses the Burnett-supplied",
      "\"Fusion Parameters Jul 7, 2021.csv\" and",
      "\"LL Fusion Parameters theta eq 97.5.csv\".",
      "The packaged rr/processed/rr_fusion.csv is the migration source, but",
      "its Git history does not establish regeneration by the current code.",
      "The lookup workbook is a validation reference only; workbook metadata",
      "identifies Joseph V. Spadaro as its creator, and the code notes that",
      "generated values do not yet exactly match it."
    )
  )
}
