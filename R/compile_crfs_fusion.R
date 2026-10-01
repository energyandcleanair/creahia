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


compile_fusion_adult_deaths <- function(overwrite = FALSE) {
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

  specs
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
