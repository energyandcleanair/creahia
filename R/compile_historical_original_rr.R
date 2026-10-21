.historical_original_rr_curve_specs <- function() {
  tibble::tibble(
    cause = c(
      "Diabetes", "LRI.child", "LRI", "LC", "COPD", "IHD", "Stroke"
    ),
    ages = list(
      AGE_ADULTS,
      AGE_CHILDREN,
      AGE_ADULTS,
      AGE_ADULTS,
      AGE_ADULTS,
      AGE_ADULTS_SPLIT,
      AGE_ADULTS_SPLIT
    ),
    filename = c(
      "historical_original_pm25_diabetes_25plus_v1.csv",
      "historical_original_pm25_lri_child_under5_v1.csv",
      "historical_original_pm25_lri_25plus_v1.csv",
      "historical_original_pm25_lc_25plus_v1.csv",
      "historical_original_pm25_copd_25plus_v1.csv",
      "historical_original_pm25_ihd_adult_v1.csv",
      "historical_original_pm25_stroke_adult_v1.csv"
    )
  )
}


.compile_historical_original_rr_curves <- function(
  source_path = NULL,
  output_dir = NULL
) {
  if (is.null(source_path)) {
    source_path <- get_hia_path(
      "rr/processed/rr_original.csv",
      error_if_not_exists = TRUE
    )
  }
  if (is.null(output_dir)) {
    output_dir <- file.path(
      dirname(dirname(source_path)),
      "compatibility",
      "tabular"
    )
  }

  source <- readr::read_csv(source_path, col_types = readr::cols())
  required_columns <- c(
    "cause", "age", "exposure", "low", "central", "high"
  )
  missing_columns <- setdiff(required_columns, names(source))
  if (length(missing_columns) > 0) {
    stop(
      "Original RR source is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  specs <- .historical_original_rr_curve_specs()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_paths <- file.path(output_dir, specs$filename)

  for (i in seq_len(nrow(specs))) {
    expected_ages <- specs$ages[[i]]
    curve <- source %>%
      dplyr::filter(
        cause == specs$cause[[i]],
        age %in% expected_ages
      ) %>%
      dplyr::mutate(.age_order = match(age, expected_ages)) %>%
      dplyr::arrange(.age_order, exposure) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      tibble::as_tibble()

    if (nrow(curve) == 0) {
      stop(
        "Original RR source has no curve for ",
        specs$cause[[i]],
        " / ",
        paste(expected_ages, collapse = ";"),
        ".",
        call. = FALSE
      )
    }
    if (anyDuplicated(curve[c("exposure", "age")])) {
      stop(
        "Original RR source has duplicate age/exposure rows for ",
        specs$cause[[i]],
        " / ",
        paste(expected_ages, collapse = ";"),
        ".",
        call. = FALSE
      )
    }
    validate_compiled_tabular_crf(
      curve,
      expected_ages = expected_ages,
      data_name = paste("Compiled original", specs$cause[[i]], "table")
    )
    readr::write_csv(curve, output_paths[[i]])
  }

  invisible(output_paths)
}
