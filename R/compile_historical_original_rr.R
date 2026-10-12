.historical_original_rr_curve_specs <- function() {
  tibble::tribble(
    ~cause, ~age, ~filename,
    "Diabetes", AGE_ADULTS,
    "historical_original_pm25_diabetes_25plus_v1.csv",
    "LRI.child", AGE_CHILDREN,
    "historical_original_pm25_lri_child_under5_v1.csv",
    "LRI", AGE_ADULTS,
    "historical_original_pm25_lri_25plus_v1.csv",
    "LC", AGE_ADULTS,
    "historical_original_pm25_lc_25plus_v1.csv",
    "COPD", AGE_ADULTS,
    "historical_original_pm25_copd_25plus_v1.csv"
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
    curve <- source %>%
      dplyr::filter(
        cause == specs$cause[[i]],
        age == specs$age[[i]]
      ) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      dplyr::arrange(age, exposure)

    if (nrow(curve) == 0) {
      stop(
        "Original RR source has no curve for ",
        specs$cause[[i]],
        " / ",
        specs$age[[i]],
        ".",
        call. = FALSE
      )
    }
    if (anyDuplicated(curve[c("exposure", "age")])) {
      stop(
        "Original RR source has duplicate age/exposure rows for ",
        specs$cause[[i]],
        " / ",
        specs$age[[i]],
        ".",
        call. = FALSE
      )
    }
    readr::write_csv(curve, output_paths[[i]])
  }

  invisible(output_paths)
}
