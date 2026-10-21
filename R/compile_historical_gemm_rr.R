.historical_gemm_rr_curve_specs <- function() {
  tibble::tribble(
    ~cause, ~age, ~filename,
    CAUSE_LRICHILD, AGE_CHILDREN,
    "historical_gemm_pm25_lri_child_under5_v1.csv"
  )
}


.compile_historical_gemm_rr_curves <- function(
  source_path = NULL,
  output_dir = NULL
) {
  if (is.null(source_path)) {
    source_path <- get_hia_path(
      "rr/processed/rr_gemm.csv",
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
    "source", "cause", "age", "exposure", "low", "central", "high"
  )
  missing_columns <- setdiff(required_columns, names(source))
  if (length(missing_columns) > 0) {
    stop(
      "GEMM RR source is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  specs <- .historical_gemm_rr_curve_specs()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_paths <- file.path(output_dir, specs$filename)

  for (i in seq_len(nrow(specs))) {
    selected <- source %>%
      dplyr::filter(
        source == RR_GEMM,
        cause == specs$cause[[i]],
        age == specs$age[[i]]
      ) %>%
      dplyr::select(exposure, age, low, central, high)

    if (nrow(selected) == 0) {
      stop(
        "GEMM RR source has no curve for ",
        specs$cause[[i]],
        " / ",
        specs$age[[i]],
        ".",
        call. = FALSE
      )
    }

    conflicting_duplicates <- selected %>%
      dplyr::group_by(exposure, age) %>%
      dplyr::summarise(
        distinct_values = dplyr::n_distinct(paste(low, central, high)),
        .groups = "drop"
      ) %>%
      dplyr::filter(distinct_values != 1)
    if (nrow(conflicting_duplicates) > 0) {
      stop(
        "GEMM child-LRI duplicate exposure rows contain different RR values.",
        call. = FALSE
      )
    }

    curve <- selected %>%
      dplyr::distinct(exposure, age, .keep_all = TRUE) %>%
      dplyr::arrange(age, exposure)
    readr::write_csv(curve, output_paths[[i]])
  }

  invisible(output_paths)
}
