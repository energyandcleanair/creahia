.historical_gbd2023_rr_curve_specs <- function() {
  tibble::tibble(
    cause = c(CAUSE_LUNGCANCER, CAUSE_LRI, CAUSE_LRICHILD, CAUSE_IHD),
    filename = c(
      "historical_gbd2023_pm25_lc_25plus_v1.csv",
      "historical_gbd2023_pm25_lri_25plus_v1.csv",
      "historical_gbd2023_pm25_lri_child_under5_v1.csv",
      "historical_gbd2023_pm25_ihd_25plus_v1.csv"
    )
  )
}


.compile_historical_gbd2023_rr_curves <- function(
  source_path = NULL,
  output_dir = NULL
) {
  if (is.null(source_path)) {
    source_path <- get_hia_path(
      "rr/processed/rr_gbd2023.csv",
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
  specs <- .historical_gbd2023_rr_curve_specs()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_paths <- file.path(output_dir, specs$filename)

  for (i in seq_len(nrow(specs))) {
    curve <- compile_gbd_deaths_data(
      source,
      version = RR_GBD2023,
      cause = specs$cause[[i]]
    )
    readr::write_csv(curve, output_paths[[i]])
  }

  invisible(output_paths)
}
