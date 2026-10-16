# Column names in epi wide table that are not metric keys
CRF_EPI_METADATA_COLUMNS <- c(
  "location_id",
  "location_level",
  "iso3",
  "estimate",
  "location_name",
  "country",
  "region",
  "income_group",
  "pop"
)

CRF_EPI_REQUIRED_ESTIMATES <- c("low", "central", "high")


get_epi_metric_keys <- function(epi) {
  setdiff(names(epi), CRF_EPI_METADATA_COLUMNS)
}


validate_epi_for_crf_check <- function(epi) {
  required_cols <- c("estimate")

  missing_cols <- setdiff(required_cols, names(epi))

  if (length(missing_cols) > 0) {
    stop(
      "Epi data is missing required columns for CRF compatibility checks: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


find_missing_epi_metric_estimates <- function(
  epi,
  metric_keys,
  required_estimates = CRF_EPI_REQUIRED_ESTIMATES
) {
  missing <- lapply(metric_keys, function(metric_key) {
    estimates_with_values <- epi %>%
      dplyr::filter(
        estimate %in% required_estimates,
        !is.na(.data[[metric_key]])
      ) %>%
      dplyr::distinct(estimate) %>%
      dplyr::pull(estimate)

    missing_estimates <- setdiff(required_estimates, estimates_with_values)

    if (length(missing_estimates) == 0) {
      return(NULL)
    }

    tibble::tibble(
      metric_key = metric_key,
      missing_estimate = missing_estimates
    )
  })

  dplyr::bind_rows(missing)
}
