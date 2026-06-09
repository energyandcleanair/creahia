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


validate_crfs_against_epi <- function(
  crfs,
  epi_version = "default",
  epi = NULL,
  required_estimates = CRF_EPI_REQUIRED_ESTIMATES
) {
  validate_crf_selection(crfs)

  if (is.null(epi)) {
    epi <- get_epi(version = epi_version)
  }

  validate_epi_for_crf_check(epi)

  required_metric_keys <- crfs %>%
    dplyr::distinct(cause, outcome) %>%
    dplyr::mutate(metric_key = build_metric_key(cause, outcome)) %>%
    dplyr::pull(metric_key) %>%
    unique()

  available_metric_keys <- get_epi_metric_keys(epi)

  missing_metric_keys <- setdiff(required_metric_keys, available_metric_keys)

  if (length(missing_metric_keys) > 0) {
    stop(
      "Selected CRFs require epidemiological metric columns missing from epi data: ",
      paste(missing_metric_keys, collapse = ", "),
      call. = FALSE
    )
  }

  missing_estimates <- setdiff(required_estimates, unique(epi$estimate))

  if (length(missing_estimates) > 0) {
    stop(
      "Epi data is missing required estimate rows: ",
      paste(missing_estimates, collapse = ", "),
      call. = FALSE
    )
  }

  missing_metric_estimates <- find_missing_epi_metric_estimates(
    epi = epi,
    metric_keys = required_metric_keys,
    required_estimates = required_estimates
  )

  if (nrow(missing_metric_estimates) > 0) {
    msg <- missing_metric_estimates %>%
      dplyr::mutate(item = paste0(metric_key, " missing ", missing_estimate)) %>%
      dplyr::pull(item) %>%
      paste(collapse = ", ")

    stop(
      "Selected CRFs require epi metrics with missing estimate values: ",
      msg,
      call. = FALSE
    )
  }

  invisible(TRUE)
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