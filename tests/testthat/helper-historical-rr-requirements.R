historical_rr_requirements_path <- function() {
  testthat::test_path("fixtures", "historical_rr_requirements.csv")
}


load_historical_rr_requirements <- function() {
  readr::read_csv(
    historical_rr_requirements_path(),
    col_types = readr::cols(
      requirement_id = readr::col_character(),
      source = readr::col_character(),
      cause = readr::col_character(),
      outcome = readr::col_character(),
      age_contracts = readr::col_character(),
      required_by = readr::col_character(),
      public_registry_crf_ids = readr::col_character(),
      coverage_status = readr::col_character(),
      reproduction_status = readr::col_character(),
      reproduction_blocking_reason = readr::col_character(),
      scientific_review_status = readr::col_character(),
      scientific_review_reason = readr::col_character()
    )
  )
}


historical_configuration_rr_sources <- function(configuration) {
  calc_causes <- configuration$legacy_calc_causes[[1]]

  if (!is.na(calc_causes)) {
    return(convert_old_parameters_to_rr_sources(
      calc_causes = calc_causes,
      gbd_causes = configuration$legacy_gbd_causes[[1]]
    ))
  }

  configuration$legacy_rr_sources[[1]]
}


derive_historical_rr_age_components <- function(
  configurations = load_historical_fingerprint_configurations()
) {
  purrr::map_dfr(seq_len(nrow(configurations)), function(i) {
    configuration <- configurations[i, , drop = FALSE]

    .normalise_rr_components(
      historical_configuration_rr_sources(configuration)
    ) %>%
      dplyr::mutate(mapping_id = configuration$mapping_id[[1]])
  })
}


derive_historical_rr_requirements <- function(
  configurations = load_historical_fingerprint_configurations()
) {
  derive_historical_rr_age_components(configurations) %>%
    dplyr::group_by(source, cause, outcome) %>%
    dplyr::summarise(
      age_contracts = paste(sort(unique(age_contract)), collapse = ";"),
      required_by = paste(sort(unique(mapping_id)), collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      requirement_id = paste(
        "rr_requirement",
        source,
        cause,
        outcome,
        sep = "|"
      ),
      .before = 1
    ) %>%
    dplyr::arrange(source, cause, outcome)
}


derive_public_rr_registry_candidates <- function(
  registry = load_crf_registry()
) {
  registry %>%
    dplyr::filter(
      form == CRF_FORM_TABULAR,
      !reference_id %in% CRF_INTERNAL_REFERENCE_IDS
    ) %>%
    dplyr::mutate(
      source = dplyr::case_when(
        grepl("^fusion_", crf_id) ~ RR_FUSION,
        grepl("^gemm_", crf_id) ~ RR_GEMM,
        grepl("^gbd2019_", crf_id) ~ RR_GBD2019,
        grepl("^gbd2021_", crf_id) ~ RR_GBD2021,
        grepl("^gbd2023_", crf_id) ~ RR_GBD2023,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(source)) %>%
    dplyr::group_by(source, cause, outcome) %>%
    dplyr::summarise(
      public_registry_crf_ids = paste(sort(crf_id), collapse = ";"),
      .groups = "drop"
    )
}
