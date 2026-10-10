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


derive_historical_rr_components <- function(
  configurations = load_historical_fingerprint_configurations(),
  include_actual_default = TRUE
) {
  configured <- purrr::map_dfr(seq_len(nrow(configurations)), function(i) {
    configuration <- configurations[i, , drop = FALSE]

    .normalise_rr_components(
      historical_configuration_rr_sources(configuration)
    ) %>%
      dplyr::mutate(mapping_id = configuration$mapping_id[[1]])
  })

  if (!include_actual_default) {
    return(configured)
  }

  dplyr::bind_rows(
    configured,
    .normalise_rr_components(c(RR_GEMM, RR_ORIGINAL)) %>%
      dplyr::mutate(mapping_id = "actual_no_argument_default")
  )
}


derive_historical_rr_requirements <- function(
  configurations = load_historical_fingerprint_configurations()
) {
  derive_historical_rr_components(configurations) %>%
    dplyr::group_by(source, cause, outcome) %>%
    dplyr::summarise(
      component_id = dplyr::first(component_id),
      age_contract = dplyr::first(age_contract),
      required_by = paste(sort(unique(mapping_id)), collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::relocate(component_id) %>%
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
