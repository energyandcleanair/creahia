# Historical RR components whose rows were promoted to the public Registry.
promoted_rr_manifest <- function(source, child) {
  manifest <- .load_historical_rr_manifest() %>%
    dplyr::filter(
      .data$component_type == "rr",
      .data$source == .env$source,
      .data$coverage_status == "public_registry_candidate"
    )
  if (child) {
    manifest %>% dplyr::filter(.data$cause == CAUSE_LRICHILD)
  } else {
    manifest %>%
      dplyr::filter(
        .data$cause != CAUSE_LRICHILD,
        .data$outcome != MEASURE_DEATHS
      )
  }
}


promoted_rr_registry <- function(source, child) {
  ids <- promoted_rr_manifest(source, child)$public_registry_crf_ids
  load_crf_registry() %>%
    dplyr::filter(.data$crf_id %in% ids) %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    .new_historical_crf_set()
}
