make_specification_readiness <- function() {
  components <- tibble::tribble(
    ~component_id, ~component_type, ~selection_id, ~source, ~cause, ~outcome, ~age_contract, ~ready, ~blocking_reason, ~reproduction_status, ~routing_status, ~scientific_review_status,
    "crf_profile|legacy_profile_default|NO2|CauseA|Deaths", "crf", "legacy_profile_default", "legacy_default", "CauseA", "Deaths", NA_character_, FALSE, "CRF parity pending.", "not_verified", "not_ready", "not_reviewed",
    "rr|gemm|CauseB|Deaths", "rr", "rr_sources:data_frame", "gemm", "CauseB", "Deaths", "25-29;30-34", FALSE, "RR parity pending.", "not_verified", "not_ready", "not_reviewed",
    "epi|default|CauseA_Deaths,CauseB_Deaths", "epi", "default", "default", NA_character_, "CauseA_Deaths,CauseB_Deaths", NA_character_, TRUE, NA_character_, NA_character_, NA_character_, NA_character_
  )

  list(
    ready = FALSE,
    components = components,
    blocking_components = components %>% dplyr::filter(!ready),
    ignored_rr_pairs = tibble::tibble(
      cause = character(),
      source = character(),
      reason = character()
    )
  )
}
