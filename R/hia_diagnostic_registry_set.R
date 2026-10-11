.supported_hia_diagnostic_contract <- function() {
  "legacy_default_gemm_gbd2019_2020"
}


.validate_hia_diagnostic_specification <- function(specification) {
  if (!inherits(specification, "creahia_hia_specification")) {
    stop(
      "Diagnostic Registry resolution requires a canonical HIA specification.",
      call. = FALSE
    )
  }
  if (!identical(specification$schema_version, 1L)) {
    stop("Unsupported HIA specification schema version.", call. = FALSE)
  }
  if (!identical(
    specification$input_contract,
    .supported_hia_diagnostic_contract()
  )) {
    stop(
      "The HIA specification is outside the supported diagnostic contract.",
      call. = FALSE
    )
  }
  if (!identical(specification$epi_version, "gbd2019")) {
    stop(
      "The first diagnostic contract requires epi_version = 'gbd2019'.",
      call. = FALSE
    )
  }
  if (!.matches_direct_population_year(specification$population_year, 2020)) {
    stop(
      "The first diagnostic contract requires directly supplied pop_year = 2020.",
      call. = FALSE
    )
  }

  expected_manifest <- .load_hia_component_manifest()
  expected_crf_ids <- expected_manifest %>%
    dplyr::filter(
      component_type == "crf",
      selection_id == "legacy_profile_default"
    ) %>%
    dplyr::pull(component_id)
  expected_rr_ids <- .load_historical_rr_manifest() %>%
    tidyr::separate_rows(required_by, sep = ";") %>%
    dplyr::filter(
      required_by == .supported_hia_diagnostic_contract()
    ) %>%
    dplyr::pull(component_id)

  supplied_crf_ids <- specification$components$crf$component_id
  supplied_rr_ids <- specification$components$rr$component_id
  is_complete_component_set <- function(supplied, expected) {
    length(supplied) == length(expected) &&
      !anyNA(supplied) &&
      !anyDuplicated(supplied) &&
      setequal(supplied, expected)
  }
  if (!is_complete_component_set(supplied_crf_ids, expected_crf_ids)) {
    stop(
      "The diagnostic HIA specification does not contain the complete CRF profile.",
      call. = FALSE
    )
  }
  if (!is_complete_component_set(supplied_rr_ids, expected_rr_ids)) {
    stop(
      "The diagnostic HIA specification does not contain the complete RR selection.",
      call. = FALSE
    )
  }
  epi_components <- specification$components$epi
  if (
    nrow(epi_components) != 1 ||
    !identical(epi_components$source, "gbd2019") ||
    !isTRUE(epi_components$ready)
  ) {
    stop(
      "The diagnostic HIA specification does not contain the required EPI component.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.resolve_historical_rr_diagnostic_components <- function(
  component_ids,
  manifest = .load_historical_rr_manifest(),
  internal_registry = .load_historical_rr_registry(),
  public_registry = load_crf_registry()
) {
  .validate_historical_rr_manifest(
    manifest,
    historical_registry = internal_registry
  )
  component_ids <- unique(as.character(component_ids))
  if (length(component_ids) == 0 || any(is.na(component_ids))) {
    stop("component_ids must contain at least one non-missing ID.", call. = FALSE)
  }

  missing_components <- setdiff(component_ids, manifest$component_id)
  if (length(missing_components) > 0) {
    stop(
      "Historical RR components are absent from the manifest: ",
      paste(missing_components, collapse = ", "),
      call. = FALSE
    )
  }

  selected_manifest <- manifest %>%
    dplyr::filter(component_id %in% component_ids)
  blocked <- selected_manifest %>%
    dplyr::filter(reproduction_status != "parity_verified")
  if (nrow(blocked) > 0) {
    stop(
      "Historical RR components are not parity-verified: ",
      paste(blocked$component_id, collapse = ", "),
      call. = FALSE
    )
  }

  internal <- selected_manifest %>%
    dplyr::filter(coverage_status == "historical_internal_candidate") %>%
    dplyr::select(component_id) %>%
    dplyr::inner_join(internal_registry, by = "component_id")

  public_manifest <- selected_manifest %>%
    dplyr::filter(coverage_status == "public_registry_candidate")
  public <- purrr::map_dfr(seq_len(nrow(public_manifest)), function(index) {
    component <- public_manifest[index, , drop = FALSE]
    candidate_ids <- strsplit(
      component$public_registry_crf_ids,
      ";",
      fixed = TRUE
    )[[1]]
    candidates <- public_registry %>%
      dplyr::filter(crf_id %in% candidate_ids)
    age_matches <- vapply(seq_len(nrow(candidates)), function(candidate_index) {
      candidate_data <- load_crf_tabular(
        candidates[candidate_index, , drop = FALSE]
      )
      identical(
        .canonical_rr_age_contract(candidate_data$age),
        component$age_contract
      )
    }, logical(1))
    matching_candidate <- candidates[age_matches, , drop = FALSE]

    if (nrow(matching_candidate) != 1) {
      stop(
        "Expected one public RR candidate for ",
        component$component_id,
        ", found ",
        nrow(matching_candidate),
        ".",
        call. = FALSE
      )
    }

    matching_candidate %>%
      dplyr::mutate(component_id = component$component_id, .before = 1)
  })

  resolved <- dplyr::bind_rows(internal, public)
  if (!setequal(resolved$component_id, component_ids)) {
    unresolved <- setdiff(component_ids, resolved$component_id)
    stop(
      "Historical RR components lack Registry data: ",
      paste(unresolved, collapse = ", "),
      call. = FALSE
    )
  }
  duplicated_crf_ids <- unique(resolved$crf_id[duplicated(resolved$crf_id)])
  if (length(duplicated_crf_ids) > 0) {
    stop(
      "Historical RR components resolve to duplicate Registry rows: ",
      paste(duplicated_crf_ids, collapse = ", "),
      call. = FALSE
    )
  }

  resolved <- resolved %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS)) %>%
    dplyr::distinct(crf_id, .keep_all = TRUE)
  validate_crf_selection(resolved)
  resolved
}


.apply_hia_set_double_counting <- function(crfs) {
  dc_groups <- load_dc_groups()
  aggregate_causes <- dc_groups %>%
    dplyr::filter(role == CRF_DC_ROLE_AGGREGATE) %>%
    dplyr::pull(cause)
  component_causes <- dc_groups %>%
    dplyr::filter(role == CRF_DC_ROLE_COMPONENT) %>%
    dplyr::pull(cause)
  has_pm25_deaths_aggregate <- any(
    crfs$pollutant == "PM25" &
      crfs$outcome == MEASURE_DEATHS &
      crfs$cause %in% aggregate_causes
  )

  if (!has_pm25_deaths_aggregate) {
    return(crfs)
  }

  crfs %>%
    dplyr::mutate(
      double_counted = double_counted |
        (
          pollutant == "PM25" &
            outcome == MEASURE_DEATHS &
            cause %in% component_causes
        )
    )
}


.resolve_hia_diagnostic_registry_set <- function(specification) {
  .validate_hia_diagnostic_specification(specification)

  historical_crfs <- .resolve_historical_crf_components(
    specification$components$crf$component_id
  ) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS))
  historical_rr <- .resolve_historical_rr_diagnostic_components(
    specification$components$rr$component_id
  )
  resolved <- dplyr::bind_rows(historical_crfs, historical_rr) %>%
    .apply_hia_set_double_counting()

  validate_crf_selection(resolved)
  resolved <- new_crfs_set(resolved)
  class(resolved) <- unique(c(
    "creahia_hia_diagnostic_set",
    class(resolved)
  ))
  resolved
}
