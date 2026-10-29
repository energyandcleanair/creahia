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
  if (
    !is.character(specification$input_contract) ||
    length(specification$input_contract) != 1 ||
    is.na(specification$input_contract) ||
    specification$input_contract == ""
  ) {
    stop(
      "The HIA specification requires one non-empty evidence label.",
      call. = FALSE
    )
  }
  if (!isTRUE(specification$readiness$ready)) {
    stop(
      "Diagnostic Registry resolution requires every component to be ready.",
      call. = FALSE
    )
  }

  supplied_crf_ids <- specification$components$crf$component_id
  supplied_rr_ids <- specification$components$rr$component_id
  if (
    length(supplied_crf_ids) == 0 ||
    anyNA(supplied_crf_ids) ||
    anyDuplicated(supplied_crf_ids)
  ) {
    stop(
      "The diagnostic HIA specification has invalid CRF component IDs.",
      call. = FALSE
    )
  }
  if (anyNA(supplied_rr_ids) || anyDuplicated(supplied_rr_ids)) {
    stop(
      "The diagnostic HIA specification has invalid RR component IDs.",
      call. = FALSE
    )
  }
  route_components <- dplyr::bind_rows(
    specification$components$crf,
    specification$components$rr
  )
  if (
    anyNA(route_components$ready) ||
    any(!route_components$ready) ||
    anyNA(route_components$routing_status) ||
    any(route_components$routing_status != "ready")
  ) {
    stop(
      "Diagnostic Registry components must be routing-ready.",
      call. = FALSE
    )
  }
  epi_components <- specification$components$epi
  if (
    nrow(epi_components) != 1 ||
    !isTRUE(epi_components$ready)
  ) {
    stop(
      "The diagnostic HIA specification requires one ready EPI component.",
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


.mark_hia_compatibility_rr_crfs <- function(crfs, rr_crf_ids) {
  if (!inherits(crfs, "creahia_hia_diagnostic_set")) {
    stop(
      "Compatibility RR provenance can only be attached to an internal HIA diagnostic Set.",
      call. = FALSE
    )
  }

  rr_crf_ids <- unique(as.character(rr_crf_ids))
  if (anyNA(rr_crf_ids) || !all(rr_crf_ids %in% crfs$crf_id)) {
    stop("Compatibility RR provenance contains invalid CRF IDs.", call. = FALSE)
  }

  attr(crfs, "creahia.hia_compatibility_rr_crf_ids") <- rr_crf_ids
  crfs
}


.hia_compatibility_rr_crf_ids <- function(crfs) {
  if (!inherits(crfs, "creahia_hia_diagnostic_set")) {
    return(character())
  }

  rr_crf_ids <- attr(crfs, "creahia.hia_compatibility_rr_crf_ids", exact = TRUE)
  if (is.null(rr_crf_ids)) {
    stop(
      paste(
        "Compatibility RR provenance is missing.",
        paste(
          "Call inspect_hia_configuration() again to obtain a fresh registry_set,",
          "or use crfs_set() to build a method Set before calculating PAFs."
        )
      ),
      call. = FALSE
    )
  }
  if (
    !is.character(rr_crf_ids) ||
    anyNA(rr_crf_ids) ||
    anyDuplicated(rr_crf_ids)
  ) {
    stop(
      paste(
        "Compatibility RR provenance is malformed.",
        paste(
          "Call inspect_hia_configuration() again to obtain a fresh registry_set,",
          "or use crfs_set() to build a method Set before calculating PAFs."
        )
      ),
      call. = FALSE
    )
  }

  intersect(rr_crf_ids, crfs$crf_id)
}


.resolve_hia_diagnostic_registry_set <- function(specification) {
  .validate_hia_diagnostic_specification(specification)

  historical_crfs <- .resolve_historical_crf_components(
    specification$components$crf$component_id
  ) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::all_of(CRF_REGISTRY_REQUIRED_COLUMNS))
  historical_rr <- if (nrow(specification$components$rr) == 0) {
    tibble::tibble()
  } else {
    .resolve_historical_rr_diagnostic_components(
      specification$components$rr$component_id
    )
  }
  rr_crf_ids <- if (nrow(historical_rr) == 0) {
    character()
  } else {
    historical_rr$crf_id
  }
  resolved <- dplyr::bind_rows(historical_crfs, historical_rr)

  validate_crf_selection(resolved)
  resolved <- new_crfs_set(resolved)
  class(resolved) <- unique(c(
    "creahia_hia_diagnostic_set",
    class(resolved)
  ))
  .mark_hia_compatibility_rr_crfs(resolved, rr_crf_ids)
}
