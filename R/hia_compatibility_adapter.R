.hia_boundary_context_fields <- function() {
  c(
    "rr_sources_supplied",
    "epi_supplied",
    "crfs_supplied",
    "epi_version",
    "crfs_version",
    "crf_compute",
    "rr_selection_mode",
    "calc_causes",
    "gbd_causes"
  )
}


.resolve_hia_rr_selection_boundary <- function(
  rr_sources,
  calc_causes,
  gbd_causes
) {
  if (!is.null(calc_causes) || !is.null(gbd_causes)) {
    log_info("Using old parameters for calc_causes and gbd_causes. Ignoring rr_sources.")
    translated_rr_sources <- convert_old_parameters_to_rr_sources(
      calc_causes = calc_causes,
      gbd_causes = gbd_causes
    )

    return(list(
      rr_sources = translated_rr_sources,
      mode = "legacy_cause_selectors",
      calc_causes = calc_causes,
      gbd_causes = gbd_causes %||% "default"
    ))
  }

  list(
    rr_sources = rr_sources,
    mode = "rr_sources",
    calc_causes = NULL,
    gbd_causes = NULL
  )
}


.not_applicable_hia_boundary <- function(reason, message) {
  list(
    status = "not_applicable",
    reason = reason,
    message = message
  )
}


.resolve_hia_compatibility_boundary <- function(
  crfs,
  rr_sources,
  epi,
  context,
  population_year_intent
) {
  missing_context_fields <- setdiff(
    .hia_boundary_context_fields(),
    names(context)
  )
  if (length(missing_context_fields) > 0) {
    stop(
      "HIA compatibility context is missing required fields: ",
      paste(missing_context_fields, collapse = ", "),
      call. = FALSE
    )
  }

  provenance_fields <- c(
    "rr_sources_supplied",
    "epi_supplied",
    "crfs_supplied"
  )
  invalid_provenance <- provenance_fields[vapply(
    context[provenance_fields],
    function(value) {
      !is.logical(value) || length(value) != 1 || is.na(value)
    },
    logical(1)
  )]
  if (length(invalid_provenance) > 0) {
    stop(
      "HIA compatibility provenance fields must be single TRUE/FALSE values: ",
      paste(invalid_provenance, collapse = ", "),
      call. = FALSE
    )
  }

  if (!identical(context$crf_compute, "legacy")) {
    return(.not_applicable_hia_boundary(
      reason = "registry_route",
      message = "Explicit Registry routing is outside the legacy-default adapter slice."
    ))
  }

  supported_selection_modes <- c("rr_sources", "legacy_cause_selectors")
  if (!(context$rr_selection_mode %in% supported_selection_modes)) {
    stop(
      "HIA compatibility context has an invalid rr_selection_mode: ",
      context$rr_selection_mode,
      call. = FALSE
    )
  }

  if (
    identical(context$rr_selection_mode, "legacy_cause_selectors") &&
    !(
      identical(context$calc_causes, "GEMM and GBD") &&
      identical(context$gbd_causes, "default")
    )
  ) {
    return(.not_applicable_hia_boundary(
      reason = "legacy_cause_selectors",
      message = "This translated calc_causes/gbd_causes combination is not yet supported by the adapter."
    ))
  }

  if (
    !identical(context$epi_version, "default") ||
    !identical(context$crfs_version, "default")
  ) {
    return(.not_applicable_hia_boundary(
      reason = "non_default_version",
      message = "Non-default CRF or EPI versions are outside the first adapter slice."
    ))
  }

  scientific_input_fields <- c("epi_supplied", "crfs_supplied")
  if (
    any(unlist(context[scientific_input_fields], use.names = FALSE)) ||
    (
      identical(context$rr_selection_mode, "rr_sources") &&
      context$rr_sources_supplied
    )
  ) {
    return(.not_applicable_hia_boundary(
      reason = "explicit_scientific_input",
      message = "Explicit CRF, RR, or EPI inputs are outside the first adapter slice."
    ))
  }

  readiness <- .resolve_hia_component_readiness(
    crfs = crfs,
    rr_sources = rr_sources,
    epi = epi,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = context$epi_version
  )
  input_contract <- if (
    identical(context$rr_selection_mode, "legacy_cause_selectors")
  ) {
    "legacy_gemm_gbd_default"
  } else {
    "legacy_default"
  }
  specification <- .new_hia_specification(
    readiness = readiness,
    population_year_intent = population_year_intent,
    input_contract = input_contract,
    epi_version = context$epi_version
  )

  list(
    status = "resolved",
    input_contract = input_contract,
    result = readiness,
    specification = specification
  )
}


.observe_hia_compatibility_boundary <- function(
  crfs,
  rr_sources,
  epi,
  context,
  population_year_intent
) {
  observation <- tryCatch(
    .resolve_hia_compatibility_boundary(
      crfs = crfs,
      rr_sources = rr_sources,
      epi = epi,
      context = context,
      population_year_intent = population_year_intent
    ),
    error = function(error) {
      list(status = "error", error = conditionMessage(error))
    }
  )

  if (identical(observation$status, "not_applicable")) {
    return(invisible(observation))
  }

  observer <- getOption("creahia.hia_readiness_shadow_observer")
  if (is.function(observer)) {
    tryCatch(
      observer(observation),
      error = function(error) invisible(NULL)
    )
  }

  invisible(observation)
}
