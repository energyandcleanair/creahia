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
    translated_rr_sources <- convert_old_parameters_to_rr_sources(
      calc_causes = calc_causes,
      gbd_causes = gbd_causes
    )
    log_info("Using old parameters for calc_causes and gbd_causes. Ignoring rr_sources.")

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


.historical_crf_profile_selection_id <- function(
  crfs_version,
  memberships = .load_historical_crf_memberships()
) {
  if (
    !is.character(crfs_version) ||
    length(crfs_version) != 1 ||
    is.na(crfs_version) ||
    crfs_version == ""
  ) {
    stop("crfs_version must contain one non-empty value.", call. = FALSE)
  }

  profile_slug <- gsub("[^a-z0-9]+", "_", tolower(crfs_version))
  profile_slug <- gsub("^_|_$", "", profile_slug)
  selection_id <- paste0("legacy_profile_", profile_slug)

  if (!(selection_id %in% memberships$selection_id)) {
    return(NULL)
  }

  selection_id
}


.hia_component_evidence_label <- function(
  crf_selection_id,
  rr_selection_mode,
  epi_version
) {
  paste(
    "component_resolved",
    crf_selection_id,
    rr_selection_mode,
    epi_version,
    sep = "|"
  )
}


.new_hia_compatibility_route_plan <- function(
  status,
  route,
  reason,
  input_contract = NULL,
  blocking_component_ids = character(),
  registry_set = NULL,
  error = NULL
) {
  structure(
    list(
      status = status,
      route = route,
      reason = reason,
      input_contract = input_contract,
      blocking_component_ids = blocking_component_ids,
      registry_set = registry_set,
      error = error
    ),
    class = c("creahia_hia_compatibility_route_plan", "list")
  )
}


.plan_hia_compatibility_route <- function(
  observation,
  registry_set_resolver = .resolve_hia_diagnostic_registry_set
) {
  if (!is.list(observation) || is.null(observation$status)) {
    stop("HIA compatibility observation is missing a status.", call. = FALSE)
  }

  if (identical(observation$status, "not_applicable")) {
    return(.new_hia_compatibility_route_plan(
      status = "planned",
      route = "unchanged",
      reason = observation$reason %||% "adapter_not_applicable"
    ))
  }
  if (identical(observation$status, "error")) {
    return(.new_hia_compatibility_route_plan(
      status = "error",
      route = NULL,
      reason = "compatibility_boundary_error",
      error = observation$error %||% "Unknown compatibility boundary error."
    ))
  }
  if (!identical(observation$status, "resolved")) {
    stop(
      "HIA compatibility observation has an unsupported status: ",
      observation$status,
      call. = FALSE
    )
  }

  required_fields <- c("input_contract", "result", "specification")
  missing_fields <- setdiff(required_fields, names(observation))
  if (length(missing_fields) > 0) {
    stop(
      "Resolved HIA compatibility observation is missing fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
  result_ready <- observation$result$ready
  specification_ready <- observation$specification$readiness$ready
  if (
    !is.logical(result_ready) || length(result_ready) != 1 ||
    !is.logical(specification_ready) || length(specification_ready) != 1 ||
    is.na(result_ready) || is.na(specification_ready) ||
    !identical(result_ready, specification_ready)
  ) {
    stop(
      "HIA readiness result and canonical specification disagree.",
      call. = FALSE
    )
  }
  if (!identical(
    observation$input_contract,
    observation$specification$input_contract
  )) {
    stop(
      "HIA observation and canonical specification use different contracts.",
      call. = FALSE
    )
  }
  result_blockers <- observation$result$blocking_components$component_id
  specification_blockers <-
    observation$specification$readiness$blocking_component_ids
  if (!setequal(result_blockers, specification_blockers)) {
    stop(
      "HIA readiness result and canonical specification list different blockers.",
      call. = FALSE
    )
  }
  if ((result_ready && length(result_blockers) > 0) ||
      (!result_ready && length(result_blockers) == 0)) {
    stop(
      "HIA readiness status is inconsistent with its blocking components.",
      call. = FALSE
    )
  }

  if (!result_ready) {
    return(.new_hia_compatibility_route_plan(
      status = "planned",
      route = "legacy",
      reason = "components_not_ready",
      input_contract = observation$input_contract,
      blocking_component_ids = specification_blockers
    ))
  }

  registry_set <- tryCatch(
    registry_set_resolver(observation$specification),
    error = function(error) error
  )
  if (inherits(registry_set, "error")) {
    return(.new_hia_compatibility_route_plan(
      status = "error",
      route = NULL,
      reason = "registry_set_resolution_error",
      input_contract = observation$input_contract,
      error = conditionMessage(registry_set)
    ))
  }
  if (!is_crfs_set(registry_set)) {
    stop(
      "HIA Registry Set resolver must return a crfs_set() result.",
      call. = FALSE
    )
  }

  .new_hia_compatibility_route_plan(
    status = "planned",
    route = "registry",
    reason = "all_components_ready",
    input_contract = observation$input_contract,
    registry_set = registry_set
  )
}


.empty_hia_rr_sources <- function() {
  tibble::tibble(
    cause = character(),
    source = character()
  )
}


.apply_hia_compatibility_route_plan <- function(
  route_plan,
  crfs,
  rr_sources,
  crf_compute
) {
  if (!inherits(route_plan, "creahia_hia_compatibility_route_plan")) {
    stop("route_plan must be a compatibility route plan.", call. = FALSE)
  }
  if (identical(route_plan$status, "error")) {
    stop(
      "Cannot apply an errored HIA compatibility route plan: ",
      route_plan$error %||% route_plan$reason,
      call. = FALSE
    )
  }
  if (!identical(route_plan$status, "planned")) {
    stop(
      "Cannot apply an HIA compatibility route plan with status: ",
      route_plan$status,
      call. = FALSE
    )
  }

  if (identical(route_plan$route, "unchanged")) {
    return(list(
      crfs = crfs,
      rr_sources = rr_sources,
      crf_compute = crf_compute
    ))
  }
  if (identical(route_plan$route, "legacy")) {
    if (!identical(crf_compute, "legacy") || is_crfs_set(crfs)) {
      stop(
        "A legacy compatibility plan requires existing legacy CRF inputs.",
        call. = FALSE
      )
    }
    return(list(
      crfs = crfs,
      rr_sources = rr_sources,
      crf_compute = crf_compute
    ))
  }
  if (!identical(route_plan$route, "registry")) {
    stop(
      "HIA compatibility route plan has an unsupported route: ",
      route_plan$route,
      call. = FALSE
    )
  }
  if (!is_crfs_set(route_plan$registry_set)) {
    stop(
      "A Registry compatibility plan requires a resolved crfs_set().",
      call. = FALSE
    )
  }

  list(
    crfs = route_plan$registry_set,
    rr_sources = .empty_hia_rr_sources(),
    crf_compute = "registry"
  )
}


.observe_hia_compatibility_route_application <- function(
  route_plan,
  crfs,
  rr_sources,
  crf_compute
) {
  tryCatch(
    structure(
      list(
        status = "applied",
        compute_inputs = .apply_hia_compatibility_route_plan(
          route_plan = route_plan,
          crfs = crfs,
          rr_sources = rr_sources,
          crf_compute = crf_compute
        ),
        error = NULL
      ),
      class = c("creahia_hia_route_application_observation", "list")
    ),
    error = function(error) {
      structure(
        list(
          status = "error",
          compute_inputs = NULL,
          error = conditionMessage(error)
        ),
        class = c("creahia_hia_route_application_observation", "list")
      )
    }
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

  if (context$crfs_supplied) {
    return(.not_applicable_hia_boundary(
      reason = "explicit_crfs",
      message = paste(
        "Explicit CRF data require a separate canonical-data rule before",
        "shared-engine routing."
      )
    ))
  }

  crf_selection_id <- .historical_crf_profile_selection_id(
    context$crfs_version
  )
  if (is.null(crf_selection_id)) {
    return(.not_applicable_hia_boundary(
      reason = "unrepresented_crf_profile",
      message = "The selected CRF profile is absent from compatibility storage."
    ))
  }

  readiness <- .resolve_hia_component_readiness(
    crfs = crfs,
    rr_sources = rr_sources,
    epi = epi,
    crf_input_mode = "legacy_profile",
    crf_selection_id = crf_selection_id,
    epi_version = context$epi_version
  )
  input_contract <- .hia_component_evidence_label(
    crf_selection_id = crf_selection_id,
    rr_selection_mode = context$rr_selection_mode,
    epi_version = context$epi_version
  )
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

  route_plan <- tryCatch(
    .plan_hia_compatibility_route(observation),
    error = function(error) {
      .new_hia_compatibility_route_plan(
        status = "error",
        route = NULL,
        reason = "route_planning_error",
        input_contract = observation$input_contract %||% NULL,
        error = conditionMessage(error)
      )
    }
  )
  observation$route_plan <- route_plan
  observation$route_application <-
    .observe_hia_compatibility_route_application(
      route_plan = route_plan,
      crfs = crfs,
      rr_sources = rr_sources,
      crf_compute = context$crf_compute
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


.hia_readiness_shadow_enabled <- function() {
  is.function(getOption("creahia.hia_readiness_shadow_observer"))
}


.hia_dual_run_enabled <- function() {
  is.function(getOption("creahia.hia_dual_run_observer"))
}


.hia_compatibility_routing_enabled <- function() {
  configured <- getOption("creahia.hia_compatibility_routing", NULL)
  if (!is.null(configured)) {
    return(isTRUE(configured))
  }

  !.hia_compatibility_diagnostics_enabled()
}


.hia_compatibility_diagnostics_enabled <- function() {
  .hia_readiness_shadow_enabled() || .hia_dual_run_enabled()
}


.hia_compatibility_resolution_enabled <- function() {
  .hia_compatibility_routing_enabled() ||
    .hia_compatibility_diagnostics_enabled()
}


.activate_hia_compatibility_route <- function(observation) {
  if (!is.list(observation) || is.null(observation$status)) {
    stop(
      "HIA compatibility routing requires a boundary observation.",
      call. = FALSE
    )
  }
  if (identical(observation$status, "error")) {
    stop(
      "HIA compatibility routing failed at the boundary: ",
      observation$error %||% "Unknown compatibility boundary error.",
      call. = FALSE
    )
  }

  application <- observation$route_application %||% NULL
  if (
    !inherits(application, "creahia_hia_route_application_observation") ||
    !identical(application$status, "applied") ||
    is.null(application$compute_inputs)
  ) {
    stop(
      "HIA compatibility routing could not apply the route plan: ",
      application$error %||% "No applied compute inputs are available.",
      call. = FALSE
    )
  }

  application$compute_inputs
}


.new_hia_dual_run_result <- function(
  status,
  reason,
  input_contract = NULL,
  tolerance = 1e-12,
  legacy_rows = NULL,
  registry_rows = NULL,
  differences = character(),
  error = NULL
) {
  structure(
    list(
      status = status,
      reason = reason,
      input_contract = input_contract,
      tolerance = tolerance,
      legacy_rows = legacy_rows,
      registry_rows = registry_rows,
      differences = differences,
      error = error
    ),
    class = c("creahia_hia_dual_run_result", "list")
  )
}


.compare_hia_dual_run_results <- function(
  legacy_impacts,
  registry_impacts,
  tolerance = 1e-12
) {
  if (
    identical(names(registry_impacts), names(legacy_impacts)) &&
    identical(class(registry_impacts), class(legacy_impacts))
  ) {
    row_identity_columns <- intersect(
      c(
        "scenario",
        "region_id",
        "pollutant",
        "cause",
        "outcome",
        "estimate",
        "age_group",
        "double_counted",
        "region_name",
        "iso3"
      ),
      names(legacy_impacts)
    )
    legacy_impacts <- legacy_impacts %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(row_identity_columns)))
    registry_impacts <- registry_impacts %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(row_identity_columns)))
  }
  differences <- all.equal(
    registry_impacts,
    legacy_impacts,
    tolerance = tolerance,
    check.attributes = TRUE
  )
  matched <- isTRUE(differences)

  .new_hia_dual_run_result(
    status = if (matched) "matched" else "different",
    reason = if (matched) "outputs_match" else "outputs_differ",
    tolerance = tolerance,
    legacy_rows = nrow(legacy_impacts),
    registry_rows = nrow(registry_impacts),
    differences = if (matched) character() else as.character(differences)
  )
}


.notify_hia_dual_run_observer <- function(result) {
  observer <- getOption("creahia.hia_dual_run_observer")
  if (is.function(observer)) {
    tryCatch(
      observer(result),
      error = function(error) invisible(NULL)
    )
  }
  invisible(result)
}


.execute_hia_compatibility_dual_run <- function(
  observation,
  legacy_impacts,
  conc_map,
  species,
  regions,
  scenarios,
  epi_version,
  epi,
  population_scale = NULL,
  tolerance = 1e-12
) {
  input_contract <- observation$input_contract %||% NULL
  route_application <- observation$route_application %||% NULL
  route_plan <- observation$route_plan %||% NULL

  if (
    !identical(observation$status, "resolved") ||
    !identical(route_plan$status, "planned") ||
    !identical(route_plan$route, "registry") ||
    !identical(route_application$status, "applied")
  ) {
    return(.notify_hia_dual_run_observer(.new_hia_dual_run_result(
      status = "not_run",
      reason = route_plan$reason %||% observation$reason %||%
        "registry_candidate_unavailable",
      input_contract = input_contract,
      tolerance = tolerance,
      legacy_rows = nrow(legacy_impacts)
    )))
  }

  candidate <- tryCatch(
    {
      compute_inputs <- route_application$compute_inputs
      paf <- compute_hia_paf(
        conc_map = conc_map,
        species = species,
        regions = regions,
        scenarios = scenarios,
        epi_version = epi_version,
        rr_sources = compute_inputs$rr_sources,
        crfs = compute_inputs$crfs,
        diagnostic_folder = NULL,
        crf_compute = compute_inputs$crf_compute
      )
      impacts <- compute_hia_impacts(
        species = species,
        paf = paf,
        conc_map = conc_map,
        regions = regions,
        epi = epi,
        crfs = compute_inputs$crfs
      )
      if (!is.null(population_scale)) {
        impacts <- scale_hia_pop(
          impacts,
          base_year = population_scale$base_year,
          target_year = population_scale$target_year
        )
      }
      impacts
    },
    error = function(error) error
  )

  if (inherits(candidate, "error")) {
    return(.notify_hia_dual_run_observer(.new_hia_dual_run_result(
      status = "error",
      reason = "registry_candidate_error",
      input_contract = input_contract,
      tolerance = tolerance,
      legacy_rows = nrow(legacy_impacts),
      error = conditionMessage(candidate)
    )))
  }

  result <- .compare_hia_dual_run_results(
    legacy_impacts = legacy_impacts,
    registry_impacts = candidate,
    tolerance = tolerance
  )
  result$input_contract <- input_contract
  .notify_hia_dual_run_observer(result)
}
