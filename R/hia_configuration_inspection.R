#' Inspect how an HIA configuration resolves
#'
#' Resolve legacy HIA selectors through the same compatibility boundary used by
#' [compute_hia()] without calculating PAFs or health impacts.
#'
#' @param rr_sources Relative-risk source selection accepted by [compute_hia()].
#' @param epi_version EPI version label.
#' @param epi EPI data. Defaults to `get_epi(version = epi_version)`.
#' @param crfs_version Legacy CRF profile label.
#' @param crfs Legacy CRF data. Defaults to `get_crfs(version = crfs_version)`.
#' @param crf_compute CRF compute mode supplied at the public boundary.
#' @param pop_year Requested population year.
#' @param scale_base_year Deprecated population base-year argument retained for
#'   compatibility inspection.
#' @param scale_target_year Deprecated population target-year argument retained
#'   for compatibility inspection.
#' @param calc_causes Legacy cause selector.
#' @param gbd_causes Legacy GBD cause selector.
#'
#' @return A `creahia_hia_configuration_inspection` list containing the planned
#'   route, canonical specification, resolved components, Registry Set when
#'   ready, references, blockers, and population-year intent.
#' @export
inspect_hia_configuration <- function(
  rr_sources = c(RR_GEMM, RR_ORIGINAL),
  epi_version = "default",
  epi = get_epi(version = epi_version),
  crfs_version = "default",
  crfs = get_crfs(version = crfs_version),
  crf_compute = c("legacy", "registry"),
  pop_year = NULL,
  scale_base_year = NULL,
  scale_target_year = NULL,
  calc_causes = NULL,
  gbd_causes = NULL
) {
  argument_provenance <- list(
    rr_sources_supplied = !missing(rr_sources),
    epi_supplied = !missing(epi),
    crfs_supplied = !missing(crfs)
  )

  crf_compute <- match.arg(crf_compute)
  if (is_crfs_set(crfs)) {
    crf_compute <- "registry"
  }

  resolved_pop_year <- pop_year
  if (!is.null(scale_base_year) || !is.null(scale_target_year)) {
    resolved_pop_year <- if (is.null(pop_year)) scale_target_year else pop_year
  }

  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = rr_sources,
    calc_causes = calc_causes,
    gbd_causes = gbd_causes
  )
  parsed_rr_sources <- parse_rr_sources(rr_selection$rr_sources)

  context <- c(
    argument_provenance,
    list(
      epi_version = epi_version,
      crfs_version = crfs_version,
      crf_compute = crf_compute,
      rr_selection_mode = rr_selection$mode,
      calc_causes = rr_selection$calc_causes,
      gbd_causes = rr_selection$gbd_causes
    )
  )
  population_year_intent <- .capture_population_year_intent(
    pop_year = pop_year,
    scale_base_year = scale_base_year,
    scale_target_year = scale_target_year,
    resolved_pop_year = resolved_pop_year
  )

  observation <- .resolve_hia_compatibility_boundary(
    crfs = crfs,
    rr_sources = parsed_rr_sources,
    epi = epi,
    context = context,
    population_year_intent = population_year_intent
  )
  route_plan <- .plan_hia_compatibility_route(observation)
  if (identical(route_plan$status, "error")) {
    stop(
      "HIA configuration inspection failed: ",
      route_plan$error %||% route_plan$reason,
      call. = FALSE
    )
  }

  specification <- observation$specification %||% NULL
  components <- observation$result$components %||%
    .inspection_components(specification)
  registry_set <- route_plan$registry_set %||% NULL
  references <- .inspection_references(registry_set)
  blocking_components <- observation$result$blocking_components %||%
    .empty_hia_components()

  structure(
    list(
      status = observation$status,
      route = route_plan$route,
      reason = route_plan$reason,
      input_contract = observation$input_contract %||% NULL,
      canonical_specification = specification,
      components = components,
      registry_set = registry_set,
      references = references,
      blocking_components = blocking_components,
      population_year = if (is.null(specification)) {
        population_year_intent
      } else {
        specification$population_year
      }
    ),
    class = c("creahia_hia_configuration_inspection", "list")
  )
}


.inspection_components <- function(specification) {
  if (is.null(specification)) {
    return(.empty_hia_components())
  }

  dplyr::bind_rows(
    specification$components$crf,
    specification$components$rr,
    specification$components$epi
  )
}


.inspection_references <- function(registry_set) {
  if (is.null(registry_set) || nrow(registry_set) == 0) {
    return(load_crf_references()[0, ])
  }

  tibble::tibble(
    reference_id = unique(registry_set$reference_id)
  ) %>%
    dplyr::left_join(load_crf_references(), by = "reference_id")
}


#' @export
print.creahia_hia_configuration_inspection <- function(x, ...) {
  ready <- identical(x$route, "registry")
  component_counts <- x$components %>%
    dplyr::count(.data$component_type, name = "n")
  component_summary <- if (nrow(component_counts) == 0) {
    "none"
  } else {
    paste(
      paste(component_counts$component_type, component_counts$n, sep = "="),
      collapse = ", "
    )
  }

  cat("HIA configuration inspection\n")
  cat("  Route:", x$route %||% "unavailable", "\n")
  cat("  Ready for Registry routing:", ready, "\n")
  cat("  Input contract:", x$input_contract %||% "not applicable", "\n")
  cat("  Components:", component_summary, "\n")
  cat("  References:", nrow(x$references), "\n")

  if (nrow(x$blocking_components) > 0) {
    cat("  Blocking components:", nrow(x$blocking_components), "\n")
  }

  invisible(x)
}
