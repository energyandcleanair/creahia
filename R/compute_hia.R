#' Compute HIA
#'
#' @param conc_map
#' @param species
#' @param regions
#' @param scenarios
#' @param calc_causes
#' @param gbd_causes
#' @param epi_version
#' @param epi
#' @param crfs_version
#' @param crfs
#' @param diagnostic_folder
#' @param scale_base_year DEPRECATED
#' @param scale_target_year
#' @param ...
#' @param rr_version
#' @param pop_year
#' @param gbd_rr
#'
#' @return
#' @export
#'
#' @examples
compute_hia <- function(conc_map,
                        species,
                        regions,
                        scenarios = names(conc_map),
                        rr_sources = c(RR_GEMM, RR_ORIGINAL),
                        epi_version = "default",
                        epi = get_epi(version = epi_version),
                        crfs_version = "default",
                        crfs = get_crfs(version = crfs_version),
                        diagnostic_folder = 'diagnostic',
                        crf_compute = c("legacy", "registry"),
                        # Years
                        pop_year = NULL,
                        scale_base_year = NULL,
                        scale_target_year = NULL,
                        # Old parameters
                        calc_causes = NULL,
                        gbd_causes = NULL,
                        ...){

  argument_provenance <- list(
    rr_sources_supplied = !missing(rr_sources),
    epi_supplied = !missing(epi),
    crfs_supplied = !missing(crfs)
  )
  supplied_population_years <- list(
    pop_year = pop_year,
    scale_base_year = scale_base_year,
    scale_target_year = scale_target_year
  )

  crf_compute <- match.arg(crf_compute)

  if (.hia_compatibility_routing_enabled() && .hia_dual_run_enabled()) {
    stop(
      paste(
        "HIA compatibility route activation and numerical dual-run mode",
        "cannot be enabled in the same call."
      ),
      call. = FALSE
    )
  }

  if (is_crfs_set(crfs)) {
    crf_compute <- "registry"
  }

  # Fix inputs: if scale_base_year or scale_target_year is not null,
  # warn user
  if(!is.null(scale_base_year) || !is.null(scale_target_year)){
    pop_year <- ifelse(is.null(pop_year), scale_target_year, pop_year)
    messages <- c(
      "scale_base_year and scale_target_year are deprecated. Use pop_year instead, as the year you",
      "want to scale the population to. The base year is now determined automatically based on available data.",
      "\npop_year set to", pop_year
    )
    warning(paste(messages, collapse = " "))
  }


  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = rr_sources,
    calc_causes = calc_causes,
    gbd_causes = gbd_causes
  )

  # Parse sources of relative risk
  rr_sources <- parse_rr_sources(rr_selection$rr_sources)

  validate_crfs_against_epi(crfs = crfs, epi = epi, species = species)

  compatibility_observation <- NULL
  if (.hia_compatibility_resolution_enabled()) {
    population_year_intent <- .capture_population_year_intent(
      pop_year = supplied_population_years$pop_year,
      scale_base_year = supplied_population_years$scale_base_year,
      scale_target_year = supplied_population_years$scale_target_year,
      resolved_pop_year = pop_year
    )
    compatibility_context <- c(
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
    compatibility_observation <- .observe_hia_compatibility_boundary(
      crfs = crfs,
      rr_sources = rr_sources,
      epi = epi,
      context = compatibility_context,
      population_year_intent = population_year_intent
    )

    if (.hia_compatibility_routing_enabled()) {
      compute_inputs <- .activate_hia_compatibility_route(
        compatibility_observation
      )
      crfs <- compute_inputs$crfs
      rr_sources <- compute_inputs$rr_sources
      crf_compute <- compute_inputs$crf_compute
    }
  }

  message("Computing PAF")
  paf <- compute_hia_paf(
    conc_map = conc_map,
    species = species,
    regions = regions,
    scenarios = scenarios,
    epi_version = epi_version,
    rr_sources = rr_sources,
    crfs = crfs,
    diagnostic_folder = diagnostic_folder,
    crf_compute = crf_compute
  )

  message("Computing impacts")
  impacts <- compute_hia_impacts(
    region = regions,
    species = species,
    paf = paf,
    conc_map = conc_map,
    epi = epi,
    crfs = crfs
  )

  # Population scaling
  # Get the actual population year
  # Ideally it would be in an attribute of hia somewhere
  population_scale <- NULL
  if(!is.null(pop_year)){
    pop_year_actual <- get_pop_year(year_desired = pop_year)
    pop_year_desired <- pop_year
    if(pop_year_actual != pop_year_desired){
      message(glue("Scaling population from {pop_year_actual} to {pop_year_desired}"))
      impacts <- scale_hia_pop(impacts, base_year = pop_year_actual, target_year = pop_year_desired)
      population_scale <- list(
        base_year = pop_year_actual,
        target_year = pop_year_desired
      )
    }
  }

  if (.hia_dual_run_enabled()) {
    .execute_hia_compatibility_dual_run(
      observation = compatibility_observation,
      legacy_impacts = impacts,
      conc_map = conc_map,
      species = species,
      regions = regions,
      scenarios = scenarios,
      epi_version = epi_version,
      epi = epi,
      population_scale = population_scale
    )
  }

  return(impacts)
}
