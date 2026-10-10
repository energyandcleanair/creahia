.observe_default_hia_readiness <- function(
  crfs,
  rr_sources,
  epi,
  epi_version,
  population_year_intent
) {
  observation <- tryCatch(
    {
      result <- .resolve_hia_component_readiness(
        crfs = crfs,
        rr_sources = rr_sources,
        epi = epi,
        crf_input_mode = "legacy_profile",
        crf_selection_id = "legacy_profile_default",
        epi_version = epi_version
      )
      specification <- .new_default_hia_specification(
        readiness = result,
        population_year_intent = population_year_intent
      )

      list(
        status = "resolved",
        result = result,
        specification = specification
      )
    },
    error = function(error) {
      list(status = "error", error = conditionMessage(error))
    }
  )

  observer <- getOption("creahia.hia_readiness_shadow_observer")
  if (is.function(observer)) {
    tryCatch(
      observer(observation),
      error = function(error) invisible(NULL)
    )
  }

  invisible(observation)
}


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

  supplied_population_years <- list(
    pop_year = pop_year,
    scale_base_year = scale_base_year,
    scale_target_year = scale_target_year
  )

  observe_default_readiness <-
    missing(rr_sources) &&
    missing(epi) &&
    missing(crfs) &&
    identical(epi_version, "default") &&
    identical(crfs_version, "default") &&
    is.null(calc_causes) &&
    is.null(gbd_causes)

  crf_compute <- match.arg(crf_compute)

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


  if(!is.null(calc_causes) || !is.null(gbd_causes)) {
    log_info("Using old parameters for calc_causes and gbd_causes. Ignoring rr_sources.")
    rr_sources <- convert_old_parameters_to_rr_sources(calc_causes = calc_causes, gbd_causes = gbd_causes)
  }

  # Parse sources of relative risk
  rr_sources <- parse_rr_sources(rr_sources)

  validate_crfs_against_epi(crfs = crfs, epi = epi, species = species)

  if (observe_default_readiness && identical(crf_compute, "legacy")) {
    population_year_intent <- .capture_population_year_intent(
      pop_year = supplied_population_years$pop_year,
      scale_base_year = supplied_population_years$scale_base_year,
      scale_target_year = supplied_population_years$scale_target_year,
      resolved_pop_year = pop_year
    )
    .observe_default_hia_readiness(
      crfs = crfs,
      rr_sources = rr_sources,
      epi = epi,
      epi_version = epi_version,
      population_year_intent = population_year_intent
    )
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
  if(!is.null(pop_year)){
    pop_year_actual <- get_pop_year(year_desired = pop_year)
    pop_year_desired <- pop_year
    if(pop_year_actual != pop_year_desired){
      message(glue("Scaling population from {pop_year_actual} to {pop_year_desired}"))
      impacts <- scale_hia_pop(impacts, base_year = pop_year_actual, target_year = pop_year_desired)
    }
  }

  return(impacts)
}
