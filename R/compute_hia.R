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
#' @param .mode
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
                        .mode = 'change',
                        # Years
                        pop_year = NULL,
                        scale_base_year = NULL,
                        scale_target_year = NULL,
                        # Old parameters
                        calc_causes = NULL,
                        gbd_causes = NULL,
                        ...){


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

  message("Computing PAF")
  paf <- compute_hia_paf(
    conc_map = conc_map,
    species = species,
    regions = regions,
    scenarios = scenarios,
    epi_version = epi_version,
    rr_sources = rr_sources,
    crfs = crfs,
    .mode = .mode
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
  pop_year_actual <- get_pop_year(year_desired = pop_year)
  pop_year_desired <- pop_year
  if(pop_year_actual != pop_year_desired){
    message(glue("Scaling population from {pop_year_actual} to {pop_year_desired}"))
    impacts <- scale_hia_pop(impacts, base_year = pop_year_actual, target_year = pop_year_desired)
  }

  return(impacts)
}
