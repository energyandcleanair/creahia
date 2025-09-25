#' Compute health impacts from PAF and epidemiological data
#'
#' This function multiplies pre-computed PAF values with epidemiological data
#' to calculate final health impacts
#'
#' @param species Vector of pollutant species
#' @param paf List of PAF values (both RR-based and CRF-based)
#' @param conc_map List of concentration maps by scenario
#' @param regions Spatial regions data
#' @param epi Epidemiological data
#' @param crfs CRF data table
#' @param .mode Computation mode (default: 'change')
#' @return Health impact assessment results
#' @export
compute_hia_impacts <- function(species,
                                paf,
                                conc_map,
                                regions,
                                epi = get_epi(),
                                crfs = get_crfs(),
                                .mode = 'change') {

  polls <- species_to_hiapoll(species)
  scenarios <- names(conc_map)
  impacts <- list()

  # Validate epi data
  # validate_epi(epi)

  for(scenario in scenarios) {

    conc_scenario <- conc_map[[scenario]]
    conc_scenario %>% ldply(.id = 'region_id') -> conc_df

    if(!all(complete.cases(conc_df))) {
      warning('missing values in concentration or population data')
      conc_df %<>% na.omit
    }

    # calculate health impacts
    pop_domain <- conc_df %>%
      group_by(region_id) %>%
      summarise(pop=sum(pop, na.rm=T))

    pop_domain$epi_location_id <- get_epi_location_id(pop_domain$region_id)

    epi_loc <- epi %>%
      sel(-pop, -country) %>%
      right_join(pop_domain %>% sel(region_id, epi_location_id, pop),
                relationship = 'many-to-many',
                by=c(location_id='epi_location_id'))

    # Exclude unmatched countries
    na_iso3s <- epi_loc$region_id[is.na(epi_loc$location_id)]
    if(length(na_iso3s) > 0) {
      warning("Couldn't find epidemiological data for regions ", paste(unique(na_iso3s), collapse=' '), ". Excluding them.")
    }

    epi_loc <- epi_loc %>% filter(!is.na(location_id))

    # Check that there are exactly three estimates per region_id
    if(any(epi_loc %>% group_by(region_id, estimate) %>% summarise(n=n()) %>% pull(n) != 1)){
      stop("Duplicated values in epidemiological data")
    }

    impacts_scenario <- multiply_paf_epi.R(
      paf_scenario = paf %>% filter(scenario == !!scenario),
      epi_loc = epi_loc
    )

    impacts_scenario <- impacts_scenario %>%
      to_long_hia() %>%
      add_double_counted(crfs = crfs, epi = epi) %>%
      add_age_group() %>%
      clean_cause_outcome()

    impacts[[scenario]] <- impacts_scenario
  }

  impacts <- impacts %>%
    ldply(.id='scenario') %>%
    left_join(regions %>%
                as.data.frame(row.names = NULL) %>%
                sel(region_id, region_name, iso3 = country_id)) %>%
    tibble()

  return(impacts)
}

