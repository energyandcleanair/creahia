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

  hia_polls <- species_to_hiapoll(species)
  scenarios <- names(conc_map)
  hia <- list()

  for(scenario in scenarios) {
    conc_scenario <- conc_map[[scenario]]
    # names(conc_adm) %<>% country.recode(merge_into)

    conc_scenario %>% ldply(.id = 'region_id') -> conc_df

    if(!all(complete.cases(conc_df))) {
      warning('missing values in concentration or population data')
      conc_df %<>% na.omit
    }

    conc_scenario <- conc_df %>% dlply(.(region_id))

    # calculate health impacts
    pop_domain <- conc_scenario %>% ldply(function(df) df %>%
                                            sel(-region_id) %>%
                                            select_if(is.numeric) %>%
                                            apply(2, weighted.mean, w = df[,'pop']) %>%
                                            t %>%
                                            data.frame %>%
                                            mutate(pop = sum(df[,'pop'], na.rm=T)), .id = 'region_id')

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

    hia_scenario <- epi_loc %>%
      sel(region_id, estimate, pop)

    # Add CRF-based health impacts using pre-computed PAF values
    if(!is.null(paf$crf[[scenario]]) && nrow(paf$crf[[scenario]]) > 0) {
      paf_crf_scenario <- paf$crf[[scenario]]
      
      # Join PAF values with epidemiological data
      for(i in which(crfs$Exposure %in% hia_polls)) {
        if(!crfs$Incidence[i] %in% names(epi_loc)) {
          stop("CRFS and EPI data are not matching")
        }
        
        effect_name <- crfs$effectname[i]
        
        # Get PAF values for this effect
        paf_values <- paf_crf_scenario %>%
          filter(!is.na(!!sym(effect_name))) %>%
          sel(region_id, estimate, !!effect_name)
        
        if(nrow(paf_values) > 0) {
          # Join PAF with EPI data
          hia_crf <- epi_loc %>%
            sel(region_id, estimate, pop, !!sym(crfs$Incidence[i])) %>%
            left_join(paf_values, by = c("region_id", "estimate"))
          
          # Calculate health impacts: EPI_RATE × POP × PAF
          hia_crf[[effect_name]] <- hia_crf[[crfs$Incidence[i]]] / 1e5 * hia_crf$pop * hia_crf[[effect_name]]
          
          # Add to main scenario results
          hia_scenario <- hia_scenario %>%
            left_join(hia_crf %>% sel(region_id, estimate, !!effect_name), 
                     by = c("region_id", "estimate"))
        }
      }
    }


    # hia_scenario <- hia_scenario %>%
    #   pivot_longer(-c(region_id, estimate, pop),
    #                names_to = 'Outcome', values_to = 'number') %>%
    #   group_by(region_id, Outcome) %>%
    #   arrange(number) %>%
    #   mutate(
    #     estimate=c('low', 'central', 'high'),
    #   ) %>%
    #   ungroup() %>%
    #   pivot_wider(names_from = Outcome, values_from = number)



    # Add RR-based health impacts using pre-computed PAF values
    if(!is.null(paf$rr[[scenario]]) && nrow(paf$rr[[scenario]]) > 0) {
      pm_mortality <- get_pm_mortality(
        paf_scenario = paf$rr[[scenario]],
        epi_loc = epi_loc
      )
      hia_scenario <- full_join(pm_mortality, hia_scenario)
    }

    hia_scenario <- hia_scenario %>%
      to_long_hia() %>%
      add_double_counted(crfs = crfs, epi = epi) %>%
      add_age_group() %>%
      clean_cause_outcome()

    hia[[scenario]] <- hia_scenario
    print(scenario)
  }

  hia <- hia %>%
    ldply(.id='scenario') %>%
    left_join(regions %>% as.data.frame(row.names = NULL) %>%
                sel(region_id, region_name, iso3 = country_id)) %>%
    tibble()

  return(hia)
}


crf_incidence_to_cause <- function(Incidence) {
  Incidence %>% gsub('_.*', '', .)
}


crf_effectname_to_outcome <- function(effectname) {
  effectname %>% gsub('O3_8h', 'O3', .) %>%
    gsub('_[A-Za-z0-9]*$', '', .) %>%
    gsub('\\.[0-9]*to[0-9]*$', '', .) %>%
    gsub('.*_', '', .)
}


crf_recode_incidence <- function(Incidence, Exposure){
  case_when(
    Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', Incidence) & grepl('NCD\\.LRI', Incidence) ~ gsub('NCD\\.LRI', 'AllCause', Incidence),
    T ~ gsub('AllCauses', "AllCause", Incidence)
  )
}


