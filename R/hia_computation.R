#' Compute HIA
#'
#' @param conc_map
#' @param species
#' @param regions
#' @param scenarios
#' @param calc_causes
#' @param gbd_causes
#' @param gemm
#' @param ihme
#' @param epi_version
#' @param ihme_version
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
                        gemm = get_gemm(),
                        rr_sources = c(RR_GEMM, RR_ORIGINAL),
                        ihme = get_ihme(version = ihme_version),
                        epi_version = "default",
                        ihme_version = epi_version,
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
  if(!is.null(scale_base_year) | !is.null(scale_target_year)){
    pop_year <- ifelse(is.null(pop_year), scale_target_year, pop_year)
    messages <- c(
      "scale_base_year and scale_target_year are deprecated. Use pop_year instead, as the year you",
      "want to scale the population to. The base year is now determined automatically based on available data.",
      "\npop_year set to", pop_year
    )
    warning(paste(messages, collapse = " "))
  }


  if(!is.null(calc_causes) | !is.null(gbd_causes)) {
    log_info("Using old parameters for calc_causes and gbd_causes. Ignoring rr_sources.")
    rr_sources <- convert_old_parameters_to_rr_sources(calc_causes = calc_causes, gbd_causes = gbd_causes)
  }

  # Parse sources of relative risk
  rr_sources <- parse_rr_sources(rr_sources)

  paf <- list()
  if(length(rr_sources) > 0) {
    print("Computing paf")
    paf <- compute_hia_paf(conc_map = conc_map,
                           scenarios = scenarios,
                           epi_version = epi_version,
                           ihme_version = ihme_version,
                           rr_sources = rr_sources,
                           ihme = ihme,
                           .mode = .mode)
  }

  print("Computing epi")
  hia <- compute_hia_epi(region = regions,
                         species = species,
                         paf = paf,
                         conc_map = conc_map,
                         epi = epi,
                         crfs = crfs)



  # Population scaling
  # Get the actual population year
  # Ideally it would be in an attribute of hia somewhere
  pop_year_actual <- get_pop_year(year_desired = pop_year)
  pop_year_desired <- pop_year
  if(pop_year_actual != pop_year_desired){
    print(glue("Scaling population from {pop_year_actual} to {pop_year_desired}"))
    hia <- scale_hia_pop(hia, base_year = pop_year_actual, target_year = pop_year_desired)
  }

  return(hia)
}


#' Compute the PAF part of the health impact assessment
#'
#' The PAF part corresponds to causes with constant relative risk per concentration change
#'
#' @param conc_map
#' @param scenarios
#' @param calc_causes
#' @param gemm
#' @param gbd
#' @param ihme
#'
#' @return
#' @export
#'
#' @examples
compute_hia_paf <- function(conc_map,
                            epi_version,
                            ihme_version,
                            rr_sources,
                            scenarios = names(conc_map),
                            ihme = get_ihme(ihme_version),
                            .mode = 'change') {

  paf <- list()
  adult_ages <- get_adult_ages(ihme)


  cause_measure_source <- get_cause_source(rr_sources=rr_sources,
                                            add_measure=T)

  for(scenario in scenarios) {
    message(paste('processing', scenario))

    conc_scenario <- conc_map[[scenario]] %>%
      subset(!is.null(.)) %>%
      subset(!is.na(unique(.))) %>%
      lapply(data.frame) %>%
      bind_rows(.id = 'region_id') %>%
      dlply(.(region_id))


    pg <- progress::progress_bar$new(
      format = "Computing PAF [:bar] :percent :eta",
      total = length(names(conc_scenario))
    )

    paf[[scenario]] <- lapply(names(conc_scenario), function(region_id) {

      tryCatch({
        pg$tick()
        paf_region <- list()
        non_na_cols <- c('conc_baseline_pm25', 'conc_scenario_pm25', 'pop')
        conc <- conc_scenario[[region_id]][complete.cases(conc_scenario[[region_id]][,non_na_cols]),]
        if(nrow(conc)==0) return(NULL)


        for(i in 1:nrow(cause_measure_source)) {
          measure_ <- cause_measure_source$measure[i]
          cause_ <- cause_measure_source$cause[i]
          cs_ms <- paste(cause_, measure_, sep = '_')
          rr_source_ <- cause_measure_source$source[i]
          logger::log_debug(glue("Computing PAF for {cause_} and {measure_} from {source_}"))

          paf_region[[cs_ms]] <- country_paf_perm(pm.base = conc[, 'conc_baseline_pm25'],
                                                  pm.perm = conc[, 'conc_scenario_pm25'],
                                                  pop = conc[, 'pop'],
                                                  region_id = region_id,
                                                  cause = cause_,
                                                  measure = measure_,
                                                  rr_source = rr_source_,
                                                  adult_ages = adult_ages,
                                                  epi_version = epi_version,
                                                  ihme = ihme,
                                                  .region = "inc_China",
                                                  .mode = .mode)
        }

        paf_region <- paf_region %>% bind_rows(.id = 'var') %>%
          mutate(region_id = region_id)

        return(paf_region)
      }, error = function(e) {
        # For instance if country iso3 not in ihme$ISO3
        # or paf not well ordered
        logger::log_warn(paste("Failed for region ", region_id, ": ", e$message))
        warning(paste("Failed for region ", region_id, ": ", e$message))
        return(NULL)
      })
    })
  }

  # Combine all scenarios
  paf %>% lapply(bind_rows)
}


get_nrt_conc <- function(region_ids, conc_name, nrt, conc_map,
                         units_multiplier = 1, nrt_flag = NULL, nrt_flag_value = 2) {
  conc_map[region_ids] %>%
    lapply(function(m) {
      if(!is.null(nrt_flag)) nrt = ifelse(m[,nrt_flag] == nrt_flag_value, 0, nrt)

      m[, conc_name] %>%
        multiply_by(units_multiplier) %>%
        subtract(nrt) %>%
        pmax(0) %>%
        weighted.mean(w = m[, 'pop'], na.rm = T)
    }) %>% unlist %>% unname
}


compute_hia_epi <- function(species,
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

    for(i in which(crfs$Exposure %in% hia_polls)) {

      if(!crfs$Incidence[i] %in% names(epi_loc)) {
        stop("CRFS and EPI data are not matching")
      }

      species_name <- hiapoll_to_species(crfs$Exposure[i])

      if(grepl('nrt', species_name)) {
        sourceconcs <- get_nrt_conc(hia$GID, species_name, 0, conc_adm = conc_adm)
      } else {
        base_name <- species_name %>% paste0('conc_baseline_',.)
        perm_name <- species_name %>% paste0('conc_scenario_',.)
        nrt_flag <- NULL # ifelse(grepl('NCD\\.LRI_', crfs$Incidence[i]), 'grump', NULL)

        cfconc <- crfs$Counterfact[i]

        base_concs <- get_nrt_conc(region_ids = hia_scenario$region_id,
                                   conc_name = base_name,
                                   nrt = cfconc,
                                   conc_map = conc_scenario,
                                   units_multiplier = crfs$Units.multiplier[i],
                                   nrt_flag = nrt_flag)

        perm_concs <- get_nrt_conc(region_ids = hia_scenario$region_id,
                                   conc_name = perm_name,
                                   nrt = cfconc,
                                   conc_map = conc_scenario,
                                   units_multiplier = crfs$Units.multiplier[i],
                                   nrt_flag = nrt_flag)

        source_concs <- perm_concs - base_concs
      }

      RR.ind <- match(hia_scenario$estimate, names(crfs))
      RRs <- crfs[i, RR.ind] %>% unlist %>% unname


      hia_scenario[[crfs$effectname[i]]] <- epi_loc[[crfs$Incidence[i]]] / 1e5 * epi_loc$pop *
        (1 - exp(-log(RRs)*source_concs / crfs$Conc.change[i]))
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



    # Add PM mortality from PAF x EPI
    if(!is.null(paf[[scenario]]) && nrow(paf[[scenario]]) > 0) {

      pm_mortality <- get_pm_mortality(
        paf_scenario = paf[[scenario]],
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


to_long_hia <- function(hia) {
  hia %>%
    pivot_longer(c(-where(is.character), -where(is.factor), -pop),
                 names_to = 'Outcome', values_to = 'number') %>%
    mutate(Outcome = Outcome %>% gsub('O3_8h', 'O3', .),
           Pollutant = Outcome %>% gsub('.*_', '', .) %>% toupper,
           Cause = Outcome %>% gsub('_.*', '', .)) %>%
    mutate(Outcome = Outcome %>% gsub('_[A-Za-z0-9]*$', '', .) %>%
             gsub('\\.[0-9]*to[0-9]*$', '', .) %>%
             gsub('.*_', '', .),
           Pollutant = case_when(Pollutant == 'O3' ~ 'O3_8h',
                                 TRUE ~ Pollutant))
}


# define a function to calculate the hazard ratio for a specific concentration, cause and age group
get_hazard_ratio <- function(pm,
                             rr,
                             .age,
                             .cause
                             ) {

  rr_filtered <- rr %>%
    dplyr::filter(cause == .cause, age == .age)

  rr_filtered %>% sel(low, central, high) %>%
    apply(2, function(y) approx(x = rr_filtered$exposure, y, xout = pm)$y)

}


# total fossil fuel PAF for a permutation run
country_paf_perm <- function(pm.base,
                             pm.perm,
                             pop,
                             region_id,
                             cause,
                             measure,
                             rr_source,
                             epi_version,
                             ihme = get_ihme(version = epi_version),
                             adult_ages = get_adult_ages(ihme),
                             .region = "inc_China",
                             .mode = 'change') { # change or attribution?


  rr <- get_rr(rr_source) %>%
    filter(cause == !!cause)

  # Get age weights ---
  ages <- unique(rr$age) %>%
    deduplicate_adult_ages()

  stopifnot(
    !AGE_ADULTS %in% ages | length(intersect(ages, AGE_ADULTS_SPLIT))==0
  )

  age_weights <- ihme %>%
    mutate(age=recode_age(age)) %>%
    mutate(cause_short = case_when(cause_short==CAUSE_LRI & age==AGE_CHILDREN ~ CAUSE_LRICHILD,
                                   T ~ cause_short)) %>%
    dplyr::filter(location_id==get_epi_location_id(region_id),
                  cause_short == !!cause,
                  measure_name == measure,
                  age %in% ages,
                  estimate == 'central')

  if(nrow(age_weights) == 0) {
    warning(glue("No age weights found for {region_id} and {cause} and {measure}"))
    return(NULL)
  }

  if(length(age_weights$age) != length(ages)) {
    stop("Unmatching age weights")
  }

  # Ensuring ages and age_weights$age are in the same order
  age_weights <- age_weights[match(ages, age_weights$age),]
#
#
#   age.specific <- c('NCD.LRI', 'Stroke', 'IHD')
#
#   if(cause %in% age.specific) {
#     ages <- adult_ages
#     age_weights <- ihme %>%
#       dplyr::filter(location_id==get_epi_location_id(region_id),
#                     cause == !!cause,
#                     measure_name == measure,
#                     age %in% ages,
#                     estimate == 'central')
#     # Ensuring ages and age_weights$age are in the same order
#     age_weights <- age_weights[match(ages, age_weights$age),]
#   } else {
#     age_weights <- data.frame(val = 1)
#     if(grepl('child', cause)) ages = 'Under 5' else ages = '25+'
#   }

  rr.base <- ages %>% sapply(function(.a) get_hazard_ratio(pm.base, rr = rr, .cause = cause, .age = .a),
                             simplify = 'array')

  if(.mode == 'change') {
    rr.perm <- ages %>% sapply(function(.a) get_hazard_ratio(pm.perm, rr = rr, .cause = cause, .age = .a),
                               simplify = 'array')


    paf <- get_paf_from_rr_lauri(
      rr_base = rr.base,
      rr_perm = rr.perm,
      age_weights = age_weights$val,
      pop = pop,
      cause=cause,measure=measure
    )

  } else {
    stop("Attribution mode not implemented yet")
    #TODO implement the method with this formula
    # paf.perm <- (1 - (1 / rr.base)) * (1 - pm.perm / pm.base)
  }

  return(paf)
}


scale_hia_pop <- function(hia, base_year = 2015, target_year = 2019) {

  message(glue("Scaling population from {base_year} to {target_year}"))
  pop_proj <- get_pop_proj()

  # scale population from year of population data to target year of estimates
  pop_scaling <- pop_proj %>%
    filter(year %in% c(base_year, target_year),
           !is.na(iso3)) %>%
    group_by(iso3, year) %>%
    summarise_at('pop', sum) %>%
    mutate(year = case_when(year == base_year ~ 'base',
                            year == target_year ~ 'target'))

  hia_scaled <- pop_scaling %>%
    spread(year, pop) %>%
    mutate(scaling=target/base) %>%
    sel(iso3, scaling) %>%
    left_join(hia, .) %>%
    (function(df) df %>% mutate_if(is.numeric, multiply_by, df$scaling)) %>%
    sel(-scaling)

  return(hia_scaled)
}


totalise_hia <- function(hia, .groups = c("scenario", "iso3", "region_id", "region_name")) {

  hia_adm <- hia %>%
    group_by(across(c(scenario, estimate, all_of(.groups)))) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    pivot_longer(is.numeric, names_to = 'Outcome', values_to = 'Number')

  hia_total <- hia_adm %>%
    group_by(across(c(all_of(.groups), scenario, estimate, Outcome))) %>%
    summarise_if(is.numeric, sum, na.rm = T) %>%
    spread(estimate, Number)

  # Add cause long name & unit
  hia_total$pollutant <- lapply(str_split(hia_total$Outcome, "_"),
                                function(x) {tail(x, 1)}) %>% unlist
  hia_total$unit <- lapply(str_split(hia_total$Outcome, "_"),
                           function(x) {ifelse(length(x) == 3, x[2], "")}) %>% unlist
  hia_total$cause <- lapply(str_split(hia_total$Outcome, "_"),
                            function(x) {x[1]}) %>% unlist

  hia_total %>% left_join(get_dict() %>% rename(cause = Code, cause_name = Long.name)) %>%
    dplyr::select(region_id, region_name, iso3, scenario, cause, cause_name,
                  unit, pollutant, central, high, low) %>%
    arrange(region_name, cause)
}


#' Summarise hia results
#'
#' @param hia_total
#' @param make_ci_fun
#' @param res_cols
#' @param dict
#'
#' @return
#' @export
#'
#' @examples
make_hia_table <- function(hia_total,
                           make_ci_fun = make_nothing,
                           res_cols = c('low', 'central', 'high'),
                           dict = get_dict()) {

  if('Cause' %notin% names(hia_total)) {
    hia_total <- hia_total %>% separate(Outcome, c('Cause', 'Outcome', 'Pollutant'), '_')
    ind <- is.na(hia_total$Pollutant)
    hia_total$Pollutant[ind] <- hia_total$Outcome[ind]
    hia_total$Outcome[ind] <- hia_total$Cause[ind]
  }

  if('Outcome_long' %notin% names(hia_total)) {
    hia_table <- hia_total %>% add_long_names(dict = dict) %>%
      sel(scenario, Cause_long, Outcome_long, Pollutant, all_of(res_cols))
  }

  deaths <- hia_table %>% filter(Outcome_long == 'deaths')
  morb <- hia_table %>% filter(!grepl('deaths|life lost|prev|birthwe', Outcome_long),
                               !is.na(Outcome_long))

  deaths <- deaths %>% filter(Outcome_long == 'deaths') %>%
    filter(!(Cause_long == 'all' & Pollutant == 'PM25'))

  bind_rows(deaths %>% make_ci_fun %>% arrange(desc(Pollutant)),
            morb %>% make_ci_fun %>% arrange(Outcome_long)) %>%
    sel(Outcome_long, Cause_long, everything()) %>%
    mutate(Cause_long = recode(Cause_long, deaths = 'total'))
}


# a simple renaming function that accepts string variables as arguments; !!newname := !!oldname was causing grief
rename_str <- function(df, oldname, newname) { names(df)[names(df) == oldname] <- newname; df }


add_long_names <- function(df, cols = c('Outcome', 'Cause'), dict = get_dict()) {
  for(cn in intersect(names(df), cols)) {
    out_cn <- paste0(cn, '_long')
    df <- df %>%
      left_join(dict %>% rename_str('Code', cn)) %>%
      rename_str('Long.name', out_cn)
    df[[out_cn]] <- df[[out_cn]] %>% na.cover(df[[cn]])
  }

  if('Cause_long' %in% names(df)) df$Cause_long[grep('non-comm', df$Cause_long)] <- 'all'

  if('Pollutant' %in% names(df)) df$Pollutant <- df$Pollutant %>% recode(PM25 = "PM2.5")
  return(df)
}


hiapoll_species_corr <- function() {
  list(
    "PM25" = "pm25",
    "NO2" = "no2",
    "O3_8h" = "o3_8h",
    "SO2" = "so2",
    'PM10' = 'tpm10')
}


species_to_hiapoll <- function(species) {
  corr <- hiapoll_species_corr()
  names(corr)[which(corr %in% species)]
}


hiapoll_to_species <- function(hiapoll) {
  corr <- hiapoll_species_corr()
  corr[hiapoll] %>% unlist() %>% as.vector()
}


#' Key function to add a double_counted field to hia results
#'
#' @param hia
#' @param crfs
#' @param epi
#'
#' @return
#' @export
#'
#' @examples
add_double_counted <- function(hia, crfs, epi) {

  # Use CRFS double counted field first
  joined <- hia %>%
    left_join(crfs %>%
                mutate(Cause = crf_incidence_to_cause(Incidence),
                       Outcome = crf_effectname_to_outcome(effectname),
                       Pollutant = Exposure) %>%
                select(Cause, Outcome, Pollutant, double_counted = Double.Counted),
              by = c('Cause', 'Outcome', 'Pollutant'))

  # Except PM25, all of them should have been found in CRFs
  if(nrow(joined %>% filter(is.na(double_counted) & Pollutant != 'PM25' & number > 0)) > 0) {
    stop('merged has failed in double counting detection')
  }

  # Manual for epi PM25
  joined[joined$Pollutant == 'PM25' &
           any(joined$Cause == CAUSE_NCDLRI) & # detect if NCD+LRI is being used (e.g. from GEMM or FUSION)
           joined$Cause %in% CAUSE_NCDLRI_INCLUDED &
           joined$Outcome %in% c(MEASURE_YLLS, MEASURE_DEATHS),
         'double_counted'] <- TRUE

  joined[joined$Pollutant == 'PM25' &
           any(joined$Cause == CAUSE_CV) & # detect if NCD+LRI is being used (e.g. from GEMM or FUSION)
           joined$Cause %in% CAUSE_CV_INCLUDED &
           joined$Outcome %in% c(MEASURE_YLLS, MEASURE_DEATHS),
         'double_counted'] <- TRUE

  joined <- joined %>%
    mutate(double_counted = tidyr::replace_na(double_counted, FALSE))

  return(joined)
}


add_age_group <- function(hia) {
  hia <- hia %>% mutate(AgeGrp = case_when(grepl("LRI\\.child", Cause) ~ "0-4",
                                           grepl("PTB|LBW", Cause) ~ "Newborn",
                                           grepl("0to17|1to18", Cause) ~ "0-18",
                                           T ~ "25+"))
  return(hia)
}


clean_cause_outcome <- function(hia) {
  # Clean asthma
  hia$Cause[grep('exac|sthma', hia$Cause)] <- 'Asthma'

  # Valuation is now different between deaths and deaths.child
  hia$Outcome[grepl("LRI\\.child", hia$Cause) & (hia$Outcome == 'Deaths')] <- 'Deaths.child'
  return(hia)
}

add_total_deaths_and_costs <- function(df) {
  df %<>% filter(!double_counted, !grepl("economic costs", Outcome)) %>%
    summarise(across(c(number=cost_mn_currentUSD), sum), .groups = 'keep') %>%
    mutate(Outcome = "economic costs", unit="million USD", Cause='AllCause', Pollutant='All', double_counted = F) %>%
    bind_rows(df %>% filter(!grepl("economic costs", Outcome)))

  df %<>% filter(!double_counted, grepl("Death", Outcome)) %>%
    summarise(across(c(number), sum), .groups = 'keep') %>%
    mutate(Outcome = "deaths, total", Cause='AllCause', Pollutant='All', unit="death", double_counted = T) %>%
    bind_rows(df %>% filter(!grepl("deaths, total", Outcome)))

  return(df)
}

