#' Compute HIA
#'
#' @param conc_map
#' @param species
#' @param regions
#' @param scenarios
#' @param calc_causes
#' @param gbd_causes
#' @param gemm
#' @param gbd
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
#'
#' @return
#' @export
#'
#' @examples
compute_hia <- function(conc_map,
                        species,
                        regions,
                        scenarios = names(conc_map),
                        calc_causes = 'GEMM and GBD',
                        gbd_causes = "default", # which causes to use GDB risk functions for; 'all' for all available, default: only when GEMM not available
                        gemm = get_gemm(),
                        gbd = NULL,
                        ihme = get_ihme(version = ihme_version),
                        epi_version = "default",
                        ihme_version = epi_version,
                        epi = get_epi(version = epi_version),
                        crfs_version = "default",
                        crfs = get_crfs(version = crfs_version),
                        diagnostic_folder = 'diagnostic',
                        .mode = 'change',

                        pop_year = NULL,
                        scale_base_year = NULL,
                        scale_target_year = NULL,

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

  if(gbd_causes[1] == 'default' & calc_causes[1] == 'GBD only') gbd_causes <- 'all'

  if(grepl('GEMM|GBD', calc_causes[1])) calc_causes <- get_calc_causes(calc_causes[1])
  if(calc_causes[1] == 'none') calc_causes <- character(0)
  calc_causes_wo_outcome <- calc_causes %>% gsub('_.*', '', .) %>%
    unique()

  if(gbd_causes[1] == 'default')
    gbd_causes <- calc_causes_wo_outcome %>% subset(!(. %in% unique(gemm$cause)))

  if(is.null(gbd)) gbd <- get_gbd(gbd_causes)

  gbd_causes <- gbd$cause_short %>% unique %>%
    subset(. %in% calc_causes_wo_outcome)
  gemm_causes <- calc_causes_wo_outcome %>% subset(!(. %in% gbd_causes))

  if(length(gemm_causes) > 0) logger::log_info('Using GEMM risk functions for ', paste(gemm_causes, collapse = ", "))
  if(length(gbd_causes) > 0) message('Using GBD risk functions for ', paste(gbd_causes, collapse = ", "))
  if(length(calc_causes_wo_outcome) == 0) logger::log_info('Not using GBD or GEMM risk functions')

  paf <- list()
  if(length(calc_causes) > 0) {
    print("Computing paf")
    paf <- compute_hia_paf(conc_map = conc_map,
                           scenarios = scenarios,
                           calc_causes = calc_causes,
                           epi_version = epi_version,
                           ihme_version = ihme_version,
                           gemm = gemm,
                           gbd = gbd,
                           ihme = ihme,
                           .mode = .mode)
  }

  print("Computing epi")
  hia <- compute_hia_epi(region = regions,
                         species = species,
                         paf = paf,
                         conc_map = conc_map,
                         epi = epi,
                         crfs = crfs,
                         calc_causes = calc_causes)



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
                            ihme_version = ihme_version,
                            scenarios = names(conc_map),
                            calc_causes = get_calc_causes(),
                            gemm = get_gemm(),
                            gbd = get_gbd(),
                            ihme = get_ihme(ihme_version),
                            .mode = 'change') {

  paf <- list()
  adult_ages <- get_adult_ages(ihme)

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

    paf[[scenario]] <- foreach(region_id = names(conc_scenario)) %dopar% {

      tryCatch({
        pg$tick()
        paf_region <- list()
        non_na_cols <- c('conc_baseline_pm25', 'conc_scenario_pm25', 'pop')
        conc <- conc_scenario[[region_id]][complete.cases(conc_scenario[[region_id]][,non_na_cols]),]
        if(nrow(conc)==0) return(NULL)

        for(cs_ms in calc_causes) {
          logger::log_debug(cs_ms)
          cs.ms <- cs_ms %>% strsplit('_') %>%
            unlist

          paf_region[[cs_ms]] <- country_paf_perm(pm.base = conc[, 'conc_baseline_pm25'],
                                                  pm.perm = conc[, 'conc_scenario_pm25'],
                                                  pop = conc[, 'pop'],
                                                  region_id = region_id,
                                                  cause = cs.ms[1],
                                                  measure = cs.ms[2],
                                                  adult_ages = adult_ages,
                                                  epi_version = epi_version,
                                                  gemm = gemm,
                                                  gbd = gbd,
                                                  ihme = ihme,
                                                  .region = "inc_China",
                                                  .mode = .mode)
        }

        paf_region <- paf_region %>% bind_rows(.id = 'var') %>%
          mutate(region_id = region_id)
      }, error = function(e) {
        # For instance if country iso3 not in ihme$ISO3
        logger::log_warn("Failed for region ", region_id)
        paf_region <- NULL
      })
      return(paf_region)
    }
  }
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
                            calc_causes = get_calc_causes(),
                            .mode = 'change') {

  hia_polls <- species_to_hiapoll(species)
  scenarios <- names(conc_map)
  hia <- list()

  for(scenario in scenarios) {
    conc_scenario <- conc_map[[scenario]]
    # names(conc_adm) %<>% country.recode(merge_into)
    conc_scenario <- conc_scenario %>%
      ldply(.id = 'region_id') %>%
      dlply(.(region_id))

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
                # multiple='all',
                by=c(location_id='epi_location_id'))
      # sel(-epi_iso3)

    # Exclude unmatched countries
    na_iso3s <- epi_loc$region_id[is.na(epi_loc$location_id)]
    if(length(na_iso3s) > 0) {
      warning("Couldn't find epidemiological data for regions ", na_iso3s, ". Excluding them.")
    }

    epi_loc <- epi_loc %>% filter(!is.na(location_id))

    # Check that there are exactly three estimates per region_id
    if(any(epi_loc %>% group_by(region_id, estimate) %>% summarise(n=n()) %>% pull(n) != 1)){
      stop("Duplicated values in epidemiological data")
    }

    hia_scenario <- epi_loc %>% sel(region_id, estimate, pop)

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

    # Add PM mortality from PAF x EPI
    if(!is.null(paf[[scenario]]) && nrow(paf[[scenario]]) > 0) {
      paf_wide <- paf[[scenario]] %>%
        gather(estimate, val, low, central, high) %>%
        mutate(var = paste0('paf_', var)) %>%
        spread(var, val)

      paf_wide <- epi_loc %>% left_join(paf_wide)

      pm_mort <- paf_wide %>% sel(region_id, estimate)

      available_causes <- intersect(unique(paf[[scenario]]$var), names(epi_loc))

      for(cs in intersect(available_causes, calc_causes)) {
        pm_mort[[cs]] <- paf_wide[[cs]] / 1e5 * paf_wide[[paste0('paf_', cs)]] * paf_wide$pop
      }

      names(pm_mort)[sapply(pm_mort, is.numeric)] <- names(pm_mort)[sapply(pm_mort, is.numeric)] %>%
        paste0('_PM25')
      hia_scenario <- full_join(pm_mort, hia_scenario)
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
                             .age = '25+',
                             .cause = 'NCD.LRI',
                             .region = 'inc_China',
                             gemm = get_gemm(),
                             gbd = get_gbd()) {

  gbd.causes <- gbd$cause_short %>% unique

  if(.cause %in% gbd.causes) {
    hr.all <- gbd %>% dplyr::filter(cause_short == .cause, age == .age)
    hr.all %>% sel(low, central, high) %>%
      apply(2, function(y) approx(x = hr.all$exposure, y, xout = pm)$y)
  } else {
    p <- gemm %>%
      dplyr::filter(age == .age, cause == .cause, region == .region) %>%
      spread(param, value)
    z <- pmax(0, pm-2.4, na.rm = T)
    g <- log(1 + z / p$a) / (1 + exp((p$u-z) / p$p))
    se <- c(-p$se, 0, p$se)
    hr.out <- (matrix(g) %*% (p$t + 2 * se)) %>% exp
    colnames(hr.out) <- c('low', 'central', 'high')
    return(hr.out)
  }
}


# total fossil fuel PAF for a permutation run
country_paf_perm <- function(pm.base,
                             pm.perm,
                             pop,
                             region_id,
                             cause,
                             measure,
                             epi_version,
                             gemm = get_gemm(),
                             gbd = get_gbd(),
                             ihme = get_ihme(version = epi_version),
                             adult_ages = get_adult_ages(ihme),
                             .region = "inc_China",
                             .mode = 'change') { # change or attribution?

  age.specific <- c('NCD.LRI', 'Stroke', 'IHD')

  if(cause %in% age.specific) {
    ages <- adult_ages
    age_weights <- ihme %>%
      dplyr::filter(location_id==get_epi_location_id(region_id),
                    cause_short == cause,
                    measure_name == measure,
                    age %in% ages,
                    estimate == 'central')
    # Ensuring ages and age_weights$age are in the same order
    age_weights <- age_weights[match(ages, age_weights$age),]
  } else {
    age_weights <- data.frame(val = 1)
    if(grepl('child', cause)) ages = 'Under 5' else ages = '25+'
  }

  rr.base <- ages %>% sapply(function(.a) get_hazard_ratio(pm.base, gbd = gbd, gemm = gemm,
                                             .cause = cause, .age = .a, .region = .region),
                             simplify = 'array')

  if(.mode == 'change') {
    rr.perm <- ages %>% sapply(function(.a) get_hazard_ratio(pm.perm, gbd = gbd, gemm = gemm,
                                               .cause = cause, .age = .a, .region = .region),
                               simplify = 'array')


    paf.perm <- rr.perm / rr.base - 1

    # IMPORTANT: should be paf.perm <- 1 - rr.base / rr.perm
    # Will fix in a new branch

  } else {
    paf.perm <- (1 - (1 / rr.base)) * (1 - pm.perm / pm.base)
  }

  if(length(dim(paf.perm)) == 2) {
    # in case the get_hazard_ratio function didn't return an array
    paf.perm %>%
      t %>%
      orderrows %>%
      apply(2, weighted.mean, age_weights$val)
  } else {
    tryCatch({

      # old <- paf.perm %>%
      #   apply(1:2, stats::weighted.mean, age_weights$val) %>%
      #   orderrows %>%
      #   apply(2, weighted.mean, w = pop)

      # matrixStats two orders of magnitude faster
      # also removed orderrows (though checking it is already ordered)
      check_order <- function(x){
        if(!is.matrix(x)){
          # just for the edge case when you only have one row
          x <- t(as.matrix(x))
        }
        ok <- all(x[,'low'] <= x[,'central'])
        ok <- ok & all(x[,'central'] <= x[,'high'])
        if(!ok){
          warning("Failed to satisfy low > central or central > high. Ordering manually")
          #Note: I (Hubert) think the whole low, central, high isn't correct. Taking the
          # ratio of low, ratio of central and ratio of high doesn't necessarily lead to low, central, high...
          x <- orderrows(x)
          }
        x
      }

      new <- paf.perm %>%
        apply(2, matrixStats::rowWeightedMeans, w = age_weights$val) %>%
        check_order() %>%
        apply(2, weighted.mean, w = pop)

      # if(any(round(old,6) != round(new,6))){
      #   stop("Broke something")
      # }
      #
      return(new)
    }, error = function(e) {
      warning("Failed for region ", region_id, cause, e)
      return(NULL)
    })
  }
}


# country_paf <- function(pm, pop, cy, cs, ms, adult_ages = get_adult_ages(),
#                         .region = "inc_China", gemm = get_gemm(), gbd = get_gbd()) {
#
#   if(grepl('child', cs)) {
#     ages <- 'Under 5'
#     w <- data.frame(val = 1)
#   } else {
#     ages <- adult_ages
#     w <- ihme %>% dplyr::filter(ISO3 == cy, cause_short == cs, measure_name == ms,
#                                 age %in% ages, estimate == 'central')
#   }
#
#   rr <- ages %>% sapply(function(.a) hr(pm, .cause = cs, .age = .a,
#                                         .region = .region, gemm = gemm, gbd = gbd),
#                   simplify = 'array')
#   paf <- 1 - 1 / rr
#
#   if(length(dim(paf)) == 2) {
#     paf %>% t %>%
#       apply(2, weighted.mean, w$val) # in case the hr function didn't return an array
#   } else {
#     paf %>% apply(1:2, weighted.mean, w$val, na.rm = T) %>%
#       apply(2, weighted.mean, w = pop, na.rm = T)
#   }
# }


scale_hia_pop <- function(hia, base_year = 2015, target_year = 2019) {

  pop_proj <- get_pop_proj()

  # scale population from year of population data to target year of estimates
  pop_scaling <- pop_proj %>% filter(year %in% c(base_year, target_year), AgeGrp != 'Newborn', !is.na(iso3)) %>%
    sel(-AgeGrp) %>%
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
           any(joined$Cause == 'NCD.LRI') & # detect if GEMM risk function for NCD+LRI is being used
           !joined$Cause %in% c('NCD.LRI', 'LRI.child') &
           joined$Outcome %in% c('YLLs', 'Deaths'),
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

