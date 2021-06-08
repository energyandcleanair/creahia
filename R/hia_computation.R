hiapoll_species_corr <- function(){
  list("PM25"="pm25",
  "NO2"="no2",
  "O3_8h"="o3",
  "SO2"="so2")
}

species_to_hiapoll <- function(species){
  corr <- hiapoll_species_corr()
  names(corr)[which(corr %in% species)]
}

hiapoll_to_species <- function(hiapoll){
  corr <- hiapoll_species_corr()
  corr[hiapoll] %>% unlist() %>% as.vector()
}


compute_hia <- function(conc_map,
                        species,
                        regions,
                        scenarios=names(conc_map),
                        calc_causes=get_calc_causes(),
                        gemm=get_gemm(),
                        gbd=get_gbd(),
                        ihme=get_ihme(),
                        epi=get_epi(),
                        crfs=get_crfs(),
                        scale_base_year=NULL,
                        scale_target_year=NULL
                        ){

  print("Computing paf")
  paf <- compute_hia_paf(conc_adm,
                         scenarios=scenarios,
                         calc_causes=calc_causes,
                         gemm=gemm, gbd=gbd, ihme=ihme)

  print("Computing epi")
  hia <- compute_hia_epi(region=regions,
                         species=species,
                         paf =paf,
                         conc_map=conc_map,
                         epi=epi,
                         crfs=crfs,
                         calc_causes=calc_causes)

  if(!any(is.null(c(scale_base_year, scale_target_year)))){
    print("Scaling")
    hia <- scale_hia_pop(hia, base_year=scale_base_year, target_year=scale_target_year)
  }

  return(hia)
}

compute_hia_paf <- function(conc_map, scenarios=names(conc_map),
                            calc_causes=get_calc_causes(),
                            gemm=get_gemm(), gbd=get_gbd(), ihme=get_ihme()){

  paf <- list()
  adult_ages <- get_adult_ages(ihme)

  for(scenario in scenarios) {
    message(paste('processing', scenario))

    conc_scenario <- conc_map[[scenario]] %>% subset(!is.null(.)) %>% lapply(data.frame) %>%
      bind_rows(.id='region_id') %>% dlply(.(region_id))

    foreach(region_id = names(conc_scenario)) %dopar% {
      paf_region <- list()
      conc <- conc_scenario[[region_id]][complete.cases(conc_scenario[[region_id]]),]

      for(cs_ms in calc_causes) {
        cs_ms %>% strsplit('_') %>% unlist -> cs.ms
        epi_country <- substr(region_id, 1, 3) %>% country.recode(c(use_as_proxy, merge_into))
        country_paf_perm(pm.base = conc[, 'conc_base_pm25'],
                         pm.perm = conc[, 'conc_coal_pm25'],
                         pop = conc[, 'pop'],
                         cy=epi_country,
                         cause=cs.ms[1],
                         measure=cs.ms[2],
                         adult_ages=adult_ages,
                         gemm=gemm,
                         gbd=gbd,
                         ihme=ihme,
                         .region="inc_China") -> paf_region[[cs_ms]]
      }
      tryCatch({
        paf_region %<>% ldply(.id='var') %>% mutate(region_id=region_id)
      }, error=function(e){
        # For instance if country iso3 not in ihme$ISO3
        warning("Failed for region ", region_id)
        paf_region <- NULL
      })
      return(paf_region)
    } -> paf[[scenario]]
  }
  paf %>% lapply(bind_rows)
}

get_nrt_conc <- function(region_ids, conc_name, nrt, conc_map,
                         units_multiplier=1, nrt_flag=NULL, nrt_flag_value=2) {
  conc_map[region_ids] %>%
    lapply(function(m) {
      if(!is.null(nrt_flag)) nrt = ifelse(m[,nrt_flag]==nrt_flag_value, 0, nrt)

      m[, conc_name] %>%
        multiply_by(units_multiplier) %>%
        subtract(nrt) %>%
        pmax(0) %>%
        weighted.mean(w=m[, 'pop'], na.rm=T)
    }) %>% unlist %>% unname
}


compute_hia_epi <- function(species, paf, conc_map, regions,
                            epi=get_epi(), crfs=get_crfs(),
                            calc_causes=get_calc_causes()){

  hia_polls <- species_to_hiapoll(species)
  scenarios <- names(conc_map)
  hia <- list()

  for(scenario in scenarios) {
    conc_scenario <- conc_map[[scenario]]
    #names(conc_adm) %<>% country.recode(merge_into)
    conc_scenario %<>% ldply(.id='region_id') %>% dlply(.(region_id))

    #calculate health impacts
    pop_domain <- conc_scenario %>% ldply(function(df) df %>% sel(-region_id) %>% apply(2, weighted.mean, w=df[,'pop']) %>% t %>%
                                            data.frame %>% mutate(pop=sum(df[,'pop'])), .id='region_id')

    pop_domain$epi_iso3 <- pop_domain$region_id %>% substr(1,3) %>% as.character %>% country_recode_iso3()

    epi_loc <- epi %>%
      sel(-pop, -country) %>%
      dplyr::rename(epi_iso3=ISO3) %>%
      filter(epi_iso3 %in% pop_domain$epi_iso3) %>%
      full_join(pop_domain %>% sel(region_id, epi_iso3, pop)) %>%
      sel(-epi_iso3)

    # Exclude unmatched countries
    na_iso3s <- epi_loc$region_id[is.na(epi_loc$estimate)]
    if(length(na_iso3s)>0){
      warning("Couldn't find epidemiological data for regions ", na_iso3s,". Excluding them.")
    }

    epi_loc %<>% filter(!is.na(estimate))

    hia_scenario <- epi_loc %>% sel(region_id, estimate, pop)

    for(i in which(crfs$Exposure %in% hia_polls)) {
      species_name <- hiapoll_to_species(crfs$Exposure[i])

      if(grepl('nrt', species_name)) {
        sourceconcs <- get_nrt_conc(hia$GID, species_name, 0, conc_adm = conc_adm)
      } else {
        species_name %>% paste0('conc_base_',.) -> base_name
        species_name %>% paste0('conc_coal_',.) -> perm_name
        nrt_flag <- NULL #ifelse(grepl('NCD\\.LRI_', crfs$Incidence[i]), 'grump', NULL)

        crfs$Counterfact[i] -> cfconc

        base_concs <- get_nrt_conc(region_ids=hia_scenario$region_id, conc_name=base_name, nrt=cfconc, conc_map=conc_scenario,
                                   units_multiplier=crfs$Units.multiplier[i], nrt_flag=nrt_flag)

        perm_concs <- get_nrt_conc(region_ids=hia_scenario$region_id, conc_name=perm_name, nrt=cfconc, conc_map=conc_scenario,
                                   units_multiplier=crfs$Units.multiplier[i], nrt_flag=nrt_flag)

        source_concs <- perm_concs - base_concs
      }

      match(hia_scenario$estimate, names(crfs)) -> RR.ind
      crfs[i, RR.ind] %>% unlist %>% unname -> RRs

      epi_loc[[crfs$Incidence[i]]] / 1e5 * epi_loc$pop * (1 - exp(-log(RRs)*source_concs / crfs$Conc.change[i])) ->
        hia_scenario[[crfs$effectname[i]]]
    }

    #calculate PM mortality
    paf[[scenario]] %>% gather(estimate, val, low, central, high) %>%
      mutate(var=paste0('paf_', var)) %>%
      spread(var, val) -> paf_wide
    epi_loc %>% left_join(paf_wide) -> paf_wide

    paf_wide %>% sel(region_id, estimate) -> pm_mort

    available_causes <- intersect(unique(paf[[scenario]]$var), names(epi_loc))

    for(cs in intersect(available_causes, calc_causes)){
      paf_wide[[cs]] / 1e5 * paf_wide[[paste0('paf_', cs)]] * paf_wide$pop -> pm_mort[[cs]]
    }

    names(pm_mort)[sapply(pm_mort, is.numeric)] %<>% paste0('_PM25')
    full_join(pm_mort, hia_scenario) -> hia_scenario

    # Summing deaths
    mort_col <- intersect(names(hia_scenario),
                          c("NCD.LRI_Deaths_PM25","NCD.LRI_Deaths_SO2","NCD.LRI_Deaths_NO2","COPD_Deaths_O3_8h","LRI.child_Deaths_PM25"))
    hia_scenario %<>%
      rowwise() %>%
      dplyr::mutate(Deaths_Total = rowSums(across(any_of(mort_col)))) %>%
      ungroup()

    hia[[scenario]] <- hia_scenario
    print(scenario)
  }

  hia %<>%
    ldply(.id='scenario') %>%
    left_join(regions %>% as.data.frame(row.names=NULL) %>% sel(region_id, region_name, iso3=country_id))

  return(hia)
}



#define a function to calculate the hazard ratio for a specific concentration, cause and age group
hr <- function(pm, .age='25+', .cause='NCD.LRI', .region='inc_China', gemm=get_gemm(), gbd=get_gbd()) {

  gbd.causes <- gbd$cause_short %>% unique

  if(.cause %in% gbd.causes) {
    gbd %>% dplyr::filter(cause_short==.cause) -> hr.all
    hr.all %>% sel(low, central, high) %>%
      apply(2, function(y) approx(x=hr.all$exposure, y, xout=pm)$y)
  } else {
    gemm %>% dplyr::filter(age == .age, cause == .cause, region==.region) %>%
      spread(param, value) -> p
    z = pmax(0, pm-2.4, na.rm=T)
    g = log(1+z/p$a) / (1 + exp((p$u-z)/p$p))
    se = c(-p$se, 0, p$se)
    (matrix(g) %*% (p$t + 2 * se)) %>% exp -> hr.out
    colnames(hr.out) <- c('low', 'central', 'high')
    return(hr.out)
  }
}


#total fossil fuel PAF for a permutation run
country_paf_perm <- function(pm.base,
                             pm.perm,
                             pop,
                             cy,
                             cause,
                             measure,
                             adult_ages=get_adult_ages(),
                             gemm=get_gemm(),
                             gbd=get_gbd(),
                             ihme=get_ihme(),
                             .region="inc_China",
                             .mode = 'change') { #change or attribution?

  age.specific=c('NCD.LRI', 'Stroke', 'IHD')

  if(cause %in% age.specific) {
    ages = adult_ages
    w = ihme %>%
      dplyr::filter(ISO3 == cy, cause_short==cause, measure_name == measure,
                    age %in% ages, estimate=='central')
  } else {
    w=data.frame(val=1)
    if(grepl('child', cause)) { ages='Under 5'
    } else { ages='25+' }
  }

  ages %>% sapply(function(.a) hr(pm.base, gbd=gbd, gemm=gemm, .cause=cause, .age=.a, .region=.region), simplify = 'array') -> rr.base

  if(.mode == 'change') {
    ages %>% sapply(function(.a) hr(pm.perm, gbd=gbd, gemm=gemm, .cause=cause, .age=.a, .region=.region), simplify = 'array') -> rr.perm
    paf.perm <- rr.perm / rr.base - 1

  } else { paf.perm = (1 - (1 / rr.base)) * (1 - pm.perm / pm.base) }

  if(length(dim(paf.perm))==2) {
    paf.perm %>% t %>%
      orderrows %>%
      apply(2, weighted.mean, w$val) #in case the hr function didn't return an array
  } else {
    tryCatch({
      paf.perm %>% apply(1:2, weighted.mean, w$val) %>%
        orderrows %>%
        apply(2, weighted.mean, w=pop)
    }, error=function(e){
      warning("Failed for region ", cy, cause, e)
      return(NULL)
    })
  }
}


country_paf <- function(pm, pop, cy, cs, ms, adult_ages=get_adult_ages(), .region="inc_China",
                        gemm=get_gemm(), gbd=get_gbd()) {


  if(grepl('child', cs)) {
    ages='Under 5'
    w=data.frame(val=1)
  } else {
    ages = adult_ages
    w = ihme %>% dplyr::filter(ISO3 == cy, cause_short==cs, measure_name == ms, age %in% ages, estimate=='central')
  }

  ages %>% sapply(function(.a) hr(pm, .cause=cs, .age=.a, .region=.region, gemm=gemm, gbd=gbd), simplify = 'array') -> rr
  paf <- 1 - 1 / rr

  if(length(dim(paf))==2) {
    paf %>% t %>% apply(2, weighted.mean, w$val) #in case the hr function didn't return an array
  } else {
    paf %>% apply(1:2, weighted.mean, w$val, na.rm=T) %>% apply(2, weighted.mean, w=pop, na.rm=T)
  }
}


scale_hia_pop <- function(hia, base_year=2015, target_year=2019){

  pop_proj <- get_pop_proj()

  #scale population from year of population data to target year of estimates
  pop_scaling <- pop_proj %>% filter(year %in% c(base_year, target_year), AgeGrp != 'Newborn', !is.na(iso3)) %>%
    sel(-AgeGrp) %>% group_by(iso3, year) %>% summarise_at('pop', sum)

  pop_scaling$year[pop_scaling$year==base_year] <- 'base'
  pop_scaling$year[pop_scaling$year==target_year] <- 'target'

  hia_scaled <- pop_scaling %>% spread(year, pop) %>% mutate(scaling=target/base) %>% sel(iso3, scaling) %>%
    left_join(hia, .) %>%
    (function(df) df %>% mutate_if(is.numeric, multiply_by, df$scaling)) %>% sel(-scaling)

  return(hia_scaled)
}

totalise_hia <- function(hia, .groups=c('region_id', 'region_name', 'iso3')){
  hia_adm <- hia %>%
    group_by(across(c(scenario, estimate, all_of(.groups)))) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    pivot_longer(is.numeric, names_to='Outcome', values_to='Number')

  hia_adm %>%
    group_by(across(c(all_of(.groups), scenario, estimate, Outcome))) %>%
     summarise_if(is.numeric, sum, na.rm=T) %>%
     spread(estimate, Number)
}


make_hia_table <- function(hia_total,
                           make_ci_fun=make_nothing,
                           res_cols = c('low', 'central', 'high'),
                           dict=get_dict()) {
  if('Cause' %notin% names(hia_total)) {
    hia_total %<>% separate(Outcome, c('Cause', 'Outcome', 'Pollutant'), '_')
    is.na(hia_total$Pollutant) -> ind
    hia_total$Pollutant[ind] <- hia_total$Outcome[ind]
    hia_total$Outcome[ind] <- hia_total$Cause[ind]
  }

  if('Outcome_long' %notin% names(hia_total)) {
    hia_table <- hia_total %>% add_long_names(dict=dict) %>%
      sel(scenario, Cause_long, Outcome_long, Pollutant, all_of(res_cols))
  }

  hia_table %>% filter(Outcome_long == 'deaths') -> deaths
  hia_table %>% filter(!grepl('deaths|life lost|prev|birthwe', Outcome_long), !is.na(Outcome_long)) -> morb

  deaths %<>% filter(Outcome_long == 'deaths') %>% filter(!(Cause_long == 'all' & Pollutant == 'PM25'))

  bind_rows(deaths %>% make_ci_fun %>% arrange(desc(Pollutant)),
            morb %>% make_ci_fun %>% arrange(Outcome_long)) %>%
    sel(Outcome_long, Cause_long, everything()) %>% mutate(Cause_long=recode(Cause_long, deaths='total'))
}

#a simple renaming function that accepts string variables as arguments; !!newname := !!oldname was causing grief
rename_str = function(df, oldname, newname) { names(df)[names(df)==oldname]<-newname; df }
add_long_names <- function(df, cols = c('Outcome', 'Cause'), dict=get_dict()) {
  for(cn in intersect(names(df), cols)) {
    out_cn = paste0(cn, '_long')
    df %<>%
      left_join(dict %>% rename_str('Code', cn)) %>%
      rename_str('Long.name', out_cn)
    df[[out_cn]] %<>% na.cover(df[[cn]])
  }

  if('Cause_long' %in% names(df)) df$Cause_long[grep('non-comm', df$Cause_long)] <- 'all'

  if('Pollutant' %in% names(df)) df$Pollutant %<>% recode(PM25="PM2.5")
  return(df)
}

add_total_deaths <- function(df, include_PM_causes = 'NCD\\.LRI|LRI\\.child') {
  if('Cause' %in% names(df)) {
    df %>% group_by(across(c(where(is.character),where(is.factor), -Cause))) %>%
      filter(Outcome %in% c('Deaths', 'YLLs'),
             (Pollutant != 'PM25' | grepl(include_PM_causes, Cause))) %>%
      summarise_at(vars(c(starts_with('number'), starts_with('cost.'))), sum, na.rm=T) %>%
      mutate(Cause='Total', Pollutant='Total') %>% bind_rows(df, .)
  } else df
}
