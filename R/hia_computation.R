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


compute_hia_paf <- function(conc_map, calc_causes, scenarios=names(conc_map),
                            gemm=get_gemm(), gbd=get_gbd(), ihme=get_ihme()){

  paf <- list()

  for(scenario in scenarios) {

    conc_scenario <- conc_map[[scenario]]
    #names(conc_map) %<>% country.recode(merge_into)
    conc_scenario %<>% ldply(.id='region_id') %>% dlply(.(region_id))

    paf_scenario = list()

    for(region_id in names(conc_scenario)) {

      paf_scenario[[region_id]] <- list()

      for(cs_ms in calc_causes) {
        cs_ms %>% strsplit('_') %>% unlist -> cs.ms
        epi_country <- substr(region_id, 1, 3) # Add a country_id instead
        country_paf_perm(pm.base = conc_scenario[[region_id]][, 'conc_base_pm25'],
                         pm.perm = conc_scenario[[region_id]][, 'conc_coal_pm25'],
                         pop = conc_scenario[[region_id]][, 'pop'],
                         cy=epi_country,
                         cause=cs.ms[1],
                         measure=cs.ms[2],
                         adult_ages=adult_ages,
                         gemm=gemm,
                         gbd=gbd,
                         ihme=ihme,
                         .region="inc_China") -> paf_scenario[[region_id]][[cs_ms]]
      }
      paf_scenario[[region_id]] %<>% ldply(.id='var')
    }

    paf_scenario %<>% ldply(.id='region_id')
    paf[[scenario]] <- paf_scenario
  }
  return(paf)
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


compute_hia_epi <- function(species, paf, conc_map, epi=get_epi()){

  hia_polls <- species_to_hiapoll(species)
  scenarios <- names(conc_map)

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

    hia_scenario <- epi_loc %>% sel(region_id, estimate, pop)

    for(i in which(crfs$Exposure %in% hia_polls)) {
      species_name <- hiapoll_to_species(crfs$Exposure[i])

      if(grepl('nrt', concname)) {
        sourceconcs <- get_nrt_conc(hia$GID, species_name, 0, conc_adm = conc_adm)
      } else {
        species_name %>% paste0('conc_base_',.) -> base_name
        species_name %>% paste0('conc_coal_',.) -> perm_name
        nrt_flag <- NULL #ifelse(grepl('NCD\\.LRI_', crfs$Incidence[i]), 'grump', NULL)

        crfs$Counterfact[i] -> cfconc

        base_concs <- get_nrt_conc(region_ids=hia$region_id, conc_name=base_name, nrt=cfconc, conc_map=conc_scenario,
                                   units_multiplier=crfs$Units.multiplier[i], nrt_flag=nrt_flag)

        perm_concs <- get_nrt_conc(region_ids=hia$region_id, conc_name=perm_name, nrt=cfconc, conc_map=conc_scenario,
                                   units_multiplier=crfs$Units.multiplier[i], nrt_flag=nrt_flag)

        source_concs <- perm_concs - base_concs
      }

      match(hia$estimate, names(crfs)) -> RR.ind
      crfs[i, RR.ind] %>% unlist %>% unname -> RRs

      epi_loc[[crfs$Incidence[i]]] / 1e5 * epi_loc$pop * (1 - exp(-log(RRs)*source_concs / crfs$Conc.change[i])) ->
        hia_scenario[[crfs$effectname[i]]]
    }

    #calculate PM mortality
    paf[[scenario]] %>% gather(estimate, val, low, central, high) %>%
      mutate(var=paste0('paf_', var)) %>%
      spread(var, val) -> paf_wide
    epi_loc %>% left_join(paf_wide) -> paf_wide

    paf_wide %>% sel(GID, estimate) -> pm_mort

    for(cs in calc_causes)
      paf_wide[[cs]] / 1e5 * paf_wide[[paste0('paf_', cs)]] * paf_wide$pop -> pm_mort[[cs]]

    names(pm_mort)[sapply(pm_mort, is.numeric)] %<>% paste0('_PM25')
    full_join(pm_mort, hia_scenario) -> hia[[s]]
    print(s)
  }

  hia %<>% ldply(.id='scenario') %>% mutate(iso3 = region_id %>% substr(1, 3))
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

  ages %>% sapply(function(.a) hr(pm.base, .cause=cause, .age=.a, .region=.region), simplify = 'array') -> rr.base

  if(.mode == 'change') {
    ages %>% sapply(function(.a) hr(pm.perm, .cause=cause, .age=.a, .region=.region), simplify = 'array') -> rr.perm
    paf.perm <- rr.perm / rr.base - 1

  } else { paf.perm = (1 - (1 / rr.base)) * (1 - pm.perm / pm.base) }

  if(length(dim(paf.perm))==2) {
    paf.perm %>% t %>%
      orderrows %>%
      apply(2, weighted.mean, w$val) #in case the hr function didn't return an array
  } else {
    paf.perm %>% apply(1:2, weighted.mean, w$val) %>%
      orderrows %>% apply(2, weighted.mean, w=pop)
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
