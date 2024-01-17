#IHME files required for update
#'IHME_GBD_2017_POP_2015_2017.CSV' -- population data
#'IHME_GBD_2017_ALL_LOCATIONS_HIERARCHIES.XLSX'
#'IHME-GBD_2017_DATA.csv' -- 'Non-communicable diseases', 'Lower respiratory infections', IHD, LC, Stroke, COPD, Diabetes; 'Number';, 'Under 5'; 'Deaths', 'YLLs', 'YLDs'
#'IHME-GBD_2017_DeathRates.csv' -- 'All Ages', 'All causes', 'Rate'
#'IHME-GBD_2017_Asthma.csv'



download_raw_epi <- function(){

  urls <- list(
    "IHME-GBD_2019_DATA-TotCV,TotResp.csv" = "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/850a0e4dd9a68940127c6b289b9caeff",



    "IHME-GBD_2017_DATA-TotCV,TotResp.csv" = "https://gbd2017.healthdata.org/gbd-results?params=gbd-api-2017-permalink/461e2102de5ca1d2e571dad42caf2aa7",

  )
}


get_locations <- function(){
  read_xlsx('data/epi_update/IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.XLSX', .name_repair = make.names) %>% #read_xlsx('2017 data/IHME_GBD_2017_ALL_LOCATIONS_HIERARCHIES.XLSX') %>%
    sel(location_id=matches('location.id', ignore.case = T),
        level=matches('^level$', ignore.case = T))
}


get_pop <- function(level=3){

  locations <- get_locations()

  pop <- read_csv('data/epi_update/IHME_GBD_2019_POP_2019_Y2020M10D15.CSV') %>%  #read.csv('2017 data/IHME_GBD_2017_POP_2015_2017.CSV') %>%
    dplyr::filter(year_id==2019, tolower(sex_name)=='both') %>%
    dplyr::rename(country = location_name)

  locations %>%
    right_join(pop) %>%
    filter(level %in% !!level) %>%
    distinct()
}


get_pop_total <- function(pop){
  pop %>% filter(age_group_name == 'All Ages')
}


get_wb_ind <- function(){
  read_csv('data/epi_update/World Bank indicators.csv')
}


get_asthma_new <- function(){
  read.csv('data/epi_update/new asthma cases.csv')
}


get_asthma_erv <- function(pop.total=NULL){
  asthma.erv <- read_xlsx('data/epi_update/Anenberg EHP 2018 results.xlsx', sheet='pm totconc') %>%
    filter(!is.na(ID)) %>%
    dplyr::rename(country=Country,
           exac.0to17=exac_0_17_base,
           exac.18to99=exac_18_99_base,
           exac.0to99=exac_0_99_base)


  #add Serbia and Montenegro data to asthma ERV
  pop.total <- creahelpers::default_if_null(pop.total, get_pop_total())
  pop.total %<>% addiso()
  pop.total %>% filter(ISO3 %in% c('SRB', 'MNE')) %>% sel(-level) %>% distinct() -> sm
  sm$val <- sm$val / sum(sm$val)
  asthma.erv %>% filter(country == 'Serbia and Montenegro') %>% '['(c(1,1), T) -> sm.erv
  sm.erv %<>% mutate_if(is.numeric, multiply_by, sm$val)
  sm.erv$country <- sm$country
  sm.erv$ID <- NA
  asthma.erv$country[asthma.erv$ID==310] <- 'Democratic Republic of the Congo'
  asthma.erv$country[asthma.erv$ID==311] <- 'Congo - Brazzaville'
  asthma.erv %<>% bind_rows(sm.erv) %>% sel(-ID)

  return(asthma.erv)
}


get_ptb <- function(){
  PTB <- readxl::read_xlsx('data/epi_update/2014 National preterm birth rates.xlsx', skip=3, col_names = F, n_max=300)
  nms <- readxl::read_xlsx('data/epi_update/2014 National preterm birth rates.xlsx', skip=1, col_names = F, n_max=2)
  nms[2, ] <- ifelse(is.na(nms[2, ]), nms[1, ], nms[2, ])
  names(PTB) <- unlist(nms[2, ]) %>% make.names(unique=T)
  PTB %<>% filter(!is.na(PTB.rate)) %>% dplyr::rename(country=Country)
  return(PTB)
}


get_death_all_cause <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DATA.csv') %>%
    filter((cause_name %in% c('Non-communicable diseases',
                              'Lower respiratory infections')),
           metric_name == 'Number') %>%
    mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric) %>%
    filter(!is.na(age_low), age_low>=30) %>%
    gather_ihme %>%
    group_by(measure_name, country, location_id, estimate) %>%
    summarise_at('val', sum) %>%
    ihme_getrate %>%
    mutate(var=paste0('NCD.LRI_', measure_name))
}


get_death_crude <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DeathRates.csv') %>%
    filter(age_name=='All Ages', cause_name=='All causes', metric_name=='Rate', measure_name=='Deaths') %>%
    gather_ihme %>% mutate(var='crude.death.rate')
}


get_death_child_lri <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('IHME-GBD_2017_DATA.csv') %>%
    filter(cause_name == 'Lower respiratory infections',
           metric_name == 'Number',
           age_name == 'Under 5') %>%
    gather_ihme %>% ihme_getrate %>%
    filter(measure_name %in% c('Deaths', 'YLLs')) %>%
    mutate(var=paste0('LRI.child_', measure_name))
}


get_yld <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DATA.csv') %>%
    filter(!grepl('Non-comm', cause_name),
           #grepl('Diab|Strok|obstr', cause_name),
           metric_name == 'Number',
           grepl('YLD|Death|YLL', measure_name)) %>%
    mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric) %>%
    filter(!is.na(age_low), age_low>=25) %>%
    gather_ihme %>%
    group_by(cause_name, measure_name, country, location_id, estimate) %>%
    summarise_at('val', sum) %>%
    ihme_getrate %>%
    mutate(cause_name = recode(cause_name,
                               'Ischemic heart disease' = 'IHD',
                               'Lower respiratory infections' = 'LRI',
                               'Tracheal, bronchus, and lung cancer' = 'LC',
                               'Chronic obstructive pulmonary disease' = 'COPD',
                               'Diabetes mellitus type 2' = 'Diabetes',
                               'All causes' = 'AllCause')) %>%
    mutate(var=paste0(cause_name, '_', measure_name))
}

get_death_totcp <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-TotCV,TotResp.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DATA.csv') %>%
    filter(metric_name == 'Number') %>%
    mutate(age_low = age_name %>% gsub("[^0-9].*", "", .) %>% as.numeric) %>%
    filter(!is.na(age_low), age_low>=25) %>%
    gather_ihme %>%
    group_by(measure_name, country, location_id, cause_name, estimate) %>%
    summarise_at('val', sum) %>%
    ihme_getrate %>%
    mutate(var=recode(cause_name, 'Cardiovascular diseases'='TotCV', 'Chronic respiratory diseases'='TotResp')) %>%
    ddply(.(measure_name, country, estimate),
          function(df) {
            df.out <- df %>% filter(var %in% c('TotCV', 'TotResp'))
            df.out$var %<>% gsub('Tot', 'Oth', .)

            yld %>% filter(country == df$country[1],
                           measure_name == df$measure_name[1],
                           estimate == df$estimate[1]) -> bycause
            bycause %>% filter(grepl('IHD|Stroke', var)) -> cv
            bycause %>% filter(grepl('COPD', var)) -> copd

            if(!(nrow(cv)==2 & nrow(copd)==1)) stop('you done messed up!')
            df.out$val[df.out$var=='OthCV'] %<>% subtract(sum(cv$val))
            df.out$val[df.out$var=='OthResp'] %<>% subtract(copd$val)
            bind_rows(df, df.out)
          }, .progress='text') %>% unite(var, var, measure_name)
}


get_asthama_prev <- function(pop.total=NULL){


  asthma.prev <- read_csv('data/epi_update/IHME-GBD_2019_DATA-asthma.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_Asthma.csv') %>%
    filter(measure_name %in% c('Incidence', 'Prevalence'),
           metric_name == 'Rate') %>%
    gather_ihme %>% filter(age_name != 'All Ages') %>%
    group_by(measure_name, country, location_id, estimate) %>%
    summarise_at('val', mean) %>%
    mutate(var=paste0('Asthma.',substr(measure_name,1,3),'.1to18nopopnorm')) %>%
    ungroup %>% distinct

  asthma.prev <- read_csv('data/epi_update/IHME-GBD_2019_DATA-asthma.csv') %>% #read.csv('IHME-GBD_2017_Asthma.csv') %>%
    filter(measure_name %in% c('Incidence', 'Prevalence'),
           metric_name == 'Number') %>%
    gather_ihme %>% filter(age_name != 'All Ages') %>%
    group_by(measure_name, country, location_id, estimate) %>%
    summarise_at('val', sum) %>%
    mutate(var=paste0('Asthma.',substr(measure_name,1,4),'.1to18')) %>%
    ihme_getrate() %>%
    bind_rows(asthma.prev)


  read_csv('data/epi_update/IHME-GBD_2019_DATA-asthma.csv') %>% #read.csv('IHME-GBD_2017_Asthma_AllAges.csv') %>%
    filter(age_name == 'All Ages', measure_name=='Prevalence', metric_name=='Rate') %>%
    gather_ihme %>% mutate(var=paste0('Asthma.',substr(measure_name,1,4),'.0to99')) %>%
    bind_rows(asthma.prev) ->
    asthma.prev



  return(asthma.prev)
}


fill_and_add_missing_regions <- function(epi_wide){


  # add missing admin regions
  epi_wide <- epi_wide %>% filter(ISO3 == 'CHN') %>%
    mutate(ISO3 = 'HKG', pop = 7.392e6, country = 'Hong Kong',
           IncomeGroup = "High income") %>%
    bind_rows(epi_wide)

  epi_wide <- epi_wide %>% filter(ISO3 == 'CHN') %>%
    mutate(ISO3 = 'MAC', pop = 622567, country = 'Macau',
           IncomeGroup = "High income") %>%
    bind_rows(epi_wide)

  #fill in missing data for Taiwan, Kosovo
  epi_wide$pop[epi_wide$ISO3=='XKX'] <- 1831e3
  epi_wide %<>% filter(country == 'Albania', estimate %in% c('high', 'low')) %>%
    mutate(ISO3='XKX', country='Kosovo') %>% bind_rows(epi_wide, .)

  epi_wide$country[epi_wide$ISO3=='TWN'] <- 'Taiwan'
  epi_wide$GDP.PPP.2011USD[epi_wide$ISO3=='TWN'] <- epi_wide$GDP.PPP.2011USD[epi_wide$ISO3=='JPN'] * 53023/44227

  epi_wide[epi_wide$ISO3=='TWN' & epi_wide$estimate=='central',] %>% unlist %>% subset(is.na(.)) %>%
    names -> fillcols
  epi_wide[epi_wide$ISO3=='TWN', fillcols] <- epi_wide[epi_wide$ISO3=='JPN', fillcols]


  epi_wide[epi_wide$ISO3=='XKX' & epi_wide$estimate=='central',] %>% unlist %>% subset(is.na(.)) %>%
    names -> fillcols
  epi_wide[epi_wide$ISO3=='XKX', fillcols] <- epi_wide[epi_wide$ISO3=='ALB', fillcols]

  #scale Kosovo asthma cases by population
  asthma.cols <- grep('new.asthma|exac\\.', names(epi_wide), value=T)
  epi_wide[epi_wide$ISO3=='XKX', asthma.cols] <-
    epi_wide[epi_wide$ISO3=='ALB', asthma.cols] *
    epi_wide$pop[epi_wide$ISO3=='XKX'][1] / epi_wide$pop[epi_wide$ISO3=='ALB'][1]

  return(epi_wide)
}


generate_epi <- function(){

  wb_ind <- get_wb_ind()
  pop <- get_pop()
  pop.total <- get_pop_total(pop)
  asthma.new <- get_asthma_new()
  asthma.erv <- get_asthma_erv(pop.total=pop.total)
  ptb <- get_ptb()


  lbw <- readWB_online('SH.STA.BRTW.ZS')
  birth.rate <- readWB_online('SP.DYN.CBRT.IN')
  labor.partic <- readWB_online('SL.TLF.ACTI.ZS')
  labor.age.share <- readWB_online('SP.POP.1564.TO.ZS')
  GDP <- readWB_online('NY.GDP.PCAP.PP.KD')


  death.all.cause <- get_death_all_cause()
  deaths.crude <- get_death_crude()
  death.child.lri <- get_death_child_lri()
  yld <- get_yld()

  death.totcp <- get_death_totcp()
  asthma.prev <- get_asthama_prev()


  pop.total %<>% addiso
  asthma.new %<>% addiso
  asthma.erv %<>% addiso
  asthma.prev %<>% addiso
  ptb %<>% addiso
  lbw %<>% addiso
  birth.rate %<>% addiso
  labor.partic %<>% addiso
  labor.age.share %<>% addiso
  GDP %<>% addiso

  mort.morb <- bind_rows(death.all.cause, deaths.crude, death.child.lri, yld, death.totcp) %>% addiso

  epi <- bind_rows(pop.total %>% sel(ISO3, val) %>% mutate(var='pop'),
            mort.morb %>% sel(ISO3, var, estimate, val),
            birth.rate %>% sel(ISO3, val=Value) %>% mutate(var='birth.rate'),
            ptb %>% sel(ISO3, val=PTB.rate) %>% mutate(var='PTB.rate'),
            lbw %>% sel(ISO3, val=Value) %>% mutate(var='LBW.rate'),
            asthma.prev %>% sel(ISO3, var, estimate, val),
            asthma.new %>% sel(ISO3, starts_with('X2ppb')) %>%
              gather(estimate, val, -ISO3) %>%
              mutate(estimate = estimate %>% gsub('.*_', '', .)) %>%
              mutate(var = 'new.asthma_NO2'),
            asthma.erv %>% sel(ISO3, starts_with('exac')) %>%
              gather(var, val, -ISO3),
            labor.partic %>% sel(ISO3, val=Value) %>% mutate(var='labor.partic'),
            labor.age.share %>% sel(ISO3, val=Value) %>% mutate(var='working.age.share'),
            GDP %>% sel(ISO3, val=Value) %>% mutate(var='GDP.PPP.2011USD')) %>%
    filter(!is.na(ISO3), !is.na(val))

  epi$estimate %<>% na.fill('central')

  #add region and income group data
  wbstats::wb_countries() %>% sel(ISO3=iso3c, Region=region, IncomeGroup=income_level, country) %>%
    filter(IncomeGroup != 'Aggregates') -> WBcountries

  epi %<>% left_join(WBcountries %>% sel(ISO3, country, Region, IncomeGroup))
  epi %>% distinct %>% pivot_wider(names_from = var, values_from = val) -> epi_wide


  epi_wide$exac.0to17 %<>% divide_by(epi_wide$pop) %>% multiply_by(1e5)
  epi_wide$exac.18to99 %<>% divide_by(epi_wide$pop) %>% multiply_by(1e5)

  epi_wide <- fill_and_add_missing_regions(epi_wide)

  #add variables for health impact calculations
  epi_wide$Absences.per <- epi_wide$working.age.share/100 *
    epi_wide$labor.partic/100 * 9.4 * 1e5
  epi_wide$PTB.per <- epi_wide$PTB.rate/100 * epi_wide$birth.rate/1e3 * 1e5
  epi_wide$LBW.per <- epi_wide$LBW.rate/100 * epi_wide$birth.rate/1e3 * 1e5
  epi_wide$new.asthma_NO2 %<>% multiply_by(1000)
  epi_wide$Asthma.Prev.0to17_no2 <- (epi_wide$Asthma.Prev.1to18/epi_wide$Asthma.Inci.1to18) *
    epi_wide$new.asthma_NO2


  #export EPI data for HIA use

  #prepare vector of column names to output
  # read_csv('data/epi_update/CRFs.csv') -> CRFs
  # read_csv('inst/extdata/CRFs.csv')
  read_csv('inst/extdata/CRFs_C40.csv') %>%
    mutate(Incidence = crf_recode_incidence(Incidence, Exposure))-> CRFs



  c('pop', CRFs$Incidence, names(epi_wide) %>% grep('Deaths|YLLs|YLDs|birth|labor', ., value=T)) -> inci.out
  CRFs$Incidence[CRFs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', CRFs$Incidence)] %<>%
    gsub('NCD\\.LRI', 'AllCause', .)


  epi_wide %>% set_names(names(.) %>% gsub('\\.per|_base', '', .) %>%
                           gsub('_0_17', '.0to17', .) %>%
                           gsub('_1_18|_1to18', '.1to18', .) %>%
                           gsub('_18_99', '.18to99', .)) %>%
    addlowhigh() -> epi_hia


  bind_cols(select_if(epi_hia, is.character),
            select_at(epi_hia, inci.out)) %>%
    filter(!is.na(pop)) %>% arrange(country) ->
    epi_hia

  epi_hia %>% write_csv('inst/extdata/epi_for_hia_gbd2019.csv')

}


