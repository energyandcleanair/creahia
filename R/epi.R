#IHME files required for update
#'IHME_GBD_2017_POP_2015_2017.CSV' -- population data
#'IHME_GBD_2017_ALL_LOCATIONS_HIERARCHIES.XLSX'
#'IHME-GBD_2017_DATA.csv' -- 'Non-communicable diseases', 'Lower respiratory infections', IHD, LC, Stroke, COPD, Diabetes; 'Number';, 'Under 5'; 'Deaths', 'YLLs', 'YLDs'
#'IHME-GBD_2017_DeathRates.csv' -- 'All Ages', 'All causes', 'Rate'
#'IHME-GBD_2017_Asthma.csv'



download_raw_epi <- function(){
  # This does not download data for now... but gives you the links to do so
  urls <- list(
    "IHME-GBD_2019_DATA-TotCV,TotResp.csv" = "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/850a0e4dd9a68940127c6b289b9caeff",
    "IHME-GBD_2017_DATA-TotCV,TotResp.csv" = "https://gbd2017.healthdata.org/gbd-results?params=gbd-api-2017-permalink/461e2102de5ca1d2e571dad42caf2aa7",

    "IHME-GBD_2017_DATA_FORIHME" = "https://gbd2017.healthdata.org/gbd-results?params=gbd-api-2017-permalink/32f325b3c6afb386f9fd930f96822001",
    "IHME-GBD_2019_DATA_FORIHME" = "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/f0db5c823f415245eeb58c9435f7bc2b"

  )
}


#' Convert a region id to the corresponding epi location_id
#' Could be national or subnational
#'
#'
#' @param region_id
#'
#' @return
#' @export
#'
#' @examples
get_epi_location_id <- function(region_id){

  locations_w_gadm <- get_locations_with_gadm(use_cache=T)

  # We try first with region_id==gadm_id
  # If not match, it uses iso3
  location_id <- tibble(
    region_id,
    iso3=substr(region_id, 1, 3) %>% country.recode(c(use_as_proxy, merge_into))) %>%
    left_join(
      locations_w_gadm %>% sel(location_id, gadm_id),
      by=c('region_id'='gadm_id')
    ) %>%
    left_join(
      locations_w_gadm %>% sel(location_id_iso3=location_id, gadm_id),
      by=c('iso3'='gadm_id')

    ) %>%
    mutate(location_id=coalesce(
      location_id, location_id_iso3
    )) %>%
    pull(location_id)

  if(length(location_id) != length(region_id)) stop(glue("Failed to get epi location_id for region: {region_id}"))

  return(location_id)
}


get_locations <- function(){
  raw <- read_xlsx(get_hia_path('IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.XLSX'), .name_repair = make.names) %>% #read_xlsx('2017 data/IHME_GBD_2017_ALL_LOCATIONS_HIERARCHIES.XLSX') %>%
    sel(location_id=matches('location.id', ignore.case = T),
        level=matches('^level$', ignore.case = T),
        location_name=matches('location.name', ignore.case = T),
        parent_id=matches('Parent.ID', ignore.case = T)
        )

  # Add country_id to each of these locations
  country_mapping <- list()
  for(level in seq(3,6)){
    raw_level <- raw %>% filter(level==!!level)
    if(level==3){
      country_mapping[[level]] <- raw_level %>%
        distinct(location_id, country_id=location_id, country_name=location_name)
    }
    else{
      country_mapping[[level]] <- raw_level %>%
        distinct(location_id, parent_id) %>%
        left_join(country_mapping[[level-1]],
                  by=c('parent_id'='location_id')) %>%
        sel(location_id, country_id, country_name)
    }
  }
  country_mapping %<>% bind_rows()


  raw %>%
    left_join(country_mapping) %>%
    sel(location_id, location_name, location_level=level, country_id, country_name) %>%
    mutate(iso3=countrycode::countrycode(country_name, "country.name", "iso3c"))
}


attach_gadm_to_locations <- function(locations=get_locations()){

  matching_files <- get_hia_paths(pattern = "*.csv", path="location_matching")
  matching_subnational <- lapply(matching_files, read_csv, col_types = cols()) %>%
    bind_rows() %>%
    sel(iso3, ihme_level, ihme_location_name, gadm_level, gadm_id, gadm_name)

  # Create a country matching
  matching_countries <- creahelpers::get_adm(level=0, res='coarse') %>%
    as.data.frame() %>%
    sel(iso3=GID_0,
           gadm_id=GID_0,
           gadm_name=COUNTRY) %>%
    mutate(gadm_level=0) %>%
    right_join(locations %>%
                filter(location_level==3) %>%
                distinct(iso3,
                         ihme_location_name=location_name,
                         ihme_level=location_level
                         ))

  matching <- bind_rows(matching_countries,
                        matching_subnational)

  locations %>%
    left_join(matching,
              by=c('iso3'='iso3',
                   'location_level'='ihme_level',
                   'location_name'='ihme_location_name'))

}

get_locations_with_gadm <- function(use_cache=T){
  dir.create("cache", showWarnings = F)
  filepath <- 'cache/locations_with_gadm.RDS'
  if(file.exists(filepath) & use_cache) return(readRDS(filepath))

  locations_w_gadm <- get_locations() %>%
    attach_gadm_to_locations()
  saveRDS(locations_w_gadm, filepath)
  return(locations_w_gadm)
}


get_pop <- function(level=c(3,4)){

  locations <- get_locations()

  pop <- read_csv('data/epi_update/IHME_GBD_2019_POP_2019_Y2020M10D15.CSV') %>%  #read.csv('2017 data/IHME_GBD_2017_POP_2015_2017.CSV') %>%
    dplyr::filter(year_id==2019, tolower(sex_name)=='both') %>%
    dplyr::rename(country = location_name)

  locations %>%
    right_join(pop) %>%
    filter(location_level %in% !!level) %>%
    distinct()
}


get_pop_total <- function(pop){
  pop %>% filter(age_group_name == 'All Ages')
}


get_wb_ind <- function(){
  read_csv('data/epi_update/World Bank indicators.csv')
}


get_asthma_new <- function(){
  read.csv('data/epi_update/new asthma cases.csv') %>%
    sel(country, starts_with('X2ppb')) %>%
    gather(estimate, val, -country) %>%
    mutate(estimate = estimate %>% gsub('.*_', '', .)) %>%
    mutate(var = 'new.asthma_NO2',
           val = val * 1e3)
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
  pop.total %>% filter(iso3 %in% c('SRB', 'MNE')) %>% distinct() -> sm
  sm$val <- sm$val / sum(sm$val)
  asthma.erv %>% filter(country == 'Serbia and Montenegro') %>% '['(c(1,1), T) -> sm.erv
  sm.erv %<>% mutate_if(is.numeric, multiply_by, sm$val)
  sm.erv$country <- sm$country
  sm.erv$ID <- NA

  asthma.erv$country[asthma.erv$ID==310] <- 'Democratic Republic of the Congo'
  asthma.erv$country[asthma.erv$ID==311] <- 'Congo - Brazzaville'
  asthma.erv %<>% bind_rows(sm.erv) %>% sel(-ID)

  # Scale to population
  asthma.erv.scaled <- asthma.erv %>%
    mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>%
    sel(country, iso3, starts_with('exac')) %>%
    left_join(pop.total %>% filter(location_level==3) %>% sel(iso3, pop=val))

  asthma.erv.scaled$exac.0to17 %<>% divide_by(asthma.erv.scaled$pop) %>% multiply_by(1e5)
  asthma.erv.scaled$exac.18to99 %<>% divide_by(asthma.erv.scaled$pop) %>% multiply_by(1e5)
  asthma.erv.scaled$exac.0to99 %<>% divide_by(asthma.erv.scaled$pop) %>% multiply_by(1e5)

  asthma.erv.scaled %>%
    sel(country, iso3, starts_with('exac')) %>%
    gather(var, val, -c(country, iso3))
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
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>% #read.csv('2017 data/IHME-GBD_2017_DATA.csv') %>
    add_location_details() %>%
    filter((cause_name %in% c('Non-communicable diseases',
                              'Lower respiratory infections')),
           metric_name == 'Number') %>%
    mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric) %>%
    filter(!is.na(age_low), age_low>=30) %>%
    gather_ihme %>%
    group_by(location_id, location_name, location_level, measure_name, estimate) %>%
    summarise_at('val', sum) %>%
    ihme_getrate %>%
    mutate(var=paste0('NCD.LRI_', measure_name))
}


get_death_crude <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DeathRates.csv') %>%
    add_location_details() %>%
    filter(age_name=='All Ages', cause_name=='All causes', metric_name=='Rate', measure_name=='Deaths') %>%
    gather_ihme %>%
    mutate(var='crude.death.rate')
}


get_death_child_lri <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('IHME-GBD_2017_DATA.csv') %>%
    add_location_details() %>%
    filter(cause_name == 'Lower respiratory infections',
           metric_name == 'Number',
           age_name == 'Under 5') %>%
    gather_ihme %>% ihme_getrate %>%
    filter(measure_name %in% c('Deaths', 'YLLs')) %>%
    mutate(var=paste0('LRI.child_', measure_name))
}


get_yld <- function(){
  read_csv('data/epi_update/IHME-GBD_2019_DATA-death_yll_yld.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DATA.csv') %>%
    add_location_details() %>%
    filter(!grepl('Non-comm', cause_name),
           #grepl('Diab|Strok|obstr', cause_name),
           metric_name == 'Number',
           grepl('YLD|Death|YLL', measure_name)) %>%
    mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric) %>%
    filter(!is.na(age_low), age_low>=25) %>%
    gather_ihme %>%
    group_by(location_id, location_name, location_level, cause_name, measure_name, estimate) %>%
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


get_death_totcp <- function(yld){

    read_csv('data/epi_update/IHME-GBD_2019_DATA-TotCV,TotResp.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_DATA.csv') %>%
    add_location_details() %>%
    filter(metric_name == 'Number') %>%
    mutate(age_low = age_name %>% gsub("[^0-9].*", "", .) %>% as.numeric) %>%
    filter(!is.na(age_low), age_low>=25) %>%
    gather_ihme %>%
    group_by(location_id, location_name, location_level, measure_name, cause_name, estimate) %>%
    summarise_at('val', sum) %>%
    ihme_getrate %>%
    mutate(var=recode(cause_name, 'Cardiovascular diseases'='TotCV', 'Chronic respiratory diseases'='TotResp')) %>%
    ddply(.(measure_name, location_id, location_name, location_level, estimate),
          function(df) {
            df.out <- df %>% filter(var %in% c('TotCV', 'TotResp'))
            df.out$var %<>% gsub('Tot', 'Oth', .)

            yld %>% filter(location_id == df$location_id[1],
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


get_asthma_prev <- function(){


  asthma.prev <- read_csv('data/epi_update/IHME-GBD_2019_DATA-asthma.csv') %>%  #read.csv('2017 data/IHME-GBD_2017_Asthma.csv') %>%
    add_location_details() %>%
    filter(measure_name %in% c('Incidence', 'Prevalence'),
           metric_name == 'Rate') %>%
    gather_ihme %>% filter(age_name != 'All Ages') %>%
    group_by(location_id, location_name, location_level, measure_name, estimate) %>%
    summarise_at('val', mean) %>%
    mutate(var=paste0('Asthma.',substr(measure_name,1,3),'.1to18nopopnorm')) %>%
    ungroup %>% distinct

  asthma.prev <- read_csv('data/epi_update/IHME-GBD_2019_DATA-asthma.csv') %>% #read.csv('IHME-GBD_2017_Asthma.csv') %>%
    add_location_details() %>%
    filter(measure_name %in% c('Incidence', 'Prevalence'),
           metric_name == 'Number') %>%
    gather_ihme %>% filter(age_name != 'All Ages') %>%
    group_by(location_id, location_name, location_level, measure_name, estimate) %>%
    summarise_at('val', sum) %>%
    mutate(var=paste0('Asthma.',substr(measure_name,1,4),'.1to18')) %>%
    ihme_getrate() %>%
    bind_rows(asthma.prev)

  read_csv('data/epi_update/IHME-GBD_2019_DATA-asthma.csv') %>% #read.csv('IHME-GBD_2017_Asthma_AllAges.csv') %>%
    add_location_details() %>%
    filter(age_name == 'All Ages', measure_name=='Prevalence', metric_name=='Rate') %>%
    gather_ihme %>%
    mutate(var=paste0('Asthma.',substr(measure_name,1,4),'.0to99')) %>%
    bind_rows(asthma.prev) ->
    asthma.prev

  return(asthma.prev)
}


add_country_to_epi_wide <- function(epi_wide,
                                    base_iso3,
                                    iso3,
                                    pop,
                                    name,
                                    income_group=NULL){

  epi_wide_new <- epi_wide %>%
    filter(iso3 == base_iso3,
           location_level == 3) %>%
    dplyr::mutate(iso3 = !!iso3,
           pop = pop,
           location_id=NA,
           location_name = name,
           country = name)


  if(!is.null(income_group)){
    epi_wide_new$income_group <- income_group
  }

  return(
    bind_rows(epi_wide, epi_wide_new)
  )
}


fill_and_add_missing_regions <- function(epi_wide){

  # add missing admin regions
  epi_wide <- add_country_to_epi_wide(
    epi_wide,
    base_iso3= 'CHN',
    iso3 = 'HKG',
    pop = 7.392e6,
    name = 'Hong Kong',
    income_group = "High income"
    )

  epi_wide <- add_country_to_epi_wide(
    epi_wide,
    base_iso3= 'CHN',
    iso3 = 'MAC',
    pop = 622567,
    name = 'Macau',
    income_group = "High income"
    )


  epi_wide <- add_country_to_epi_wide(
    epi_wide,
    base_iso3= 'ALB',
    iso3 = 'XKX',
    pop = 1831e3,
    name = 'Kosovo'
  )


  # Fill Taiwan
  idx_taiwan <- !is.na(epi_wide$iso3) & epi_wide$iso3=='TWN'
  idx_japan<- !is.na(epi_wide$iso3) & epi_wide$iso3=='JPN'  & epi_wide$location_level==3
  epi_wide$country[idx_taiwan] <- 'Taiwan'
  epi_wide$GDP.PPP.2011USD[idx_taiwan] <- epi_wide$GDP.PPP.2011USD[idx_japan] * 53023/44227
  epi_wide[idx_taiwan & epi_wide$estimate=='central',] %>% unlist %>% subset(is.na(.)) %>%
    names -> fillcols
  epi_wide[idx_taiwan, fillcols] <- epi_wide[idx_japan, fillcols]


  # epi_wide[!is.na(epi_wide$iso3) & epi_wide$iso3=='XKX' & epi_wide$estimate=='central',] %>% unlist %>% subset(is.na(.)) %>%
     # names -> fillcols
  # epi_wide[!is.na(epi_wide$iso3) & epi_wide$iso3=='XKX', fillcols] <- epi_wide[epi_wide$iso3=='ALB', fillcols]

  #scale Kosovo asthma cases by population
  asthma.cols <- grep('new.asthma|exac\\.', names(epi_wide), value=T)
  epi_wide[epi_wide$iso3=='XKX', asthma.cols] <-
    epi_wide[epi_wide$iso3=='ALB', asthma.cols] *
    epi_wide$pop[epi_wide$iso3=='XKX'][1] / epi_wide$pop[epi_wide$iso3=='ALB'][1]

  return(epi_wide)
}


#' For subnational levels, we fill with national data if subnational one is not available
#'
#' @param epi
#'
#' @return
#' @export
#'
#' @examples
fill_subnational <- function(epi){

  bind_rows(
    epi %>%
      filter(location_level != 4),

    epi %>%
      tidyr::complete(
        nesting(estimate,var),
        nesting(location_id, country, iso3, location_name, location_level, region, income_group)) %>%
      filter(location_level==4) %>%
      left_join(
        epi %>%
          filter(location_level==3) %>%
          sel(iso3, var, estimate, val_country=val)
      ) %>%
      mutate(val=coalesce(val, val_country)) %>%
      sel(-c(val_country))
  )
}



fill_low_high <- function(indata){
  ddply(indata, .(location_id),
        function(df) {
          for(col in names(df))
            df[[col]] <- df[[col]] %>%
              na.fill(df[[col]][df$estimate == 'central'])
          return(df)
        })
}


add_location_details <- function(x, locations=get_locations()){


  joiner <- locations %>%
    sel(location_id, location_name, location_level, iso3_filler=iso3)

  if(length(intersect(names(x), names(joiner)))==0){
    if("country" %in% names(x)){
      x$iso3 <- countrycode::countrycode(x$country, "country.name", "iso3c")
      joiner %<>% dplyr::mutate(iso3=iso3_filler)
    }else{
      stop("Missing location details")
    }
  }

  # Ensure country-level if not explicitly stated otherwise
  if(!any(c("location_id", "location_level") %in% names(x))){
    x$location_level <- 3
  }

  if(!"iso3" %in% names(x)) x$iso3 <- NA

  y <- x %>%
    left_join(joiner) %>%
    mutate(iso3=coalesce(iso3, iso3_filler)) %>%
    sel(-c(iso3_filler))

  if(nrow(x) != nrow(y)){
    stop("Adding location details changed dataset")
  }

  y$location_id %<>% as.integer()
  y$location_level %<>% as.integer()

  return(y)
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

  death.totcp <- get_death_totcp(yld=yld)
  asthma.prev <- get_asthma_prev()

  locations <- get_locations()

  pop.total %<>% add_location_details(locations=locations)
  asthma.new %<>% add_location_details(locations=locations)
  asthma.erv %<>% add_location_details(locations=locations)
  asthma.prev %<>% add_location_details(locations=locations)
  ptb %<>% add_location_details(locations=locations)
  lbw %<>% add_location_details(locations=locations)
  birth.rate %<>% add_location_details(locations=locations)
  labor.partic %<>% add_location_details(locations=locations)
  labor.age.share %<>% add_location_details(locations=locations)
  GDP %<>% add_location_details(locations=locations)

  mort.morb <- bind_rows(death.all.cause %>% add_location_details(locations=locations),
                         deaths.crude %>% add_location_details(locations=locations),
                         death.child.lri %>% add_location_details(locations=locations),
                         yld %>% add_location_details(locations=locations),
                         death.totcp %>% add_location_details(locations=locations))


  epi <- bind_rows(
      pop.total %>% sel(location_id, location_name, location_level, iso3, val) %>% mutate(var='pop'),
      mort.morb %>% sel(location_id, location_name, location_level, iso3, var, estimate, val),
      birth.rate %>% sel(location_id, location_name, location_level, iso3, val=Value) %>% mutate(var='birth.rate'),
      ptb %>% sel(location_id, location_name, location_level, iso3, val=PTB.rate) %>% mutate(var='PTB.rate'),
      lbw %>% sel(location_id, location_name, location_level, iso3, val=Value) %>% mutate(var='LBW.rate'),
      asthma.prev %>% sel(location_id, location_name, location_level, iso3, var, estimate, val),
      asthma.new %>% sel(location_id, location_name, location_level, iso3, var, estimate, val),
      asthma.erv %>% sel(location_id, location_name, location_level, iso3, var, val),
      labor.partic %>% sel(location_id, location_name, location_level, iso3, val=Value) %>% mutate(var='labor.partic'),
      labor.age.share %>% sel(location_id, location_name, location_level, iso3, val=Value) %>% mutate(var='working.age.share'),
      GDP %>% sel(location_id, location_name, location_level, iso3, val=Value) %>% mutate(var='GDP.PPP.2011USD')) %>%
    filter(!is.na(location_id), !is.na(val))

  epi$estimate %<>% na.fill('central')
  wbstats::wb_countries() %>% sel(iso3=iso3c, region=region, income_group=income_level, country) %>%
    filter(income_group != 'Aggregates') -> wb_countries

  epi %<>% left_join(wb_countries %>% sel(iso3, country, region, income_group))
  epi %<>% fill_subnational()
  epi %>% distinct %>% pivot_wider(names_from = var, values_from = val) -> epi_wide

  epi_wide <- fill_and_add_missing_regions(epi_wide)

  #add new variables for health impact calculations
  epi_wide$Absences.per <- epi_wide$working.age.share/100 *
    epi_wide$labor.partic/100 * 9.4 * 1e5
  epi_wide$PTB.per <- epi_wide$PTB.rate/100 * epi_wide$birth.rate/1e3 * 1e5
  epi_wide$LBW.per <- epi_wide$LBW.rate/100 * epi_wide$birth.rate/1e3 * 1e5
  epi_wide$Asthma.Prev.0to17_no2 <- (epi_wide$Asthma.Prev.1to18/epi_wide$Asthma.Inci.1to18) *
    epi_wide$new.asthma_NO2


  list.files(path='inst/extdata/', pattern='CRF', full.names = T) %>%
    lapply(read_csv)  %>%
    bind_rows %>%
    mutate(Incidence = crf_recode_incidence(Incidence, Exposure)) %>%
    distinct(Incidence, Exposure)-> CRFs

  c('pop', CRFs$Incidence, names(epi_wide) %>% grep('Deaths|YLLs|YLDs|birth|labor', ., value=T)) -> inci.out

  # CRFs$Incidence[CRFs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', CRFs$Incidence)] %<>%
  #   gsub('NCD\\.LRI', 'AllCause', .)


  epi_wide %>% set_names(names(.) %>%
                           gsub('\\.per|_base', '', .) %>%
                           gsub('_0_17', '.0to17', .) %>%
                           gsub('_1_18|_1to18', '.1to18', .) %>%
                           gsub('_18_99', '.18to99', .)) %>%
    fill_low_high() -> epi_hia


  bind_cols(
    select_at(epi_hia, c('location_id', 'location_level')),
    select_if(epi_hia, is.character),
    select_at(epi_hia, intersect(inci.out, names(epi_hia)))) %>%
    filter(!is.na(pop)) %>% arrange(country) ->
    epi_hia

  epi_hia %>% write_csv('inst/extdata/epi_for_hia_gbd2019.csv')

}


generate_ihme <- function(version='gbd2019'){

  raw_filepath <- recode(version,
                    gbd2017='data/epi_update/IHME-GBD_2017_DATA-ihme.csv',
                    gbd2019='data/epi_update/IHME-GBD_2019_DATA-ihme.csv')

  # read IHME mortality and morbidity data to enable country calculations
  ihme <- read_csv(raw_filepath) %>%
    add_location_details() %>%
    filter(location_level %in% c(3,4)) %>%
    dplyr::filter(metric_name == 'Number') %>%
    dplyr::filter(!grepl('95+', age_name)) %>%
    gather_ihme

  get_age_low <- function(age_name){
    age_low = stringr::str_extract(ihme$age_name, "^\\d+") %>% as.numeric
    age_low[age_name == 'Under 5'] <- 0 # In 2017 version
    age_low[age_name == '<5 years'] <- 0 # In 2019 version
    age_low[age_name == 'All Ages'] <- -1
    age_low
  }

  homogenise_age_name <- function(age_name){
    age_name %>%
      gsub(' years', '', .) %>%
      gsub(' to ', '-', .) %>%
      gsub(' plus', '+', .)
  }

  ihme$age_low <- get_age_low(ihme$age_name)
  ihme$age <- homogenise_age_name(ihme$age_name)

  if(ihme %>% group_by(age_low) %>% dplyr::summarise(count=n_distinct(age_name)) %>% pull(count) %>% max() > 1){
    stop("Two many age categories")
  }

  ihme <- ihme %>%
    dplyr::filter(age_low >= 25) %>%
    group_by_at(vars(-val, -starts_with('age'))) %>%
    summarise_at('val', sum) %>%
    mutate(age = '25+') %>%
    bind_rows(ihme) %>%
    ungroup

  # ihme2 <- ihme %>% add_location_details()

  ihme <- ihme %>%
    mutate(cause_short = case_when(grepl('Diab', cause_name) ~ 'Diabetes',
                                   grepl('Stroke', cause_name) ~ 'Stroke',
                                   grepl('Lower resp', cause_name) ~ 'LRI',
                                   grepl('Non-comm', cause_name) ~ 'NCD',
                                   grepl('Isch', cause_name) ~ 'IHD',
                                   grepl('obstr', cause_name) ~ 'COPD',
                                   grepl('lung canc', cause_name) ~ 'LC',
                                   T ~ NA))

  ihme <- ihme %>%
    dplyr::filter(grepl('Lower resp|Non-comm', cause_name)) %>%
    group_by_at(vars(-val, -starts_with('cause'))) %>%
    summarise_at('val', sum) %>%
    mutate(cause_name = 'NCD+LRI', cause_short = 'NCD.LRI') %>%
    bind_rows(ihme) %>%
    ungroup


  adult.ages <- ihme$age[ihme$age_low >= 25] %>% subset(!is.na(.)) %>%
    unique


  ihme <- ihme %>%
    dplyr::filter(iso3 == 'ALB', location_level==3) %>%
    mutate(iso3 = 'XKX', location_name='Kosovo', location_id=NA) %>%
    bind_rows(ihme)

  # Generate a lighter version
  ihme %>%
    sel(location_id, location_name, iso3, location_level, age, measure_name, age_low, age_name, cause_short, cause_name, sex_name, metric_name, estimate, val) %>%
    filter(estimate == 'central') %>%
    write_csv(glue('inst/extdata/ihme_{version}.csv')) %>%
    saveRDS(glue('inst/extdata/ihme_{version}.RDS'))
}

