readWB <- function(inF, sheet='Data', skip=3, countries.only=T, ...) {
  read_xls(inF, sheet=sheet, skip=skip, ...) %>%
    gather(Year, Value, matches('[0-9]{4}')) %>% mutate_at('Year', as.numeric) %>%
    set_names(make.names(names(.))) %>%
    filter(!is.na(Value)) %>% arrange(-Year) %>% filter(!duplicated(Country.Code)) %>%
    rename(country=Country.Name, ISO3=Country.Code) %>%
    filter(!countries.only | ISO3 %in% WBcountries$ISO3)
}

if(!require(wbstats)) devtools::install_github("nset-ornl/wbstats")

wb_indicators() %>% sel(Indicator.Code=indicator_id, Indicator.Name=indicator) -> WBindicators
wb_countries() %>% sel(ISO3=iso3c, Region=region, IncomeGroup=income_level, country) %>%
  filter(IncomeGroup != 'Aggregates') -> WBcountries

readWB_online <- function(indicator, start_date = 2010, end_date = 2019,
                          valuename='Value', latest.year.only=T, ...) {
  wb_data(indicator, start_date=start_date, end_date=end_date, ...) -> d
  names(d)[names(d) == indicator] <- 'Value'
  d %<>%
    sel(ISO3=iso3c, country, Year=date, Value) %>%
    filter(!is.na(Value)) %>%
    mutate(Indicator.Code = indicator) %>%
    right_join(WBindicators, .)

  if(latest.year.only)
    d %<>% arrange(-Year) %>% distinct(ISO3, .keep_all = T)

  names(d)[names(d) == 'Value'] <- valuename
  if(nrow(d)==0) stop('no data!')
  return(d)
}

require(countrycode)
addiso <- function(df, ...) {
  df$ISO3 <- countrycode(df$country, origin = 'country.name', destination = 'iso3c', ...)
  df$ISO3[grepl('Kosovo', df$country)] <- 'XKX'
  df$ISO3[grepl('Aland$', df$country)] <- 'FIN'
  df
}

gather_ihme <- function(df) {
  df %>% rename(central=val, low=lower, high=upper) %>%
    gather(estimate, val, central, low, high) %>%
    mutate(measure_name = measure_name %>% gsub(' .*', '', .)) %>%
    rename(country=location_name)
}

ihme_getrate <- function(df) {
  df %>% left_join(pop.total %>% sel(location_id, pop=val)) %>%
    mutate(val = val / pop * 1e5) %>% sel(-pop) %>%
    ungroup %>% distinct
}

addlowhigh <-
  function(indata) ddply(indata, .(ISO3),
                         function(df) {
                           for(col in names(df))
                             df[[col]] %<>% na.fill(df[[col]][df$estimate=='central'])
                           return(df)
                         })

get.nrt.conc <- function(admIDs, poll, nrt, Units.multiplier=1, nrt.flag=NULL, nrt.flag.value=2,
                         conc_adm=get("conc0", envir = .GlobalEnv)) {
  conc_adm[admIDs] %>%
    lapply(function(m) {
      if(!is.null(nrt.flag)) nrt = ifelse(m[,nrt.flag]==nrt.flag.value, 0, nrt)
      m[, poll] %>% multiply_by(Units.multiplier) %>%
        subtract(nrt) %>% pmax(0) %>% weighted.mean(w=m[, 'pop'], na.rm=T)
    }) %>% unlist %>% unname
}




makeCI <- function(df, rescols = c('low', 'central', 'high')) { df %>% mutate_at(rescols, scales::comma, accuracy=1) %>%
    mutate(CI = paste0('(', low, ' - ', high, ')')) %>%
    sel(-low, -high) %>% gather(type, val, central, CI) %>% unite(var, scenario, type) %>% spread(var, val) }

maketable <- function(hiadata, makeCI_fun=makeCI, rescols = c('low', 'central', 'high')) {
  hiadata %<>% separate(Outcome, c('Cause', 'Outcome', 'Pollutant'), '_')
  is.na(hiadata$Pollutant) -> ind
  hiadata$Pollutant[ind] <- hiadata$Outcome[ind]
  hiadata$Outcome[ind] <- hiadata$Cause[ind]

  hiadata %>% left_join(dict %>% rename(Outcome_long = Long.name, Outcome=Code)) %>%
    left_join(dict %>% rename(Cause_long = Long.name, Cause=Code)) %>%
    sel(-Outcome, -Cause) %>%
    sel(scenario, Cause=Cause_long, Outcome = Outcome_long, Pollutant, all_of(rescols)) ->
    hiatable

  hiatable %>% filter(Outcome == 'deaths') -> deaths
  deaths$Cause[grep('non-comm', deaths$Cause)] <- 'all'
  hiatable %>% filter(!grepl('deaths|life lost|prev|birthwe', Outcome), !is.na(Outcome)) -> morb


  deaths %<>% filter(Outcome == 'deaths') %>% filter(!(Cause == 'all' & Pollutant == 'PM25'))


  bind_rows(deaths %>% makeCI_fun %>% arrange(desc(Pollutant)), morb %>% makeCI_fun %>% arrange(Outcome)) %>%
    sel(Outcome, Cause, everything()) %>% mutate(Cause=recode(Cause, deaths='total'))
}
