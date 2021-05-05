#' Get path of external data bundled with creahia package
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
get_hia_path <- function(filename){
  # We bundle HIA data with creahia package, in inst/extdata
  # Read here for more details:
  # https://r-pkgs.org/data.html#data-extdata
  f <- system.file("extdata", filename, package = "creahia")
  if(f=="") stop("Couldn't find file ",filename," in HIA folder")
  return(f)
}

#fill in default values for places missing data
fillcol <- function(df2, targetcols) {
  for(coln in targetcols) {
    df2[[coln]] %>% median(na.rm=T) -> defval
    df2[[coln]] %<>% na.fill(defval)
  }
  return(df2)
}


adddefos <- function(df, exl='pop') {
  names(df)[sapply(df, is.numeric) & (!names(df) %in% exl)] -> targetcols
  df %>% ddply(.(estimate, Region, IncomeGroup), fillcol, targetcols) %>%
    ddply(.(estimate), fillcol, targetcols)
}


get_crfs <- function(){
  print("Getting CRFS")
  crfs <- read_csv(get_hia_path('CRFs.csv'), col_types = cols())

  names(crfs) %<>% gsub('RR_', '', .)
  crfs$Exposure %<>% gsub('PM2\\.5', "PM25", .)
  crfs$Incidence %<>% gsub('AllCauses', "AllCause", .)
  crfs$effectname <- paste0(crfs$Incidence %>% gsub('\\.per|_base', '', .),
                            '_',
                            crfs$Exposure %>% gsub('\\..*|nrt', '', .))


  crfs$Incidence[crfs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', crfs$Incidence)] %<>%
    gsub('NCD\\.LRI', 'AllCause', .)

  return(crfs)
}


get_epi <- function(){
  print("Getting EPI")
  epi <- read_csv(get_hia_path('epi_for_hia_C40.csv'), col_types = cols())

  epi %<>% adddefos

  #add missing admin regions
  epi %<>% filter(ISO3=='CHN') %>% mutate(ISO3='HKG', pop=7.392e6, country='Hong Kong', IncomeGroup="High income") %>% bind_rows(epi)
  epi %<>% filter(ISO3=='CHN') %>% mutate(ISO3='MAC', pop=622567, country='Macau', IncomeGroup="High income") %>% bind_rows(epi) %>% distinct

  return(epi)
}


get_gdp <- function(){
  print("Getting GDP")
  read_csv(get_hia_path('GDP.csv'), col_types = cols()) %>%
    dplyr::rename(iso3=ISO3)
}

get_gdp_historical <- function(start_year=1980, end_year=2020){
  list(GDP.PPP.2011USD = 'NY.GDP.PCAP.PP.KD',
       GDP.currUSD     = 'NY.GDP.PCAP.CD',
       GDP.currLCU     = 'NY.GDP.PCAP.CN',
       GDP.PPP.tot     = 'NY.GDP.MKTP.PP.KD') %>%
    lapply(readWB_online, start_date = start_year, end_date = end_year, latest.year.only=F) %>%
    bind_rows(.id='valuename') %>%
    sel(country, iso3, year, valuename, Value) %>%
    spread(valuename, Value)
}

get_gdp_forecast <- function(){
  print("Getting GDP forecast")
  gdp_forecast_file <- get_hia_path('OECD_GDP_forecast.csv')
  if(!file.exists(gdp_forecast_file)){
    download.file('https://stats.oecd.org/sdmx-json/data/DP_LIVE/.GDPLTFORECAST.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en',
                  gdp_forecast_file)
  }

  read_csv(gdp_forecast_file, col_types = cols()) %>%
    sel(iso3=LOCATION, year=TIME, GDP.realUSD.tot=Value)
}


get_valuation <- function(){
  print("Getting valuation")
  read_csv(get_hia_path('valuation.csv'), col_types = cols())
}


get_calc_causes <- function(){
  print("Getting calc_causes")
  #define short names
  names_causes <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

  c(paste0(c('NCD.LRI', 'LRI.child'), '_YLLs'),
    paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
    paste0(c(names_causes, 'LRI.child', 'Diabetes'), '_Deaths')) %>%
    unique
}


get_pop_proj <- function(){
  get_hia_path('WPP2019_population-death_rate-birth_rate.csv') %>%
    read_csv(.,col_types = cols()) %>%
    mutate(deaths=pop*death_rate) %>%
    dplyr::rename(iso3=ISO3,
                  year=Yr)
}

get_gemm <- function(){
  print("Getting GEMM")
  #read GEMM function fit parameters
  infile <- get_hia_path('GEMM Calculator (PNAS)_ab.xlsx')
  read_xlsx(infile, sheet='GEMM fit parameters', skip=8, n_max=14) -> gemm.china
  read_xlsx(infile, sheet='GEMM fit parameters', skip=29, n_max=14) -> gemm.exchina

  bind_rows(gemm.china %>% mutate(region='inc_China'),
            gemm.exchina %>% mutate(region='ex_China')) -> gemm

  #eliminate empty rows and columns
  gemm[rowSums(!is.na(gemm))>1, colSums(!is.na(gemm))>0] -> gemm
  # write.csv(gemm, 'gemm fit parameters.csv') #CHECK necessary?

  #read names of causes of death
  read_xlsx(infile, sheet='GEMM fit parameters', skip=6, n_max=1,
            col_names=F) %>%
    unlist %>% subset(!is.na(.)) ->
    causes

  #define short names
  names(causes) <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

  #remove duplicated age columns
  names(gemm)[1] <- 'age'
  gemm %<>% sel(-contains('Age'), age)

  #give parameter columns names; t=theta, se=standard error of theta, a=alpha, u=mu, p=pi
  names(causes) %>% sapply(paste, c('t', 'se', 'a', 'u', 'p'), sep='_') %>%
    as.vector() -> newnames
  names(gemm)[seq_along(newnames)] <- newnames

  gemm %<>% gather(cause, value, -region, -age) %>%
    separate(cause, c('cause', 'param'), '_')
  gemm$age[gemm$age=='30-35'] <- '30-34'
  # setwd(origwd)

  return(gemm)
}



get_ihme <- function(){
  print("Getting IHME")
  #read IHME mortality and morbidity data to enable country calculations
  ihme <- read.csv(get_hia_path('IHME-GBD_2017_DATA.csv')) %>%
    dplyr::filter(metric_name == 'Number') %>%
    gather_ihme

  ihme %<>% mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric)
  ihme$age_low[ihme$age_name=='Under 5'] <- 0
  ihme$age_low[ihme$age_name=='All Ages'] <- -1
  ihme$age <- ihme$age_name %>% gsub(' to ', '-', .) %>% gsub(' plus', '+', .)

  ihme %<>%
    dplyr::filter(age_low>=25) %>%
    group_by_at(vars(-val, -starts_with('age'))) %>%
    summarize_at('val', sum) %>%
    mutate(age='25+') %>% bind_rows(ihme) %>% ungroup

  ihme %<>% addiso

  ihme$cause_short <- NA
  ihme$cause_short[grep('Diab', ihme$cause_name)] <- 'Diabetes'
  ihme$cause_short[grep('Stroke', ihme$cause_name)] <- 'Stroke'
  ihme$cause_short[grep('Lower resp', ihme$cause_name)] <- 'LRI'
  ihme$cause_short[grep('Non-comm', ihme$cause_name)] <- 'NCD'
  ihme$cause_short[grep('Isch', ihme$cause_name)] <- 'IHD'
  ihme$cause_short[grep('obstr', ihme$cause_name)] <- 'COPD'
  ihme$cause_short[grep('lung canc', ihme$cause_name)] <- 'LC'

  ihme %<>%
    dplyr::filter(grepl('Lower resp|Non-comm', cause_name)) %>%
    group_by_at(vars(-val, -starts_with('cause'))) %>%
    summarize_at('val', sum) %>%
    mutate(cause_name='NCD+LRI', cause_short='NCD.LRI') %>% bind_rows(ihme) %>% ungroup

  ihme$age[ihme$age_low>=25] %>% subset(!is.na(.)) %>% unique -> adult.ages

  ihme %<>% dplyr::filter(ISO3 == 'ALB') %>% mutate(ISO3='XKX', country='Kosovo') %>% bind_rows(ihme)

  return(ihme)
}

get_adult_ages <- function(ihme=get_ihme()){
  print("Getting adult_ages")
  ihme$age[ihme$age_low>=25] %>% subset(!is.na(.)) %>%
    unique
}


get_gbd <- function(){
  print("Getting GBD")

  #read GBD RR values
  read.csv(get_hia_path('ier_computed_table.csv'), stringsAsFactors = F) %>% sel(-X, -age) %>%
    dplyr::filter(cause %in% c('lri', 't2_dm')) %>%
    dplyr::rename(cause_short = cause, central=rr_mean, low=rr_lower, high=rr_upper) %>%
    mutate(cause_short = recode(cause_short, lri='LRI.child', t2_dm='Diabetes')) %>%
    dplyr::filter(exposure <=300)
}

get_dict <- function(){
  get_hia_path('dict.csv') %>% read_csv(, col_types = cols())
}
