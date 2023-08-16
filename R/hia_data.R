#' Get path of external data bundled with creahia package
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
get_hia_path <- function(filename) {
  # We bundle HIA data with creahia package, in inst/extdata
  # Read here for more details:
  # https://r-pkgs.org/data.html#data-extdata
  f <- system.file("extdata", filename, package = "creahia")
  if(f == "") stop("Couldn't find file ", filename, " in HIA folder")
  return(f)
}


# fill in default values for places missing data
fillcol <- function(df2, targetcols) {
  for(coln in targetcols) {
    defval <- df2[[coln]] %>% median(na.rm = T)
    df2[[coln]] <- df2[[coln]] %>% na.fill(defval)
  }
  return(df2)
}


adddefos <- function(df, exl = 'pop') {
  targetcols <- names(df)[sapply(df, is.numeric) & (!names(df) %in% exl)]
  df %>% ddply(.(estimate, Region, IncomeGroup), fillcol, targetcols) %>%
    ddply(.(estimate), fillcol, targetcols)
}


get_crfs_versions <- function() {
  list(
    "default" = "CRFs.csv",
    "C40" = "CRFs_C40.csv",
    "Krewski" = "CRFs_Krewski.csv",
    "Krewski-South Africa" = "CRFs_Krewski_SouthAfrica.csv"
  )
}


get_crfs <- function(version = "default") {
  filename <- get_crfs_versions()[[version]]
  print(sprintf("Getting CRFS: %s", filename))

  crfs <- read_csv(get_hia_path(filename), col_types = cols())

  names(crfs) <- names(crfs) %>% gsub('RR_', '', .)
  crfs$Exposure <- crfs$Exposure %>% gsub('PM2\\.5', "PM25", .)
  crfs$Incidence <- crfs$Incidence %>% gsub('AllCauses', "AllCause", .)
  crfs$effectname <- paste0(crfs$Incidence %>% gsub('\\.per|_base', '', .),
                            '_',
                            crfs$Exposure %>% gsub('\\..*|nrt', '', .))


  crfs$Incidence[crfs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', crfs$Incidence)] <-
    crfs$Incidence[crfs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', crfs$Incidence)] %>%
    gsub('NCD\\.LRI', 'AllCause', .)

  return(crfs)
}


get_epi_versions <- function() {
  list(
    "default" = "epi_for_hia.csv",
    "C40" = "epi_for_hia_C40.csv"
  )
}


get_epi <- function(version = "default") {

  filename <- get_epi_versions()[[version]]
  print(sprintf("Getting epi: %s", filename))

  epi <- read_csv(get_hia_path(filename), col_types = cols()) %>% adddefos

  # add missing admin regions
  epi <- epi %>% filter(ISO3 == 'CHN') %>%
    mutate(ISO3 = 'HKG', pop = 7.392e6, country = 'Hong Kong',
           IncomeGroup = "High income") %>%
    bind_rows(epi)
  epi <- epi %>% filter(ISO3 == 'CHN') %>%
    mutate(ISO3 = 'MAC', pop = 622567, country = 'Macau',
           IncomeGroup = "High income") %>%
    bind_rows(epi) %>%
    distinct

  return(epi)
}


get_gdp <- function(year = NULL) {

  print("Getting GDP")
  read_csv(get_hia_path('gdp.csv'), col_types = cols()) %>%
    filter(is.null(!!year) | year %in% !!year)
}


#' Every time new data is available, we want to build a new gdp.csv file
#'
#' @return
#' @export
#'
#' @examples
recreate_gdp <- function() {
  gdp <- get_gdp_timeseries()
  write_csv(gdp, 'inst/extdata/gdp.csv')
}


get_gdp_timeseries <- function(start_year = 1980, end_year = 2021) {
  list(GDP.PC.PPP.2017USD = 'NY.GDP.PCAP.PP.KD',
       GDP.PC.PPP.currUSD = 'NY.GDP.PCAP.PP.CD',
       GDP.PC.currLCU     = 'NY.GDP.PCAP.CN',
       GDP.PC.currUSD     = 'NY.GDP.PCAP.CD',
       GDP.PC.2015USD     = 'NY.GDP.PCAP.KD',
       GDP.TOT.currLCU    = 'NY.GDP.MKTP.CN',
       GDP.TOT.currUSD    = 'NY.GDP.MKTP.CD',

       GNI.PC.PPP.2017USD = 'NY.GNP.PCAP.PP.KD',
       GNI.PC.PPP.currUSD = 'NY.GNP.PCAP.PP.CD',
       GNI.PC.currLCU     = 'NY.GNP.PCAP.CN',
       GNI.PC.currUSD     = 'NY.GNP.PCAP.CD',
       PPP.convLCUUSD     = 'PA.NUS.PPP') %>%
    lapply(readWB_online, start_date = start_year, end_date = end_year, latest.year.only = F) %>%
    bind_rows(.id = 'valuename') %>%
    sel(country, iso3, year, valuename, Value) %>%
    spread(valuename, Value)
}


get_gdp_forecast <- function() {
  print("Getting GDP forecast")
  gdp_forecast_file <- get_hia_path('OECD_GDP_forecast.csv')
  if(!file.exists(gdp_forecast_file)) {
    download.file('https://stats.oecd.org/sdmx-json/data/DP_LIVE/.GDPLTFORECAST.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en',
                  gdp_forecast_file)
  }

  read_csv(gdp_forecast_file, col_types = cols()) %>%
    sel(iso3 = LOCATION, year = TIME, GDP.realUSD.tot = Value)
}


get_valuation_versions <- function() {
  list(
    "default" = "valuation.csv",
    "viscusi" = "valuation_viscusi.csv",
    "viscusi_gdp" = "valuation_viscusi_gdp.csv",
    "viscusi_gni" = "valuation_viscusi_gni.csv"
  )
}


get_valuation <- function(version = "default") {
  filename <- get_valuation_versions()[[version]]
  print(sprintf("Getting epi: %s", filename))
  read_csv(get_hia_path(filename), col_types = cols())
}


get_calc_causes <- function(causes_set = 'GEMM and GBD', filter = NULL) {
  print("Getting calc_causes")

  if(causes_set == 'GEMM and GBD') {
    # define short names
    names_causes <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

    causes_out <- c(paste0(c('NCD.LRI', 'LRI.child'), '_YLLs'),
                    paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
                    paste0(c(names_causes, 'LRI.child', 'Diabetes'), '_Deaths')) %>%
      unique
  }

  if(causes_set == 'GEMM only') {
    # define short names
    names_causes <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

    causes_out <- c(paste0(c('NCD.LRI'), '_YLLs'),
                    paste0(c('Stroke', 'COPD'), '_YLDs'),
                    paste0(c(names_causes), '_Deaths')) %>%
      unique
  }

  if(causes_set == 'GBD only') {
    # define short names
    names_causes <- c('IHD', 'Stroke', 'COPD', 'LC', 'LRI')

    causes_out <- c(paste0(c(names_causes, 'LRI.child'), '_YLLs'),
                    paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
                    paste0(c(names_causes, 'LRI.child', 'Diabetes'), '_Deaths')) %>%
      unique
  }

  if(!is.null(filter)) {
    causes_out <- causes_out %>% grep(filter, ., value = T)
  }

  return(causes_out)
}


get_pop_proj <- function() {
  creahelpers::get_population_path('WPP2019_population-death_rate-birth_rate.csv') %>%
    read_csv(., col_types = cols()) %>%
    mutate(deaths = pop * death_rate) %>%
    dplyr::rename(iso3 = ISO3, year = Yr)
}


get_gemm <- function() {
  print("Getting GEMM")

  # read GEMM function fit parameters
  infile <- get_hia_path('GEMM Calculator (PNAS)_ab.xlsx')
  gemm.china <- suppressMessages(read_xlsx(infile, sheet = 'GEMM fit parameters',
                                           skip = 8, n_max = 14))
  gemm.exchina <- suppressMessages(read_xlsx(infile, sheet = 'GEMM fit parameters',
                                             skip = 29, n_max = 14))

  gemm <- bind_rows(gemm.china %>% mutate(region = 'inc_China'),
            gemm.exchina %>% mutate(region = 'ex_China'))

  # eliminate empty rows and columns
  gemm <- gemm[rowSums(!is.na(gemm)) > 1, colSums(!is.na(gemm)) > 0]
  # write.csv(gemm, 'gemm fit parameters.csv') # CHECK necessary?

  # read names of causes of death
  causes <- suppressMessages(read_xlsx(infile, sheet = 'GEMM fit parameters',
                                       skip = 6, n_max = 1, col_names = F)) %>%
    unlist %>%
    subset(!is.na(.))


  # define short names
  names(causes) <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

  # remove duplicated age columns
  names(gemm)[1] <- 'age'
  gemm <- gemm %>% sel(-contains('Age'), age)

  # give parameter columns names; t = theta, se = standard error of theta,
  # a = alpha, u = mu, p = pi
  newnames <- names(causes) %>% sapply(paste, c('t', 'se', 'a', 'u', 'p'), sep = '_') %>%
    as.vector()
  names(gemm)[seq_along(newnames)] <- newnames

  gemm <- gemm %>% gather(cause, value, -region, -age) %>%
    tidyr::separate(cause, c('cause', 'param'), '_')
  gemm$age[gemm$age == '30-35'] <- '30-34'
  # setwd(origwd)

  return(gemm)
}


get_ihme <- function() {
  print("Getting IHME")

  # read IHME mortality and morbidity data to enable country calculations
  ihme <- read.csv(get_hia_path('IHME-GBD_2017_DATA.csv')) %>%
    dplyr::filter(metric_name == 'Number') %>%
    gather_ihme

  ihme <- ihme %>% mutate(age_low = age_name %>% gsub(" .*", "", .) %>% as.numeric)
  ihme$age_low[ihme$age_name == 'Under 5'] <- 0
  ihme$age_low[ihme$age_name == 'All Ages'] <- -1
  ihme$age <- ihme$age_name %>% gsub(' to ', '-', .) %>%
    gsub(' plus', '+', .)

  ihme <- ihme %>%
    dplyr::filter(age_low >= 25) %>%
    group_by_at(vars(-val, -starts_with('age'))) %>%
    summarise_at('val', sum) %>%
    mutate(age = '25+') %>%
    bind_rows(ihme) %>%
    ungroup

  ihme <- ihme %>% addiso

  # TODO test
  ihme <- ihme %>%
    mutate(cause_short = case_when(grep('Diab', cause_name) ~ 'Diabetes',
                                   grep('Stroke', cause_name) ~ 'Stroke',
                                   grep('Lower resp', cause_name) ~ 'LRI',
                                   grep('Non-comm', cause_name) ~ 'NCD',
                                   grep('Isch', cause_name) ~ 'IHD',
                                   grep('obstr', cause_name) ~ 'COPD',
                                   grep('lung canc', cause_name) ~ 'LC',
                                   T ~ NA))

  # ihme$cause_short <- NA
  # ihme$cause_short[grep('Diab', ihme$cause_name)] <- 'Diabetes'
  # ihme$cause_short[grep('Stroke', ihme$cause_name)] <- 'Stroke'
  # ihme$cause_short[grep('Lower resp', ihme$cause_name)] <- 'LRI'
  # ihme$cause_short[grep('Non-comm', ihme$cause_name)] <- 'NCD'
  # ihme$cause_short[grep('Isch', ihme$cause_name)] <- 'IHD'
  # ihme$cause_short[grep('obstr', ihme$cause_name)] <- 'COPD'
  # ihme$cause_short[grep('lung canc', ihme$cause_name)] <- 'LC'

  ihme <- ihme %>%
    dplyr::filter(grepl('Lower resp|Non-comm', cause_name)) %>%
    group_by_at(vars(-val, -starts_with('cause'))) %>%
    summarise_at('val', sum) %>%
    mutate(cause_name = 'NCD+LRI', cause_short = 'NCD.LRI') %>%
    bind_rows(ihme) %>%
    ungroup

  adult.ages <- ihme$age[ihme$age_low >= 25] %>% subset(!is.na(.)) %>%
    unique

  ihme <- ihme %>% dplyr::filter(ISO3 == 'ALB') %>%
    mutate(ISO3 = 'XKX', country = 'Kosovo') %>%
    bind_rows(ihme)

  return(ihme)
}


get_adult_ages <- function(ihme = get_ihme()) {
  print("Getting adult_ages")

  ihme$age[ihme$age_low >= 25] %>% subset(!is.na(.)) %>%
    unique
}


get_gbd <- function(gbd_causes = c('LRI.child', 'Diabetes')) {
  print("Getting GBD")

  if(length(gbd_causes) == 0) gbd_causes <- 'none'

  # read GBD RR values
  gbd <- read_csv(get_hia_path('ier_computed_table.csv')) %>% dplyr::select(-...1) %>%
    dplyr::rename(cause_short = cause, central = rr_mean, low = rr_lower, high = rr_upper) %>%
    mutate(cause_short = recode(cause_short,
                                lri = 'LRI',
                                t2_dm = 'Diabetes',
                                cvd_ihd = 'IHD',
                                cvd_stroke = 'Stroke',
                                neo_lung = 'LC',
                                resp_copd = 'COPD'),
           age = case_when(age == 99 ~ '25+',
                           age == 80 ~ '80+',
                           age < 80 ~ paste0(age, '-', age + 4))) %>%
    dplyr::filter(exposure <= 300, !is.na(age))

  # add the LRI risk function to be used for children if needed
  if(any(c('LRI.child', 'all') %in% gbd_causes)) {
    gbd <- gbd %>% filter(cause_short == 'LRI') %>%
      mutate(cause_short = 'LRI.child', age = 'Under 5') %>%
      bind_rows(gbd)
  }

  if(gbd_causes[1] != 'all') gbd <- gbd %>% dplyr::filter(cause_short %in% gbd_causes)

  return(gbd)
}


get_dict <- function() {
  get_hia_path('dict.csv') %>% read_csv(col_types = cols())
}


merge_into <- c(IND = 'KAS', AUS = 'IOA', FIN = 'ALA')
use_as_proxy <- c(CHN = 'HKG', CHN = 'MAC', AUT = 'LIE', VUT = 'PLW', ITA = 'SMR')


country.recode <- function(ISOcodes, replacements) {
  for(i in 1:length(replacements))
    ISOcodes[ISOcodes == replacements[i]] <- names(replacements)[i]
  return(ISOcodes)
}
