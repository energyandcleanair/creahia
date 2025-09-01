#' Get path of external data bundled with creahia package
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
get_hia_path <- function(filename, error_if_not_exists=F) {
  # We bundle HIA data with creahia package, in inst/extdata
  # Read here for more details:
  # https://r-pkgs.org/data.html#data-extdata

  file1 <- file.path(find.package("creahia"), "extdata", filename)
  file2 <- file.path(find.package("creahia"), "inst", "extdata", filename)

  if(!file.exists(file1) && !file.exists(file2)) {
    if(error_if_not_exists) stop("Couldn't find file ", filename, " in HIA folder")
    else warning("Couldn't find file ", filename, " in HIA folder")
  }

  if(file.exists(file1)) return(file1)
  else return(file2)
}


get_hia_paths <- function(pattern, path="", error_if_not_exists=F) {
  # We bundle HIA data with creahia package, in inst/extdata
  # Read here for more details:
  # https://r-pkgs.org/data.html#data-extdata
  dirs <- c(
    file.path(find.package("creahia"), "extdata"),
    file.path(find.package("creahia"), "inst", "extdata")
  )

  dir <- dirs[which(dir.exists(dirs))]
  if(length(dir) == 0){
    warning("Could not find extdata directory")
    return(c())
  }

  list.files(path = file.path(dir, path),
             pattern = pattern,
             full.names = T)
}


# fill in default values for places missing data
fillcol <- function(df2, targetcols) {
  for(coln in targetcols) {
    defval <- df2[[coln]] %>% median(na.rm = T)
    df2[[coln]] <- df2[[coln]] %>% na.fill(defval)
  }
  return(df2)
}


adddefos <- function(df, exl = c('pop', 'location_id', 'location_level')) {
  targetcols <- names(df)[sapply(df, is.numeric) & (!names(df) %in% exl)]
  df %>%
    ddply(.(estimate, region, income_group), fillcol, targetcols) %>%
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
  crfs$Incidence <- crf_recode_incidence(crfs$Incidence, crfs$Exposure)
  crfs$effectname <- paste0(crfs$Incidence %>% gsub('\\.per|_base', '', .),
                            '_',
                            crfs$Exposure %>% gsub('\\..*|nrt', '', .))

  return(crfs)
}


fix_epi_cols <- function(epi){

  old_new_cols <- list(
    Region='region',
    Country='country',
    ISO3='iso3',
    IncomeGroup='income_group'
  )

  for(old_col in names(old_new_cols)) {
    new_col <- old_new_cols[[old_col]]
    # Rename if exists
    if(old_col %in% names(epi)) {
      epi <- epi %>% dplyr::rename(!!new_col := !!old_col)
    }
  }

  epi
}

get_epi_versions <- function() {
  list(
    "default" = "epi_for_hia.csv",
    "C40" = "epi_for_hia_C40.csv",
    "gbd2017" = "epi_for_hia_gbd2017.csv",
    "gbd2019" = "epi_for_hia_gbd2019.csv",
    "gbd2021" = "epi_for_hia_gbd2021.csv"
  )
}


get_epi <- function(version = "default") {

  filename <- get_epi_versions()[[version]]
  print(sprintf("Getting epi: %s", filename))

  epi <- read_csv(get_hia_path(filename), col_types = cols()) %>%
    fix_epi_cols() %>%
    adddefos %>%
    add_location_details()

  # add missing admin regions
  if(!'HKG' %in% epi$iso3){
     epi <- add_country_to_epi_wide(
        epi,
        base_iso3= 'CHN',
        iso3 = 'HKG',
        pop = 7.392e6,
        name = 'Hong Kong',
        income_group = "High income"
    )
  }

  if(!'MAC' %in% epi$iso3){
    epi <- add_country_to_epi_wide(
      epi,
      base_iso3= 'CHN',
      iso3 = 'MAC',
      pop = 622567,
      name = 'Macau',
      income_group = "High income"
    )
  }



  return(epi %>% distinct())
}


get_gdp_forecast <- function(pop_proj=NULL) {
  print("Getting GDP forecast")
  gdp_forecast_file <- get_hia_path('OECD_GDP_forecast.csv')
  if(!file.exists(gdp_forecast_file)) {
    download.file('https://stats.oecd.org/sdmx-json/data/DP_LIVE/.GDPLTFORECAST.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en',
                  gdp_forecast_file)
  }

  gdp_forecast <- read_csv(gdp_forecast_file, col_types = cols()) %>%
    mutate(GDP.realUSD = Value * 1e6) %>%
    sel(iso3 = LOCATION, year = TIME, GDP.realUSD)

  # Add per capita
  pop_proj <- creahelpers::default_if_null(pop_proj, get_pop_proj())

  gdp_forecast %>%
    left_join(pop_proj %>%
                group_by(year, iso3) %>%
                summarise(pop=sum(pop)*1000)) %>%
    mutate(GDP.PC.realUSD = GDP.realUSD / pop) %>%
    select(-c(pop))
}

get_gdp_scaling <- function(iso3){

  gdp_historical <- get_income() %>%
    spread(income_type, value)
  gdp_forecast <- get_gdp_forecast()

  gdp_all <- suppressMessages(full_join(gdp_historical, gdp_forecast, by=c("iso3", "year"))) %>%
    filter(iso3 %in% !!iso3)

  suppressMessages(
    gdp_all %>%
      # mutate(GDP.realUSD = GDP.realUSD.tot * 1000 / pop) %>%
      group_by(iso3) %>%
      group_modify(function(df, ...) {

        PPP.scaling <- df$GDP.PC.PPP.2017USD[df$year == 2019] / df$GDP.PC.realUSD[df$year == 2019]

        past.scaling <- df %>% filter(!is.na(GDP.PC.PPP.2017USD + GDP.PC.realUSD)) %>% head(1)
        ind <- df$year < past.scaling$year

        df$GDP.PC.PPP.2017USD[ind] <- df$GDP.PC.PPP.2017USD[ind] %>%
          na.cover(df$GDP.PC.realUSD[ind] * past.scaling$GDP.PC.PPP.2017USD / past.scaling$GDP.PC.realUSD)


        future.scaling = df %>% filter(!is.na(GDP.PC.PPP.2017USD + GDP.PC.realUSD)) %>% tail(1)
        ind <- df$year > future.scaling$year

        df$GDP.PC.PPP.2017USD[ind] <- df$GDP.PC.PPP.2017USD[ind] %>%
          na.cover(df$GDP.PC.realUSD[ind] * future.scaling$GDP.PC.PPP.2017USD / future.scaling$GDP.PC.realUSD)

        return(df)
      }))
}


get_valuation_versions <- function() {
  list(
    "default" = "valuation.csv",
    "viscusi" = "valuation_viscusi.csv",
    "viscusi_gdp" = "valuation_viscusi_gdp.csv",
    "viscusi_gni" = "valuation_viscusi_gni.csv",
    "worldbank" = "valuation_worldbank.csv"
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


get_ihme <- function(version='gbd2017') {

  file_version <- recode(
    version,
    default='gbd2017',
    C40='gbd2017',
    gbd2019='gbd2019',
    gbd2021='gbd2021'
  )

  read_csv(get_hia_path(glue("ihme_{file_version}.csv")), col_types = cols())
}


get_adult_ages <- function(ihme) {
  ihme$age[ihme$age_low >= 25] %>% subset(!is.na(.)) %>%
    unique
}

get_gbd_rr <- function(version="original", gbd_causes=c('LRI.child', 'Diabetes')){

  gbd_rr <-read_csv(get_hia_path(glue("gbd_rr_{version}.csv")), col_types = cols())

  if(length(gbd_causes) == 0) gbd_causes <- 'none'
  if(gbd_causes[1] != 'all') gbd_rr <- gbd_rr %>% dplyr::filter(cause_short %in% gbd_causes)
  gbd_rr
}


get_dict <- function(cols=c("Code", "Long.name")) {
  get_hia_path('dict.csv') %>%
    read_csv(col_types = cols()) %>%
    select_at(cols)
}


merge_into <- c(IND = 'KAS', AUS = 'IOA', FIN = 'ALA')
use_as_proxy <- c(CHN = 'HKG', CHN = 'MAC', AUT = 'LIE', VUT = 'PLW', ITA = 'SMR')


country.recode <- function(ISOcodes, replacements) {
  for(i in 1:length(replacements))
    ISOcodes[ISOcodes == replacements[i]] <- names(replacements)[i]
  return(ISOcodes)
}
