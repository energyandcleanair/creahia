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

  # First, fill within (estimate, region, income_group)
  df1 <- df %>%
    dplyr::group_by(estimate, region, income_group) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(targetcols),
      ~ ifelse(is.na(.x), stats::median(.x, na.rm = TRUE), .x)
    )) %>%
    dplyr::ungroup()

  # Then, fill remaining NAs within (estimate)
  df2 <- df1 %>%
    dplyr::group_by(estimate) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(targetcols),
      ~ ifelse(is.na(.x), stats::median(.x, na.rm = TRUE), .x)
    )) %>%
    dplyr::ungroup()

  df2
}


get_crfs_versions <- function() {
  list(
    "default" = "rr/processed/CRFs.csv",
    "C40" = "rr/processed/CRFs_C40.csv",
    "Krewski-South Africa" = "rr/processed/CRFs_Krewski_SouthAfrica.csv"
  )
}


get_crfs <- function(version = "default") {

  filename <- get_crfs_versions()[[version]]
  crfs <- read_csv(get_hia_path(filename), col_types = cols())
  crfs$pollutant <- crfs$pollutant %>% gsub('PM2\\.5', "PM25", .)
  return(crfs)
}


fix_epi_cols <- function(epi){

  # Old names to new names
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

  # Clean the asthma cause-outcome data to have a single cause (i.e. Asthma)
  # and various outcomes i.e. AsthmaPrevalence, AsthmaIncidence, AsthmaERV (emergency room visit)
  # Asthma.Inci.{} -> Asthma.{}_AsthmaIncidence
  # Asthma.Prev.{} -> Asthma.{}_AsthmaPrevalence
  # exac.{} -> Asthma.{}_AsthmaERV
  col_names <- names(epi)
  asthma_cols <- col_names[grepl('^Asthma\\.(Inci|Prev)\\.', col_names) | grepl('^exac\\.', col_names)]
  for(col in asthma_cols) {
    if(grepl('^Asthma\\.Inci\\.', col)) {
      newname <- gsub('^Asthma\\.Inci\\.', 'Asthma.', col)
      newname <- paste0(newname, '_AsthmaIncidence')
    }
    if(grepl('^Asthma\\.Prev\\.', col)) {
      newname <- gsub('^Asthma\\.Prev\\.', 'Asthma.', col)
      newname <- paste0(newname, '_AsthmaPrevalence')
    }
    if(grepl('^exac\\.', col)) {
      newname <- gsub('^exac\\.', 'Asthma.', col)
      newname <- paste0(newname, '_AsthmaERV')
    }
    message("Renaming asthma column ", col, " to ", newname)
    colnames(epi)[colnames(epi) == col] <- newname
  }

  # For causes without outcomes, add default one to be compatible with new (explicit) crfs
  # Select columns that are numeric and have no _ in their name (and not the pop column)
  causes_wo_outcome <- names(epi)[sapply(epi, is.numeric) & !grepl('_', names(epi))
                                  & !names(epi) %in% c("pop", "birth_rate_p1k", "labor_partic_pct")]
  for(cause in causes_wo_outcome) {
    newname <- paste0(cause, '_', cause)
    colnames(epi)[colnames(epi) == cause] <- newname
  }


  epi
}

get_epi_versions <- function() {
  list(
    "original" = "epi/processed/epi_rate_wide_original.csv",
    "C40" = "epi/processed/epi_rate_wide_C40.csv",
    "gbd2017" = "epi/processed/epi_rate_wide_gbd2017.csv",
    "gbd2019" = "epi/processed/epi_rate_wide_gbd2019.csv",
    "gbd2021" = "epi/processed/epi_rate_wide_gbd2021.csv",
    # Default is the latest GBD version
    "default" = "epi/processed/epi_rate_wide_gbd2021.csv"
  )
}


get_epi <- function(version = "default") {

  filename <- get_epi_versions()[[version]]
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



#'
#' @param epi
#'
#' @returns
#' @export
#'
#' @examples
clean_epi_asthma <- function(epi) {

  cols <- colnames(epi)


  # Asthma.


  # Clean asthma data
  if('Asthma_Number' %in% names(epi)) {
    epi <- epi %>%
      mutate(Asthma_Number = ifelse(is.na(Asthma_Number) & !is.na(Asthma_Prevalence),
                                    Asthma_Prevalence * pop / 100,
                                    Asthma_Number),
             Asthma_Prevalence = ifelse(is.na(Asthma_Prevalence) & !is.na(Asthma_Number),
                                        Asthma_Number / pop * 100,
                                        Asthma_Prevalence)
      )
  }
  epi
}

get_gdp_forecast <- function(pop_proj=NULL) {
  print("Getting GDP forecast")
  gdp_forecast_file <- get_hia_path('economics/OECD_GDP_forecast.csv')
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
                summarise(pop=sum(pop)*1000), by = c("year", "iso3")) %>%
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

        PPP.scaling <- df$GDP.PC.PPP.constUSD[df$year == 2019] / df$GDP.PC.realUSD[df$year == 2019]

        past.scaling <- df %>% filter(!is.na(GDP.PC.PPP.constUSD + GDP.PC.realUSD)) %>% head(1)
        ind <- df$year < past.scaling$year

        df$GDP.PC.PPP.constUSD[ind] <- df$GDP.PC.PPP.constUSD[ind] %>%
          na.cover(df$GDP.PC.realUSD[ind] * past.scaling$GDP.PC.PPP.constUSD / past.scaling$GDP.PC.realUSD)


        future.scaling = df %>% filter(!is.na(GDP.PC.PPP.constUSD + GDP.PC.realUSD)) %>% tail(1)
        ind <- df$year > future.scaling$year

        df$GDP.PC.PPP.constUSD[ind] <- df$GDP.PC.PPP.constUSD[ind] %>%
          na.cover(df$GDP.PC.realUSD[ind] * future.scaling$GDP.PC.PPP.constUSD / future.scaling$GDP.PC.realUSD)

        return(df)
      }))
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

get_epi_count_long_raw <- function(version = 'gbd2021') {
  file_version <- recode(
    version,
    C40='gbd2017',
    gbd2017='gbd2017',
    gbd2019='gbd2019',
    gbd2021='gbd2021',
    # Default is the latest GBD version
    default='gbd2021',
  )

  ihme <- read_csv(get_hia_path(glue("epi/processed/epi_count_long_{file_version}.csv")), col_types = cols())

  # Backward compatibility: handle old format with cause_short and cause_name
  if ("cause_short" %in% names(ihme) && "cause_name" %in% names(ihme)) {
    ihme <- ihme %>%
      mutate(cause = cause_short) %>%
      select(-cause_name, -cause_short)
  }

  # Validate age completeness (allows both aggregate and split ages to coexist)
  check_age_completeness(unique(ihme$age), data_name = glue("EPI count long {file_version}"))

  return(ihme)
}

# Memoised version to avoid re-reading large CSV files
get_epi_count_long <- memoise::memoise(get_epi_count_long_raw)

# Helper function to clear cache if needed
clear_epi_count_long_cache <- function() {
  memoise::forget(get_epi_count_long)
}

# Get age weights for a specific region, cause, and measure
get_age_weights <- function(region_id, cause, measure, rr_source, version = "gbd2019") {
  ihme <- get_epi_count_long(version)

  ages <- get_rr(rr_source) %>%
    filter(cause == !!cause) %>%
    distinct(age) %>%
    pull(age) %>%
    deduplicate_adult_ages()

  # Validate that deduplicated ages have no overlap and are complete
  check_age_coverage_and_uniqueness(ages, data_name = glue("RR {rr_source} for {cause}"))

  age_weights <- ihme %>%
    mutate(age = recode_age(age)) %>%
    filter(location_id == get_epi_location_id(region_id),
           cause == !!cause,
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

  return(list(ages = ages, age_weights = age_weights))
}


get_adult_ages <- function(ihme) {
  ihme$age[ihme$age_low >= 25] %>% subset(!is.na(.)) %>%
    unique
}

get_gbd_rr <- function(version="original", gbd_causes=c('LRI.child', 'Diabetes')){

  gbd_rr <-read_csv(get_hia_path(glue("rr/processed/rr_{version}.csv")), col_types = cols())

  if(length(gbd_causes) == 0) gbd_causes <- 'none'
  if(gbd_causes[1] != 'all') gbd_rr <- gbd_rr %>% dplyr::filter(cause %in% gbd_causes)
  gbd_rr
}


get_dict <- function(cols=c("code", "long_name")) {
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
