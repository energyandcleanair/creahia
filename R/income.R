#' Get income data per country/region
#'
#' @param year Optional vector of years to filter the data. If NULL, returns all available years.
#'
#' @returns A tibble with columns: iso3, country, year, income_type, value
#' @export
#'
#' @examples
get_income <- function(year = NULL) {

  income <- read_csv(get_hia_path('income.csv'), col_types = cols())

  if (!is.null(year)) {
    income <- income %>% filter(year %in% !!year)
  }

  if (nrow(income) == 0) {
    logger::log_info(glue("No income data found for requested year(s). Attempting to download... Consider using recreate_gdp and rebuild the package."))
    end_year <- if(is.null(year)) lubridate::year(lubridate::today()) else max(year)
    start_year <- if(is.null(year)) 1980 else min(year)

    income <- download_income(start_year = start_year, end_year = end_year)

    if (is.null(income) || nrow(income) == 0) {
      logger::log_error("Failed to download income data")
      return(NULL)
    }

    if (!is.null(year)) {
      income <- income %>% filter(year %in% !!year)
    }
  }

  return(income)
}



#' Download GDP and GNI data from World Bank API
#'
download_income <- function(start_year = 1980,
                            end_year = lubridate::year(lubridate::today())) {

  logger::log_info(glue("Downloading GDP data from {start_year} to {end_year}"))

  income_raw <- list(
    # GDP per capita
    GDP.PC.PPP.constUSD = 'NY.GDP.PCAP.PP.KD',
    GDP.PC.PPP.currUSD = 'NY.GDP.PCAP.PP.CD',
    GDP.PC.currLCU     = 'NY.GDP.PCAP.CN',
    GDP.PC.currUSD     = 'NY.GDP.PCAP.CD',
    GDP.PC.constUSD     = 'NY.GDP.PCAP.KD',

    # GDP total
    GDP.TOT.PPP.constUSD = 'NY.GDP.MKTP.PP.KD',
    GDP.TOT.PPP.currUSD = 'NY.GDP.MKTP.PP.CD',
    GDP.TOT.currLCU    = 'NY.GDP.MKTP.CN',
    GDP.TOT.currUSD    = 'NY.GDP.MKTP.CD',
    GDP.TOT.constUSD    = 'NY.GDP.MKTP.KD',

    # GNI per capita
    GNI.PC.PPP.constUSD = 'NY.GNP.PCAP.PP.KD',
    GNI.PC.PPP.currUSD = 'NY.GNP.PCAP.PP.CD',
    GNI.PC.currLCU     = 'NY.GNP.PCAP.CN',
    GNI.PC.currUSD     = 'NY.GNP.PCAP.CD',
    GNI.PC.constUSD     = 'NY.GNP.PCAP.KD',

    # PPP conversion factor
    PPP.convLCUUSD     = 'PA.NUS.PPP'
  ) %>%
    pbapply::pblapply(function(x){
      # Sleeping to prevent timeout on WB api
      Sys.sleep(5)
      wbstats::wb_data(x,
                       start_date = start_year,
                       end_date = end_year,
                       country = "all", # Important to have OECD, EUU etc. for valuation purposes
                       return_wide = FALSE
      )
    }) %>%
    bind_rows(.id = 'income_type')

  income <- income_raw %>%
    select(iso3 = iso3c,
           country = country,
           year = date,
           income_type,
           value = value
    )

  return(income)
}

#' Update the income data file bundled with the package
#'
#' This function downloads the latest GDP/GNI data and saves it to the package's extdata folder.
#' It should be run by package maintainers when updating the package.
#' Data will then be available offline using get_income
#'
recreate_income <- function(
    start_year = 1980,
    end_year = lubridate::year(lubridate::today()),
    output_path = "inst/extdata/income.csv") {

  income <- download_income(start_year = start_year, end_year = end_year)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(income, output_path)
}
