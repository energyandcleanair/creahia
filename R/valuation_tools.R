#' Takes raw values and convert them to World average 2017 in 2017 intl dollars,
#' using both GNI PPP and GDP PPP
#'
#' @return
#' @export
#'
#' @examples
create_valuation_file <- function(input_file = 'inst/extdata/valuation_viscusi_raw.csv',
                                  output_file = 'inst/extdata/valuation_viscusi.csv') {

  raw <- read_csv(input_file)

  # Step 0: convert currency
  library(priceR)
  exchange <- raw %>%
    filter(currency != 'USD') %>%
    distinct(currency, year) %>%
    rowwise() %>%
    mutate(exchange = mean(historical_exchange_rates(currency, to = "USD",
                                                     start_date = sprintf("%s-01-01", year),
                                                     end_date = sprintf("%s-12-31", year))[,2],
                           na.rm = T)) %>%
    ungroup()


  valuation <- raw %>%
    left_join(exchange) %>%
    mutate(valuation_usd=case_when(currency == 'USD' ~ valuation,
                                   T ~ valuation * exchange)) %>%
    mutate(cpi_ref = case_when(
      grepl('California', reference_income_level) ~ 'USA', # For CPI we assume Cal = USA
      reference_income_level == 'UK' ~ 'GBR',
      reference_income_level == 'EU' ~ 'EUU',
      T ~ reference_income_level
    ))

  start_year <- min(raw$year)
  end_year <- max(raw$year)

  # Step 1: scale to 2017$ using CPI
  # https://aspe.hhs.gov/sites/default/files/2021-07/hhs-guidelines-appendix-d-vsl-update.pdf
  # cpi <- wbstats::wb_data('FP.CPI.TOTL', start_date=start_year, end_date=end_year, country=unique(valuation$cpi_ref))
  #
  # cpi_eu <- eurostat::get_eurostat("tec00027", stringsAsFactors = FALSE,  time_format = "num") %>%
  #   filter(geo=='EU') %>%
  #   mutate(geo='EUU')
  #
  # cpi_cal <- cpi %>%
  #   filter(iso3c=='USA') %>%
  #   mutate(iso3c='California')
  #
  # cpi_all <- bind_rows(
  #   cpi %>% select(reference_income_level=iso3c, year=date, cpi_refyear='FP.CPI.TOTL'),
  #   cpi_cal %>% select(reference_income_level=iso3c, year=date, cpi_refyear='FP.CPI.TOTL'),
  #   cpi_eu %>% select(reference_income_level=geo, year=time, cpi_refyear=values),
  # ) %>%
  #   filter(!is.na(cpi_refyear))
  #
  # cpi_all <- cpi_all %>%
  #   left_join(cpi_all %>%
  #               filter(year==2017) %>%
  #               select(reference_income_level, cpi_2017=cpi_refyear))


  # Step 2: Bring to World average valuation international dollars 2017
  # Using both GDP and GNI
  # Get PPP GDP and GNI in constant 2017 dollars
  gdp_gni <- wbstats::wb_data(c(
    gdp_current = 'NY.GDP.PCAP.CD',
    gdp_constant2015 = 'NY.GDP.PCAP.KD',
    gdp_ppp_current = 'NY.GDP.PCAP.PP.CD',
    gni_ppp_current = 'NY.GNP.PCAP.PP.CD',
    gdp_ppp_intl2017 = 'NY.GDP.PCAP.PP.KD',
    gni_ppp_intl2017 = 'NY.GNP.PCAP.PP.KD'),
    start_date = start_year,
    end_date = end_year,
    country = c(unique(valuation$reference_income_level), 'World')) %>%
    select(reference_income_level = iso3c,
           year = date,
           gdp_current,
           gdp_constant2015,
           gdp_ppp_current,
           gni_ppp_current,
           gdp_ppp_intl2017,
           gni_ppp_intl2017)

  # Create one for California
  #TODO improve
  cal_vs_us <- 79 / 65 # https://datausa.io/profile/geo/california
  gdp_gni_cal <- gdp_gni %>%
    filter(reference_income_level == 'USA') %>%
    mutate(reference_income_level = 'California',
           gdp_current = gdp_current * cal_vs_us,
           gdp_constant2015 = gdp_constant2015 * cal_vs_us,
           gdp_ppp_current = gdp_ppp_current * cal_vs_us,
           gni_ppp_current = gni_ppp_current * cal_vs_us,
           gdp_ppp_intl2017 = gdp_ppp_intl2017 * cal_vs_us,
           gni_ppp_intl2017 = gni_ppp_intl2017 * cal_vs_us,
           )
  gdp_gni <- bind_rows(gdp_gni, gdp_gni_cal)

  gdp_world_2017_current <- as.numeric(gdp_gni[gdp_gni$reference_income_level == 'WLD' & gdp_gni$year == 2017,] %>%
                                         pull(gdp_current))
  gdp_world_2017_constant2015 <- as.numeric(gdp_gni[gdp_gni$reference_income_level == 'WLD' & gdp_gni$year == 2017,] %>%
                                              pull(gdp_constant2015))
  gdp_ppp_world_2017_intl2017 <- as.numeric(gdp_gni[gdp_gni$reference_income_level == 'WLD' & gdp_gni$year == 2017,] %>%
                                              pull(gdp_ppp_intl2017))
  gni_ppp_world_2017_intl2017 <- as.numeric(gdp_gni[gdp_gni$reference_income_level == 'WLD' & gdp_gni$year == 2017,] %>%
                                              pull(gni_ppp_intl2017))

  valuation_final <- valuation %>%
    # left_join(cpi_all, by=c('reference_income_level', 'year')) %>%
    left_join(gdp_gni) %>%
    mutate(valuation_refyear = case_when(
      gni_or_gdp == 'gni' & ppp ~ valuation_usd * gni_ppp_intl2017 / gni_ppp_current,
      gni_or_gdp == 'gdp' & ppp ~ valuation_usd * gdp_ppp_intl2017 / gdp_ppp_current,
      gni_or_gdp == 'gdp' & !ppp ~ valuation_usd *  gdp_constant2015 / gdp_current,
      T ~ NA_real_ # Other cases not yet supported
      )) %>%
    mutate(ratio = case_when(
      gni_or_gdp == 'gni' & ppp ~ gni_ppp_world_2017_intl2017 / gni_ppp_intl2017,
      gni_or_gdp == 'gdp' & ppp ~ gdp_ppp_world_2017_intl2017 / gdp_ppp_intl2017,
      gni_or_gdp == 'gdp' & !ppp ~ gdp_world_2017_constant2015 / gdp_constant2015,
      T ~ NA_real_ # Other cases not yet supported
    )) %>%
    mutate(valuation_world_2017 = as.numeric(valuation_refyear * ratio^elasticity)) %>%
    select(Outcome, Outcome_name, currency, unit,
           elasticity, gni_or_gdp, ppp, valuation_world_2017) %>%
    filter(!is.na(valuation_world_2017))

  if(nrow(raw) != nrow(valuation)) {stop('Things went wrong')}

  write_csv(valuation_final, output_file)
  return(valuation_final)
}
