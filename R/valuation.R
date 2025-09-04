# These are a series of functions to help transfer valuations from the
# reference year/location to any location/year of our choice

#' Get valuations for specific countries and years
#'
#' @param valuation_version Character string specifying the valuation version (e.g., 'viscusi')
#' @param iso3s Character vector of ISO3 country codes
#' @param years Numeric vector of years
#'
#' @return Data frame with transferred valuations. Columns are
#' @export
get_valuations <- function(valuation_version = 'viscusi',
                          iso3s = NULL,
                          years = NULL) {

  # Step 1: Take raw valuation
  raw_valuation <- get_raw_valuation(valuation_version)

  # Step 2: Attach reference income
  valuation_with_ref <- attach_reference_income(raw_valuation)

  # Step 3: Attach target income
  valuation_with_target <- attach_target_income(valuation_with_ref, iso3s, years)

  # Step 4: Compute transferred valuation
  final_valuation <- compute_transferred_valuation(valuation_with_target)

  # Step 5: Only keep necessary columns
  final_valuation <- final_valuation %>%
    select(Outcome, iso3, year, valuation_usd, lcu_per_usd, gdp_curr_usd)

  return(final_valuation)
}

#' Get raw valuation data
#'
#' @param valuation_version Character string specifying the valuation version
#'
#' @return Raw valuation data
get_raw_valuation <- function(valuation_version) {
  read_csv(get_hia_path(paste0("valuation_", valuation_version, "_raw.csv")))
}

#' Attach reference income data
#'
#' @param raw_valuation Processed raw valuation data
#'
#' @return Valuation data with reference income attached
attach_reference_income <- function(raw_valuation) {

  # Add valuation_usd_reference column
  valuation <- transfer_to_usd(raw_valuation)

  # Get income reference levels
  income <- get_income()

  # Add local reference levels (e.g., California, OECD)
  income <- add_local_reference_levels(income)

  # Add reference column
  valuation <- valuation %>%
    mutate(
      income_type = case_when(
        gni_or_gdp == "gdp" & !ppp ~ "GDP.PC.currUSD",
        gni_or_gdp == "gdp" & ppp ~ "GDP.PC.PPP.currUSD",
        gni_or_gdp == "gni" & !ppp ~ "GNI.PC.currUSD",
        gni_or_gdp == "gni" & ppp ~ "GNI.PC.PPP.currUSD",
      )
    )

  valuation_with_income <- valuation %>%
    left_join(income %>% select(iso3, year, income_type, income_reference = value),
              by=c("reference_income_level"="iso3", "year", "income_type"))

  if(any(is.na(valuation_with_income$income_reference))){
    warning("Some reference income levels could not be matched. Check 'reference_income_level', 'year', and 'income_type' columns.")
  }

  return(valuation_with_income)
}

add_local_reference_levels <- function(income){

  # California
  cal_vs_us <- 79 / 65 # https://datausa.io/profile/geo/california
  income_cal <- income %>%
    filter(iso3 == 'USA') %>%
    mutate(iso3 = 'California') %>%
    # For Per Capita numbers i.e. starting with GDP.PC or GNI.PC, multiply by ratio
    # For total, replace with NA just in case
    mutate(
      value = case_when(
        # Per Capita (PPP or not, GDP or GNI
        stringr::str_starts(income_type, c("GDP\\.PC|GNI\\.PC")) ~ value * cal_vs_us,
        TRUE ~ NA
    ))

  income <- bind_rows(income, income_cal)

  # OECD
  # OECD is simply "OED" in WB data
  income_oed <- income %>%
    filter(iso3 == 'OED') %>%
    mutate(iso3 = 'OECD')
  income <- bind_rows(income, income_oed)

  return(income)
}

transfer_to_usd <- function(raw_valuation){

  exchange_data <- raw_valuation %>%
    filter(currency != 'USD') %>%
    distinct(currency, year)

  # Use hardcoded exchange rates for common cases
  exchange <- get_hardcoded_exchange_rates(exchange_data)

  # If any currencies/years are missing, fall back to priceR API
  missing_exchange <- exchange_data %>%
    anti_join(exchange, by = c("currency", "year"))

  if(nrow(missing_exchange) > 0) {
    message("Fetching missing exchange rates from API for: ",
            paste(paste(missing_exchange$currency, missing_exchange$year, sep=" "), collapse=", "))

    api_exchange <- get_api_exchange_rates(missing_exchange)
    exchange <- bind_rows(exchange, api_exchange)
  }

  valuation <- raw_valuation %>%
    left_join(exchange) %>%
    mutate(exchange = case_when(currency == 'USD' ~ 1, T ~ exchange)) %>%
    mutate(valuation_usd_reference = valuation * exchange) %>%
    mutate(reference_income_level = case_when(
      reference_income_level == 'OECD' ~ 'OED',
      T ~ reference_income_level
    ))

  # Quick checks
  stopifnot(nrow(valuation) == nrow(raw_valuation))
  stopifnot(!any(is.na(valuation$valuation_usd_reference)))

  return(valuation)
}

#' Get hardcoded exchange rates for common currencies and years
#'
#' @param exchange_data Data frame with currency and year columns
#'
#' @return Data frame with hardcoded exchange rates
get_hardcoded_exchange_rates <- function(exchange_data) {

  # Hardcoded exchange rates for common cases
  hardcoded_rates <- data.frame(
    currency = c("GBP", "EUR"),
    year = c(2018, 2010),
    exchange = c(1.334946, 1.326974)
  )

  # Filter to only include the requested currency/year combinations
  exchange_data %>%
    inner_join(hardcoded_rates, by = c("currency", "year"))
}

#' Get exchange rates from priceR API for missing currencies/years
#'
#' @param missing_exchange Data frame with missing currency and year combinations
#'
#' @return Data frame with API-fetched exchange rates
get_api_exchange_rates <- function(missing_exchange) {

  exchange <- data.frame()
  for (i in 1:nrow(missing_exchange)) {
    curr <- missing_exchange$currency[i]
    yr <- missing_exchange$year[i]

    rates <- priceR::historical_exchange_rates(curr, to = "USD",
                                       start_date = sprintf("%s-01-01", yr),
                                       end_date = sprintf("%s-12-31", yr))

    exchange <- bind_rows(exchange,
                          data.frame(currency = curr,
                                     year = yr,
                                     exchange = mean(rates[,2], na.rm = TRUE)))

    # Sleep for 1 second to avoid hitting API limits
    Sys.sleep(1)
  }

  return(exchange)
}

#' Attach target income data
#'
#' @param valuation_with_ref Valuation data with reference income
#' @param iso3s Character vector of ISO3 country codes
#' @param years Numeric vector of years
#'
#' @return Valuation data with both reference and target income
attach_target_income <- function(valuation_with_ref, iso3s, years) {

  # Get income data for target countries and years
  income <- get_income()

  # Add local reference levels (e.g., California, OECD)
  income <- add_local_reference_levels(income)

  # Filter to requested countries and years
  target_income <- income %>%
    filter(iso3 %in% iso3s, year %in% years) %>%
    select(iso3, year, income_type, income_target = value)

  # Join target income to valuation data
  valuation_with_target <- valuation_with_ref %>%
    select(Outcome,
           income_type,
           year_reference=year,
           income_reference,
           valuation_usd_reference,
           ppp,
           elasticity) %>%
    left_join(target_income,
              by = c("income_type"),
              relationship = "many-to-many"
              )

  # Add PPP conversion factor and lcu_per_usd for target countries/years
  # and gdp total to compute %gdp later on
  additional_infos <- income %>%
    filter(income_type %in% c("PPP.convLCUUSD", "GDP.PC.currLCU", "GDP.PC.currUSD", "GDP.TOT.currUSD")) %>%
    spread(income_type, value) %>%
    mutate(
      lcu_per_usd = GDP.PC.currLCU / GDP.PC.currUSD,
      ppp_conversion_factor = PPP.convLCUUSD,
      gdp_curr_usd = GDP.TOT.currUSD
    ) %>%
    select(iso3, year, lcu_per_usd, ppp_conversion_factor, gdp_curr_usd)

  valuation_with_target <- valuation_with_target %>%
    left_join(additional_infos, by = c("iso3", "year"))

  # Check for missing target income data
  missing_target <- valuation_with_target %>%
    filter(is.na(income_target)) %>%
    distinct(Outcome)

  if(nrow(missing_target) > 0) {
    warning("Missing target income data for: ",
            paste(missing_target$Outcome, collapse=", "))
  }

  return(valuation_with_target)
}

#' Compute transferred valuation
#'
#' @param valuation_with_target Valuation data with both reference and target income
#'
#' @return Final transferred valuation data
compute_transferred_valuation <- function(valuation_with_target) {

  # Apply income elasticity to transfer valuation from reference to target
  final_valuation <- valuation_with_target %>%
    mutate(
      # Apply elasticity scaling: V_target = V_reference * (income_ratio)^elasticity
      valuation_usd = valuation_usd_reference * (income_target / income_reference)^elasticity,

      # Convert to market prices if needed
      valuation_usd = if_else(ppp, valuation_usd * ppp_conversion_factor / lcu_per_usd, valuation_usd)
    )

  # Validation checks
  if(any(is.na(final_valuation$valuation_usd))) {
    warning("Some transferred valuations could not be calculated. Check income data and elasticity values.")
  }

  return(final_valuation)
}