readWB <- function(inF, sheet = 'Data', skip = 3, countries.only = T, ...) {
  read_xls(inF, sheet = sheet, skip = skip, ...) %>%
    gather(Year, Value, matches('[0-9]{4}')) %>%
    mutate_at('Year', as.numeric) %>%
    set_names(make.names(names(.))) %>%
    filter(!is.na(Value)) %>%
    arrange(-Year) %>%
    filter(!duplicated(Country.Code)) %>%
    rename(country = Country.Name, ISO3 = Country.Code) %>%
    filter(!countries.only | ISO3 %in% WBcountries$ISO3)
}


getWBIndicators <- function() {
  wbstats::wb_indicators() %>% sel(Indicator.Code = indicator_id, Indicator.Name = indicator)
}


getWBCountries <- function() {
  wbstats::wb_countries() %>%
    sel(ISO3 = iso3c, Region = region, IncomeGroup = income_level, country) %>%
    filter(IncomeGroup != 'Aggregates')
}


readWB_online <- function(indicator, start_date = 2010, end_date = 2019,
                          valuename = 'Value', latest.year.only = T, ...) {

  wb_indicators <- getWBIndicators()
  d <- wbstats::wb_data(indicator, start_date = start_date, end_date = end_date, ...)
  names(d)[names(d) == indicator] <- 'Value'
  d <- d %>%
    sel(iso3 = iso3c, country, year = date, Value) %>%
    filter(!is.na(Value)) %>%
    mutate(Indicator.Code = indicator) %>%
    right_join(wb_indicators, .)

  if(latest.year.only)
    d <- d %>% arrange(-year) %>% distinct(iso3, .keep_all = T)

  names(d)[names(d) == 'Value'] <- valuename
  if(nrow(d) == 0) stop('no data!')
  return(d)
}


addiso <- function(df, ...) {
  df$ISO3 <- countrycode::countrycode(df$country, origin = 'country.name', destination = 'iso3c', ...)
  df$ISO3[grepl('Kosovo', df$country)] <- 'XKX'
  df$ISO3[grepl('Aland$', df$country)] <- 'FIN'
  df
}


gather_ihme <- function(df) {
  df %>% dplyr::rename(central = val, low = lower, high = upper) %>%
    tidyr::gather(estimate, val, central, low, high) %>%
    dplyr::mutate(measure_name = measure_name %>% gsub(' .*', '', .)) %>%
    dplyr::rename(country = location_name)
}


ihme_getrate <- function(df) {
  df %>% left_join(pop.total %>% sel(location_id, pop = val)) %>%
    mutate(val = val / pop * 1e5) %>% sel(-pop) %>%
    ungroup %>%
    distinct
}


addlowhigh <- function(indata) ddply(indata, .(ISO3),
                                     function(df) {
                                       for(col in names(df))
                                         df[[col]] <- df[[col]] %>% na.fill(df[[col]][df$estimate == 'central'])
                                       return(df)
                                     })


make_ci <- function(df, rescols = c('low', 'central', 'high')) {
  df %>%
    mutate_at(rescols, scales::comma, accuracy = 1) %>%
    mutate(CI = paste0('(', low, ' - ', high, ')')) %>%
    sel(-low, -high) %>%
    gather(type, val, central, CI) %>%
    unite(var, scenario, type) %>%
    spread(var, val)
  }


make_nothing <- function(x) {x}


#' Get administrative areas within the model
#'
#' @param grid_raster projection destination and crop extent
#' @param shp optional shapefile path that will serve as map
#' @param admin_level GADM level to consider
#' @param ...
#'
#' @return a simple feature collection of administrative boundaries
#' @export
#'
#' @examples
get_model_adm <- function(grid_raster, shp = NULL, admin_level = 0, iso3s = NULL, ...) {

  crs_to <- CRS(proj4string(grid_raster))

  grid_4326 <- projectExtent(grid_raster, crs(rworldmap::countriesLow)) %>%
    extend(c(40, 40))

  adm_4326 <- if(is.null(shp)) {
    creahelpers::get_adm(admin_level, ...)
  } else {
    shp %>%
      {if(!is.null(iso3s)) adm_4326[adm_4326$GID_0 %in% iso3s,] else .}
  }

  adm_utm <- adm_4326 %>% crop(grid_4326) %>%
    spTransform(crs_to)

  maps <- adm_utm %>%
    sf::st_as_sf() %>%
    dplyr::rename_at(paste0(c("GID_", "NAME_"), admin_level), ~c("region_id", "region_name")) %>%
    dplyr::mutate(country_id = if(admin_level == 0) region_id else GID_0) %>%
    dplyr::select(region_id, region_name, country_id, geometry)

  return(maps)
}


get_map_cities <- function(grid_raster) {

  crs_to <- CRS(proj4string(grid_raster))

  cities_utm <- sf::read_sf(creahelpers::get_boundaries_path("citiesDistToLarge.shp")) %>%
    sf::st_transform(crs_to) %>%
    sf::st_crop(grid_raster)

  cities_utm %>%
    dplyr::mutate(region_id = name, region_name = name,
                  country_id = countrycode::countrycode(cntry_t, "country.name","iso3c")) %>%
    dplyr::select(region_id, region_name, country_id, geometry)
}


country_recode_iso3 <- function(iso3s, replacements = NULL) {

  if(is.null(replacements)) {
    cl <- rworldmap::countriesLow
    UK <- cl$ADM0_A3[cl$SOV_A3 == 'GB1'] %>% as.character
    names(UK) <- rep('GBR', length(UK))
    use_as_proxy <- c(CHN = 'HKG', CHN = 'MAC', AUT = 'LIE', VUT = 'PLW', ITA = 'SMR')
    replacements <- c(IND = 'KAS', AUS = 'IOA', UK, use_as_proxy)
  }

  for(i in 1:length(replacements))
    iso3s[iso3s == replacements[i]] <- names(replacements)[i]
  return(iso3s)
}


orderrows <- function(df) {
  rr.out <- df %>% apply(1, sort) %>% t
  colnames(rr.out) <- c('low', 'central', 'high')
  rr.out
}


#' Just to put a breakpoint here
#'
#' @param e
#'
#' @return
#' @export
#'
#' @examples
debug_and_stop <- function(e) {
  stop(e)
}


debug_and_warning <- function(e) {
  warning(e)
}
