#' Alias for dplyr::select
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
sel <- dplyr::select


#' Title
#'
#' @param level
#' @param res
#' @param version
#'
#' @return
#' @export
#'
#' @examples
getadm <- function(level=0, res='full', version='36') {
  gis_dir <- creahia.env$gis_dir
  resext=''
  if(res!='full') resext=paste0('_', res)
  f <- file.path(gis_dir,
                   "boundaries",
                   paste0('gadm',version,'_',level,resext,'.RDS'))

  if(file.exists(f)) {
    readRDS(f)
  } else {
    raster::shapefile(gsub('\\.RDS','.shp',f),
                      encoding='UTF-8', use_iconv=TRUE)
  }
}

readWB <- function(inF, sheet='Data', skip=3, countries.only=T, ...) {
  read_xls(inF, sheet=sheet, skip=skip, ...) %>%
    gather(Year, Value, matches('[0-9]{4}')) %>% mutate_at('Year', as.numeric) %>%
    set_names(make.names(names(.))) %>%
    filter(!is.na(Value)) %>% arrange(-Year) %>% filter(!duplicated(Country.Code)) %>%
    rename(country=Country.Name, ISO3=Country.Code) %>%
    filter(!countries.only | ISO3 %in% WBcountries$ISO3)
}

getWBIndicators <- function(){
  wbstats::wb_indicators() %>% sel(Indicator.Code=indicator_id, Indicator.Name=indicator)
}

getWBCountries <- function(){
  wbstats::wb_countries() %>% sel(ISO3=iso3c, Region=region, IncomeGroup=income_level, country) %>%
    filter(IncomeGroup != 'Aggregates')
}


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


addiso <- function(df, ...) {
  df$ISO3 <- countrycode::countrycode(df$country, origin = 'country.name', destination = 'iso3c', ...)
  df$ISO3[grepl('Kosovo', df$country)] <- 'XKX'
  df$ISO3[grepl('Aland$', df$country)] <- 'FIN'
  df
}

gather_ihme <- function(df) {
  df %>% dplyr::rename(central=val, low=lower, high=upper) %>%
    tidyr::gather(estimate, val, central, low, high) %>%
    dplyr::mutate(measure_name = measure_name %>% gsub(' .*', '', .)) %>%
    dplyr::rename(country=location_name)
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



#' C
#'
#' @param grid_raster maps will be projected and croped on this raster
#' @param shp optional shapefile path that will serve as map
#' @param admin_level GADM level to consider
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_map_adm <- function(grid_raster, shp=NULL, admin_level=0, ...) {

  #TODO allow several levels at once

  crs_to <- CRS(proj4string(grid_raster))

  grid_4326 <- raster::projectExtent(grid_raster, crs(countriesLow)) %>% extend(c(40,40))

  adm_4326 <- if(is.null(shp)) {
     creahelpers::get_adm(admin_level, ...)
  } else shp

  adm_utm <- spTransform(crop(adm_4326, grid_4326), crs_to)

  maps <- adm_utm %>%
    sf::st_as_sf() %>%
    dplyr::rename_at(paste0(c("GID_","NAME_"), admin_level), ~c("region_id","region_name")) %>%
    dplyr::select(region_id, region_name, geometry)

  return(maps)
}


get_map_cities <- function(grid_raster){

  cities_4326 <- sf::read_sf(creahelpers::get_boundaries_path("citiesDistToLarge.shp")) %>%
    as("Spatial") %>%
    raster::crop(grid_4326)
  cities_utm <- spTransform(cities_4326, crs_to)

  cities_utm %>%
    sf::st_as_sf() %>%
    dplyr::mutate(region_id=name, region_name=name) %>%
    dplyr::select(region_id, region_name, geometry)
}


country_recode_iso3 <- function(iso3s, replacements=NULL) {

  if(is.null(replacements)){
    cl <- rworldmap::countriesLow
    cl$ADM0_A3[cl$SOV_A3 == 'GB1'] %>% as.character -> UK
    names(UK) <- rep('GBR', length(UK))
    use_as_proxy <- c(CHN='HKG', CHN='MAC', AUT='LIE', VUT='PLW', ITA='SMR')
    replacements <- c(IND='KAS', AUS='IOA', UK, use_as_proxy)
  }

  for(i in 1:length(replacements))
    iso3s[iso3s == replacements[i]] <- names(replacements)[i]
  return(iso3s)
}
