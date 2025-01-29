# Population related functions
# This should all move to creahelpers at some point

#' Return years for which the population is available
#'
#' @return
#' @export
#'
#' @examples
get_pop_years_available <- function(){
  # extract year from gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif right after adjusted_to
  list.files(creahelpers::get_population_path(), pattern = '.*tif$') %>%
    stringr::str_extract('(?<=unwpp_country_totals_rev11_)[0-9]+') %>%
    .[!is.na(.)] %>%
    unique() %>%
    as.numeric()
}



#' Returns the closest available population year to the desired year
#'
#' @param desired_year
#'
#' @return
#' @export
#'
#' @examples
get_pop_year <- function(year_desired){
  years_available <- get_pop_years_available()
  years_available[which.min(abs(years_available - year_desired))]
}



#' Get population SpatRaster for the closest year available to the desired year
#'
#' @param grid_raster
#' @param year_desired
#'
#' @return
#' @export
#'
#' @examples
get_pop_count <- function(grid_raster, year_desired=2020) {

  year <- get_pop_year(year_desired)
  filename <- glue("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_{year}_30_sec.tif")
  filepath <- creahelpers::get_population_path(filename)
  if(!file.exists(filepath)) {
    stop(sprintf("Can't find population file: %s", filepath))
  }

  pop_density <- terra::rast(filepath)

  # Reproject if grid_raster provided
  if(!is.null(grid_raster)) pop_density <- project(pop_density, terra::rast(grid_raster))

  pop_density[is.na(pop_density)] <- 0

  # Getting pop in million per cell
  pop_mn <- pop_density %>% multiply_by(terra::cellSize(pop_density, unit='km', transform=T))

  # Add year attribute, to be used later for scaling
  attr(pop_mn, 'year') <- year
  return(pop_mn)
}


get_pop_proj <- function() {
  creahelpers::get_population_path('WPP2019_population-death_rate-birth_rate.csv') %>%
    read_csv(., col_types = cols()) %>%
    mutate(deaths = pop * death_rate) %>%
    dplyr::rename(iso3 = ISO3, year = Yr)
}
