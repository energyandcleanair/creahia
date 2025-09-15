#' Generate random exposure raster
#'
#' @param min
#' @param max
#'
#' @return
#' @export
#'
#' @examples
generate_random_exposure <- function(min, max){
  res <- 0.01
  pm25 <- terra::rast(
    xmin=88,
    xmax=92,
    ymin=20,
    ymax=27,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  pm25[] <- runif(prod(dim(pm25)), min, max)
  return(pm25)
}



#' Generate a HIA for a uniform exposure perturbation in Bangladesh
#'
#' @param baseline
#' @param target
#' @param calc_causes
#'
#' @return
#' @export
#'
#' @examples
generate_uniform_exposure_hia <- function(baseline,
                                          target,
                                          iso3 = "BGD",
                                          epi_version = "gbd2019",
                                          ...){

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')
  adm <- creahelpers::get_adm(level=0, res="low", iso2s=iso2)
  bbox <- sf::st_bbox(adm)
  res <- 0.01
  baseline_rast <- terra::rast(
    xmin=bbox$xmin,
    xmax=bbox$xmax,
    ymin=bbox$ymin,
    ymax=bbox$ymax,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  baseline_rast[] <- baseline

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  perturbation_rast <- target-baseline_rast

  creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = perturbation_rast),
    baseline_rasters = list(pm25 = baseline_rast),
    scale_base_year = NULL, # Just to avoid unnecessary warning
    scale_target_year = NULL,  # Just to avoid unnecessary warning
    pop_year=2020,
    administrative_level = 0,
    administrative_res = "low",
    administrative_iso3s = iso3,
    epi_version = epi_version,
    ...
  )

}



#' Generate a HIA for a random exposure perturbation in Bangladesh
#' at multiple levels
#'
#' @param levels
#' @param min
#' @param max
#' @param target
#' @param calc_causes
#'
#' @return
#' @export
#'
#' @examples
generate_random_exposure_hias <- function(levels,
                                          min=20,
                                          max=60,
                                          target=0,
                                          epi_version = "gbd2019",
                                          administrative_res = "full",
                                          iso3 = "BGD",
                                          res = 0.01,
                                          baseline_rast = NULL,
                                          ...
){

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  if(is.null(baseline_rast)){
    iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')
    bbox <- creahelpers::get_adm(level=0, res="full", iso2s=iso2) %>% sf::st_bbox()
    baseline_rast <- terra::rast(
      xmin=bbox$xmin,
      xmax=bbox$xmax,
      ymin=bbox$ymin,
      ymax=bbox$ymax,
      res=res,
      crs="+proj=longlat +datum=WGS84")
    baseline_rast[] <- runif(prod(dim(baseline_rast)), min, max)
  }

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  perturbation_rast <- target-baseline_rast

  # Compute HIAs
  lapply(levels, function(level){
    creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = list(pm25 = perturbation_rast),
      baseline_rasters = list(pm25 = baseline_rast),
      scale_base_year = NULL, # Just to avoid unnecessary warning
      scale_target_year = NULL,  # Just to avoid unnecessary warning
      pop_year=2020,
      administrative_level = level,
      administrative_res = administrative_res,
      administrative_iso3s = iso3,
      epi_version = epi_version
    ) %>%
      mutate(level=level)
  }) %>%
    bind_rows()
}

#' Generate a HIA for van Donkelaar exposure map
#'
#' @param baseline
#' @param target
#' @param calc_causes
#'
#' @return
#' @export
#'
#' @examples
generate_donkelaar_exposure_hia <- function(target,
                                            rr_sources,
                                          iso3 = "BGD",
                                          epi_version = "gbd2021",
                                          ...){

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')
  adm <- creahelpers::get_adm(level=0, res="low", iso2s=iso2)
  bbox <- sf::st_bbox(adm)
  pop <- creaexposure::data.pop(res=creaexposure::RES_30_SEC, bbox = bbox)
  pm25 <- creaexposure::data.basemap_pm25(pop=pop, res=creaexposure::RES_30_SEC, year=2020)


  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  perturbation_rast <- target-pm25

  creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = perturbation_rast),
    baseline_rasters = list(pm25 = pm25),
    scale_base_year = NULL, # Just to avoid unnecessary warning
    scale_target_year = NULL,  # Just to avoid unnecessary warning
    pop_year=2020,
    administrative_level = 0,
    administrative_res = "low",
    administrative_iso3s = iso3,
    epi_version = epi_version,
    rr_sources = rr_sources,
    ...
  )

}
