# Internal helpers -------------------------------------------------------

parse_uniform_exposures <- function(exposure_values) {
  if(is.null(exposure_values) || !is.list(exposure_values) || is.null(names(exposure_values))) {
    stop("exposure_values must be a named list, e.g. list(pm25 = list(baseline = 60, target = 0))", call. = FALSE)
  }

  parsed <- lapply(exposure_values, function(values) {
    if(!is.list(values) || !all(c("baseline", "target") %in% names(values))) {
      stop("Each pollutant must define a list with 'baseline' and 'target' entries", call. = FALSE)
    }
    list(baseline = values[["baseline"]], target = values[["target"]])
  })

  setNames(parsed, names(exposure_values))
}


parse_random_ranges <- function(exposure_ranges, target_default = 0) {
  if(is.null(exposure_ranges) || !is.list(exposure_ranges) || is.null(names(exposure_ranges))) {
    stop("exposure_ranges must be a named list, e.g. list(pm25 = list(min = 10, max = 30))", call. = FALSE)
  }

  parsed <- lapply(exposure_ranges, function(values) {
    if(!is.list(values) || !all(c("min", "max") %in% names(values))) {
      stop("Each pollutant must define a list with 'min' and 'max' entries", call. = FALSE)
    }
    list(min = values[["min"]],
         max = values[["max"]],
         target = if("target" %in% names(values)) values[["target"]] else target_default)
  })

  setNames(parsed, names(exposure_ranges))
}


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



#' Generate a HIA for uniform exposure perturbations in Bangladesh
#'
#' @param exposure_values Named list mapping pollutant names to baseline/target
#'   values. Each element can be a numeric vector `c(baseline, target)` or a
#'   list with elements `baseline` and `target`.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' generate_uniform_exposure_hia(
#'   exposure_values = list(
#'     pm25 = c(60, 15),
#'     no2 = list(baseline = 40, target = 20)
#'   )
#' )
#' }
generate_uniform_exposure_hia <- function(exposure_values = list(pm25 = list(baseline = 60, target = 0)),
                                          iso3 = "BGD",
                                          epi_version = "gbd2023",
                                          ...){

  library(terra)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over country with resolution 0.01deg
  iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')

  adm <- creahelpers::get_adm(level = 0, res = "low", iso2s = iso2)
  bbox <- sf::st_bbox(adm)
  res <- 0.01
  template_rast <- terra::rast(
    xmin=bbox$xmin,
    xmax=bbox$xmax,
    ymin=bbox$ymin,
    ymax=bbox$ymax,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  exposures <- parse_uniform_exposures(exposure_values)

  make_constant_raster <- function(value) {
    rast <- terra::rast(template_rast)
    rast[] <- value
    rast
  }

  baseline_rasters <- lapply(exposures, function(vals) make_constant_raster(vals$baseline))
  perturbation_rasters <- lapply(exposures, function(vals) make_constant_raster(vals$target - vals$baseline))
  baseline_rasters <- setNames(baseline_rasters, names(exposures))
  perturbation_rasters <- setNames(perturbation_rasters, names(exposures))

  creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = perturbation_rasters,
    baseline_rasters = baseline_rasters,
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
#' @param exposure_ranges Named list of pollutants with minimum/maximum (and
#'   optional target) values. Each element can be a numeric vector
#'   `c(min, max)` or `c(min, max, target)`, or a list containing `min`, `max`
#'   and optional `target`.
#' @param target_default Default target value used when a pollutant range does
#'   not specify one explicitly.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' generate_random_exposure_hias(
#'   levels = c(0, 1),
#'   exposure_ranges = list(
#'     pm25 = c(20, 60, 0),
#'     no2 = list(min = 15, max = 30, target = 5)
#'   )
#' )
#' }
generate_random_exposure_hias <- function(levels,
                                          exposure_ranges = list(pm25 = list(min = 10, max = 30)),
                                          target_default = 0,
                                          epi_version = "gbd2023",
                                          administrative_res = "full",
                                          iso3 = "BGD",
                                          res = 0.01,
                                          baseline_rast = NULL,
                                          ...
){

  library(terra)
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over country with resolution 0.01deg
  exposures <- parse_random_ranges(exposure_ranges, target_default)

  if(is.null(baseline_rast)){
    iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')

    # Debug before calling get_adm
    adm <- creahelpers::get_adm(level = 0, res = administrative_res, iso2s = iso2)
    bbox <- sf::st_bbox(adm)
    template_rast <- terra::rast(
      xmin=bbox$xmin,
      xmax=bbox$xmax,
      ymin=bbox$ymin,
      ymax=bbox$ymax,
      res=res,
      crs="+proj=longlat +datum=WGS84")

    make_random_raster <- function(min_val, max_val) {
      rast <- terra::rast(template_rast)
      rast[] <- runif(prod(dim(rast)), min_val, max_val)
      rast
    }

    baseline_rasters <- lapply(exposures, function(vals) make_random_raster(vals$min, vals$max))
  } else {
    if(inherits(baseline_rast, 'SpatRaster')) {
      if(length(exposures) != 1) {
        stop("Provide a named list of baseline rasters when simulating multiple pollutants", call. = FALSE)
      }
      template_rast <- baseline_rast
      baseline_rasters <- setNames(list(baseline_rast), names(exposures))
    } else if(is.list(baseline_rast)) {
      if(is.null(names(baseline_rast))) {
        stop("baseline_rast list must be named", call. = FALSE)
      }
      baseline_rasters <- baseline_rast[names(exposures)]
      if(length(baseline_rasters) != length(exposures)) {
        stop("baseline_rast list must match the number of pollutants", call. = FALSE)
      }
      template_rast <- baseline_rasters[[1]]
    } else {
      stop("baseline_rast must be a SpatRaster or a named list of SpatRaster objects", call. = FALSE)
    }
  }

  baseline_rasters <- setNames(baseline_rasters, names(exposures))
  perturbation_rasters <- mapply(function(vals, base_rast) {
    pert <- terra::rast(base_rast)
    pert[] <- vals$target - base_rast[]
    pert
  }, exposures, baseline_rasters, SIMPLIFY = FALSE)
  perturbation_rasters <- setNames(perturbation_rasters, names(exposures))

  names(baseline_rasters) <- names(exposures)
  names(perturbation_rasters) <- names(exposures)

  # Compute HIAs
  lapply(levels, function(level){
    creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = perturbation_rasters,
      baseline_rasters = baseline_rasters,
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
  library(dplyr)
  library(creahia)
  library(creaexposure)

  # Get PM2.5 exposure raster over country with resolution 0.01deg
  iso2 <- countrycode::countrycode(iso3, origin='iso3c', destination='iso2c')

  adm <- creahelpers::get_adm(level = 0, res = "low", iso2s = iso2)
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
