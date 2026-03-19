#' Build concentration dataset from CALPUF results
#'
#' CALPUFF typically exports results in a series of .csv files. This function leverages `creapuff` package to transform
#' these .csv files into .tiff that correspond to additional sources for instance.
#'
#' @param calpuff_dir
#' @param calpuff_files
#' @param utm_zone
#' @param utm_hem 'N' or 'S'
#' @param map_res in kilometers
#'
#' @return a tibble with three columns: `scenario` (chr), `species` (chr), `conc_perturbation` (RasterLayer)
#' @export
#'
#' @examples
get_conc_calpuff <- function(calpuff_dir = NULL, calpuff_files = NULL, utm_zone,
                             utm_hem, map_res, ...) {

  if(is.null(calpuff_files)) {
    calpuff_files <- creapuff::get_calpuff_files(dir = calpuff_dir)
  }

  grids <- creapuff::get_grids_calpuff(calpuff_files = calpuff_files, utm_zone = utm_zone,
                                       utm_hem = utm_hem, map_res = map_res)

  # Create tifs from csv results
  creapuff::make_tifs(calpuff_files, grids = grids, ...)

  # Specify function that returns the concentration grid for a specific scenario and pollutant
  calpuff_files = creapuff::get_calpuff_files(ext='\\.tif$', dir = unique(dirname(calpuff_files$path)))

  calpuff_files %>%
    filter(period == 'annual') %>%
    sel(scenario, species) %>%
    rowwise() %>%
    dplyr::mutate(
      conc_perturbation = list(creapuff::get_conc_raster(calpuff_files, scenario, species))
    )
}


#' Get baseline concentrations for various species
#'
#' @param species
#' @param folder
#'
#' @return a tibble with `species` (chr) and named `conc_baseline` (RasterLayer) columns
#' @export
#'
#' @examples
get_conc_baseline_manual <- function(species, folder) {
  species <- pollutants_to_process %>% tolower()

  files <- list.files(folder, full.names = T)

  conc <- lapply(species, function(spec){
    file <- files[files %>% tolower() %>% str_detect(spec)]
    rast(file)
  }) %>% `names<-`(species)

  tibble(species = names(conc),
         conc_baseline = conc)
}


#' Get PM2.5 baseline concentration for a given year and grid
#'
#' Delegates to creaexposure::get_concentration().
#'
#' @param target_year
#' @param grid_raster
#'
#' @return SpatRaster
#' @export
get_conc_baseline_pm25 <- function(target_year = lubridate::year(lubridate::today()),
                                   grid_raster){
  r <- creaexposure::get_concentration("pm25", source = "vandonkelaar", year = target_year,
                                       grid_raster = grid_raster)
  .validate_unit(r, "µg/m3")
  r
}


get_baseline_pm25_year <- function(year){
  creaexposure::get_concentration_closest_year("pm25", source = "vandonkelaar", year = year)
}


#' Get O3 baseline concentration for a given year and grid
#'
#' Delegates to creaexposure::get_concentration().
#'
#' @param grid_raster
#' @param species "o3" for M3M layer, "o3_8h" for SM8h layer
#'
#' @return SpatRaster
#' @export
get_conc_baseline_o3 <- function(grid_raster, species){
  variant <- if (species == "o3") "m3m" else "sm8h"
  creaexposure::get_concentration("o3", source = "geoschem", variant = variant,
                                  grid_raster = grid_raster)
}


#' Get NO2 baseline concentration for a given year and grid
#'
#' Delegates to creaexposure::get_concentration() with temporal scaling.
#'
#' @param grid_raster
#' @param no2_targetyear target year for temporal scaling via OMI ratio
#'
#' @return SpatRaster
#' @export
get_conc_baseline_no2 <- function(grid_raster, no2_targetyear){
  conc_no2 <- creaexposure::get_concentration(
    "no2", source = "larkin", grid_raster = grid_raster,
    scale_year = no2_targetyear
  )
  .validate_unit(conc_no2, "µg/m3")

  conc_no2 <- creahelpers::to_raster(conc_no2)
  conc_no2[] <- conc_no2[] %>% zoo::na.approx(maxgap = 5, na.rm = F)
  conc_no2
}


#' Extract concentration values and population at specified spatial features
#'
#' @param concs
#' @param map
#'
#' @return
#' @export
#'
#' @examples
extract_concs_and_pop <- function(concs, regions, species) {

  conc_map <- list()

  # One row per scenario
  for(i in seq(nrow(concs))) {

    scenario <- concs$scenario[i]

    cols_to_extract <- c(paste0("conc_scenario_", species),
                         paste0("conc_baseline_", species),
                         "pop") %>%
      intersect(names(concs))

    concs_stack <- concs[i, cols_to_extract] %>%
      purrr::transpose() %>%
      `[[`(1) %>%
      creahelpers::to_rast() %>%
      terra::rast()


    extracted <- terra::extract(concs_stack, terra::vect(regions), weights = T)
    # IMPORTANT: scale population to account for only partially covered pixels
    extracted$pop <- extracted$pop * extracted$weight
    extracted <- extracted %>% select(-weight)

    conc_map[[scenario]] <- split(extracted %>% select(-c(ID)), extracted$ID)
    ids <- names(conc_map[[scenario]]) %>% as.numeric()
    names(conc_map[[scenario]]) <- regions$region_id[ids]
  }

  return(conc_map)
}


sum_raster_columns <- function(x,y) {list(raster::stack(unlist(x)) + raster::stack(unlist(y)))}

combine_concs <- function(conc_perturbation, conc_baseline) {
  conc_perturbation %>%
    left_join(conc_baseline, by = c("species")) %>%
    filter(!is.null(conc_baseline)) %>%
    rowwise() %>%
    mutate(conc_scenario = sum_raster_columns(conc_perturbation, conc_baseline))
}

flatten_concs <- function(concs) {
  concs %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = species, values_from = -c(scenario, species))
}

add_pop <- function(concs, grid_raster, year_desired=2020) {
  concs$pop <- list(get_pop_count(grid_raster, year_desired = year_desired))
  return(concs)
}


.validate_unit <- function(r, expected_unit) {
  unit <- terra::units(r)
  if (length(unit) == 0 || unit == "") {
    stop(glue::glue(
      "No unit set on raster. Expected '{expected_unit}'. ",
      "Update creaexposure to a version that sets terra::units()."
    ))
  }
  # Compare raw bytes to avoid UTF-8 vs native encoding mismatches (e.g. µ character)
  if (!identical(charToRaw(unit), charToRaw(expected_unit))) {
    stop(glue::glue(
      "Unexpected unit: got '{unit}', expected '{expected_unit}'. ",
      "Check the source registry in creaexposure."
    ))
  }
}


get_conc_at_locations <- function() {

  #TODO
  #
  # #query concentration values for cities
  # cityConcs <- crop(cityUTM,gridR)
  #
  # files[queue] %>% stack %>% extract(cityConcs) %>% data.frame -> cc
  # names(cc) <- file_species$titletxt[queue] %>% gsub('\n', ' ', .)
  # cityConcs@data %<>% bind_cols(cc)
  #
  # cbind(cityConcs@coords,cityConcs@data) %>% write_csv(paste0("cityConcs.csv"))
}
