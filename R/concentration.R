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
#' @param grid_raster
#' @param no2_min_incr
#' @param no2_targetyear
#'
#' @return
#' @export a tibble with `species` (chr) and `conc_baseline` (RasterLayer) columns
#'
#' @examples
get_conc_baseline <- function(species,
                              grid_raster,
                              no2_min_incr = NULL,
                              no2_targetyear = 2019) {

  conc = list()

  if("pm25" %in% species) {
    # source of PM2.5 data: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
    conc[["pm25"]] <- creahelpers::get_concentration_path('GlobalGWRcwUni_PM25_GL_201601_201612-RH35_Median.nc') %>%
      raster %>%
      creahelpers::cropProj(grid_raster)
  }

  if("no2" %in% species) {
    # source of NO2 data: https://data.world/datasets/no2
    conc_no2 <- creahelpers::get_concentration_path('no2_agg8.grd') %>%
      raster %>%
      creahelpers::cropProj(grid_raster) %>%
      multiply_by(1.88)

    # adjust NO2 concentrations using OMI data for 2011 and the target year
    if(!is.null(no2_targetyear)) {
      no2_11 <- creahelpers::get_concentration_path("no2_omi_2011.tif") %>%
        raster %>%
        creahelpers::cropProj(grid_raster)
      no2_targetyr <- creahelpers::get_concentration_path(paste0("no2_omi_", no2_targetyear, ".tif")) %>%
        raster %>%
        cropProj(grid_raster)
      no2_11_smooth <- no2_11 %>%
        focal(focalWeight(., 100, "circle"), mean, na.rm = T, pad = T, padValue = NA)
      no2_targetyr_smooth <- no2_targetyr %>%
        focal(focalWeight(., 100, "circle"), mean, na.rm = T, pad = T, padValue = NA)
      no2_ratio <- no2_targetyr_smooth / no2_11_smooth

      if(!is.null(no2_min_incr)) {
        no2_ratio <- no2_ratio %>% max(no2_min_incr)
      }

      conc_no2 <- conc_no2 %>% multiply_by(no2_ratio)
    }

    conc_no2[] <- conc_no2[] %>% na.approx(maxgap=5, na.rm=F)
    conc[["no2"]] <- conc_no2
  }

  if("o3" %in% species) {
    #source of NO2 data: https://data.world/datasets/no2
    conc[["o3"]] <- creahelpers::get_concentration_path('O3_77e3b7-xmessy_mmd_kk.nc') %>%
      raster %>%
      creahelpers::cropProj(grid_raster)
  }

  if("so2" %in% species) {
    conc[["so2"]] <- grid_raster %>% `values<-`(10) # why all the same value?
  }

  tibble(species = names(conc),
         conc_baseline = conc)
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
extract_concs_at_regions <- function(concs, regions, species) {

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
      stack

    conc_map[[scenario]] <- raster::extract(concs_stack, regions)
    names(conc_map[[scenario]]) <- regions$region_id
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

add_pop <- function(concs, grid_raster) {
  concs$pop <- list(get_pop(grid_raster))
  return(concs)
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
