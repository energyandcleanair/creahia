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
#' @return a tibble with `species` (chr) and named `conc_baseline` (RasterLayer) columns
#' @export
#'
#' @examples
get_conc_baseline <- function(species,
                              grid_raster,
                              no2_min_incr = NULL,
                              no2_targetyear = 2019,
                              pm25_to_pm10_ratio = .7) {
  avail_species <- c('no2', 'so2', 'pm25', 'tpm10', 'o3') # pollutants with available baseline
  file_list <- list(
    'o3' = 'O3_77e3b7-xmessy_mmd_kk.nc'
  )

  species <- species[tolower(species) %in% avail_species]

  conc <- lapply(species, function(spec) {
    if(spec == 'no2') {
      conc_no2 <- creahelpers::get_concentration_path('no2_agg8.grd') %>%
        raster %>%
        creahelpers::cropProj(grid_raster) %>%
        multiply_by(1.88)

      if(!is.null(no2_targetyear)) {
        no2_11 <- creahelpers::get_concentration_path("no2_omi_2011.tif") %>%
          raster %>%
          creahelpers::cropProj(grid_raster)
        no2_targetyr <- creahelpers::get_concentration_path(glue("no2_omi_{no2_targetyear}.tif")) %>%
          raster %>%
          creahelpers::cropProj(grid_raster)

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

      conc_no2[] <- conc_no2[] %>% na.approx(maxgap = 5, na.rm = F)
      conc_no2
    } else if(spec == 'so2') {
      grid_raster %>% setValues(10)
    } else if(spec == 'tpm10') {
      get_conc_baseline_pm25(target_year = no2_targetyear, grid_raster = grid_raster) %>%
        multiply_by(1 / pm25_to_pm10_ratio)
    } else if(spec == 'pm25'){
      get_conc_baseline_pm25(target_year = no2_targetyear, grid_raster = grid_raster)
    }

  }) %>% `names<-`(species)

  tibble(species = names(conc),
         conc_baseline = conc)
}


#' Get PM2.5 baseline concentration for a given year and grid
#'
#' @param target_year
#' @param grid_raster
#'
#' @return SpatRaster
#' @export
#'
#' @examples
get_conc_baseline_pm25 <- function(target_year = lubridate::year(lubridate::today()),
                                   grid_raster){
  basemap_year <- get_baseline_pm25_year(target_year) # get latest available basemap year

  pm25_nc <- if(basemap_year < 2018){
    glue("V5GL03.HybridPM25.Global.{basemap_year}01-{basemap_year}12.nc")
  } else {
    glue('V5GL04.HybridPM25.Global.{basemap_year}01-{basemap_year}12.nc')
  }
  creahelpers::get_concentration_path(pm25_nc) %>% rast %>%
    cropProj(grid_raster)
}


get_baseline_pm25_year <- function(year){
  basemap_years <- seq(2015, 2022)
  max(basemap_years[basemap_years<=year])
}


#' Get O3 baseline concentration for a given year and grid
#'
#' @param grid_raster
#'
#' @return SpatRaster
#' @export
#'
#' @examples
get_conc_baseline_o3 <- function(grid_raster, species){
  creahelpers::get_concentration_path('O3_77e3b7-xmessy_mmd_kk.nc') %>%
    rast %>%
    `[[`(if(species == 'o3') 'M3M_lev31_31=31' else 'SM8h_lev31_31=31') %>%
    creahelpers::cropProj(grid_raster)
}


#' Get NO2 baseline concentration for a given year and grid
#'
#' @param grid_raster
#' @param no2_targetyear
#'
#' @return SpatRaster
#' @export
#'
#' @examples
get_conc_baseline_no2 <- function(grid_raster, no2_targetyear, no2_min_incr){
  conc_no2 <- creahelpers::get_concentration_path('no2_agg8.grd') %>%
    raster %>%
    creahelpers::cropProj(grid_raster) %>%
    multiply_by(1.88)

  if(!is.null(no2_targetyear)) {
    no2_11 <- creahelpers::get_concentration_path("no2_omi_2011.tif") %>%
      raster %>%
      creahelpers::cropProj(grid_raster)
    no2_targetyr <- creahelpers::get_concentration_path(glue("no2_omi_{no2_targetyear}.tif")) %>%
      raster %>%
      creahelpers::cropProj(grid_raster)

    focal_d <- get_focal_d(grid_raster)

    no2_11_smooth <- no2_11 %>%
      focal(focalWeight(., focal_d, "circle"), mean, na.rm = T, pad = T, padValue = NA)
    no2_targetyr_smooth <- no2_targetyr %>%
      focal(focalWeight(., focal_d, "circle"), mean, na.rm = T, pad = T, padValue = NA)

    no2_ratio <- no2_targetyr_smooth / no2_11_smooth

    if(!is.null(no2_min_incr)) {
      no2_ratio <- no2_ratio %>% max(no2_min_incr)
    }

    conc_no2 <- conc_no2 %>% multiply_by(no2_ratio)
  }

  conc_no2[] <- conc_no2[] %>% na.approx(maxgap = 5, na.rm = F)
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
