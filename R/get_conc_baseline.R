#' Get baseline concentrations for various species
#'
#' @param species
#' @param grid_raster
#' @param no2_targetyear
#'
#' @return a tibble with `species` (chr) and named `conc_baseline` (RasterLayer) columns
#' @export
#'
#' @examples
get_conc_baseline <- function(species, grid_raster,
                                       no2_targetyear = 2019,
                                       pm25_to_pm10_ratio = .7) {
  avail_species <- c('no2', 'so2', 'pm25', 'tpm10', 'o3', 'o3_8h') # pollutants with available baseline

  species <- species %>% tolower() %>% subset(. %in% avail_species)

  conc <- lapply(species, function(spec) {
    if(spec == 'no2') {
      get_conc_baseline_no2(grid_raster = grid_raster,
                            no2_targetyear = no2_targetyear)
    } else if(spec == 'so2'){
      grid_raster %>% setValues(10)
    } else if(spec == 'tpm10'){
      get_conc_baseline_pm25(target_year = no2_targetyear, grid_raster = grid_raster) %>%
        multiply_by(1 / pm25_to_pm10_ratio)
    } else if(spec == 'pm25'){
      get_conc_baseline_pm25(target_year = no2_targetyear, grid_raster = grid_raster)
    } else if(spec %in% c('o3', 'o3_8h')){
      get_conc_baseline_o3(grid_raster = grid_raster, species = spec)
    }
  }) %>% `names<-`(species)

  tibble(species = names(conc),
         conc_baseline = conc)
}
