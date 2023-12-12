get_pop <- function(grid_raster) {
  filepath <- creahelpers::get_population_path('gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif')
  if(!file.exists(filepath)) {
    stop(sprintf("Can't find population file: %s", filepath))
  }

  pop_density <- terra::rast(filepath) %>%
    project(rast(grid_raster))

  pop_density[is.na(pop_density)] <- 0
  pop <- pop_density %>% multiply_by(terra::cellSize(pop_density, unit='km')) #pop in million

  return(raster(pop))
}


get_grump <- function(grid_raster) {
  creahelpers::get_landcover_path('GRUMPv1/glurextents.bil') %>%
    raster %>%
    cropProj(grid_raster, expand = 1)
}

