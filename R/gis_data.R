get_pop <- function(grid_raster){
  f <- creahelpers::get_population_path('gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif')
  if(!file.exists(f)){
    stop(sprintf("Can't find population file: %s", f))
  }

  pop_density <- raster(f) %>%
    cropProj(grid_raster, expand=1)
  pop_density[is.na(pop_density)] <- 0
  # pop_density %<>% raster::aggregate(8, mean) %>%
  #   cropProj(grid_raster, expand=1)
  pop <- pop_density %>% multiply_by(area(pop_density))
  return(pop)
}

get_pop_proj <- function(){
  creahelpers::get_population_path('WPP2019_population-death_rate-birth_rate.csv') %>%
    read_csv() %>%
    mutate(deaths=pop*death_rate)
}

get_grump <- function(grid_raster){
  creahelpers::get_landcover_path('GRUMPv1/glurextents.bil') %>%
    raster %>%
    cropProj(grid_raster, expand=1)
}

