

get_grump <- function(grid_raster) {
  creahelpers::get_landcover_path('GRUMPv1/glurextents.bil') %>%
    raster %>%
    cropProj(grid_raster, expand = 1)
}

