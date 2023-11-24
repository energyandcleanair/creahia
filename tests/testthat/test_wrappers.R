library(testthat)

get_examples_dir <- function(){
  if(dir.exists("../../../examples")) "../../../examples" else "examples"
}

test_that("wrapper 2 images works", {

  dir <- get_examples_dir()
  perturbation_rasters <- list(
    "no2"=raster::raster(file.path(dir, "example_ph", "rank(0)_no2_8760hr_conc_opr_all.tif")),
    "pm25"=raster::raster(file.path(dir, "example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif"))
  )

  # Take a fraction to make computing faster
  crop_fraction <- function(rs, fact=8){
    bbox_org <- raster::bbox(rs[[1]])
    x_center <- (bbox_org[3] + bbox_org[1])/2
    y_center <- (bbox_org[4] + bbox_org[2])/2
    dx <- (bbox_org[3] - bbox_org[1])
    dy <- (bbox_org[4] - bbox_org[2])

    bbox_new <- list(
      xmin=x_center - dx/(fact*2),
      xmax=x_center + dx/(fact*2),
      ymin=y_center - dy/(fact*2),
      ymax=y_center + dy/(fact*2))

    lapply(rs,function(r){r %>% raster::crop(unlist(bbox_new))})
  }

  perturbation_rasters <- crop_fraction(perturbation_rasters)
  baseline_rasters <- lapply(perturbation_rasters, function(r){r[!is.na(r)] <- 0; r})

  hia <- creahia::wrappers.compute_hia_two_images(perturbation_rasters = perturbation_rasters,
                                           baseline_rasters = baseline_rasters,
                                           administrative_iso3s = "PHL",
                                           # crfs_version = "C40",
                                           # epi_version="C40",
                                           valuation_version="viscusi")

  dir.create("tmp")
  cost <- creahia::compute_econ_costs(hia=hia, results_dir = "tmp", crfs_version = "C40")

  future <-  creahia::fu(hia=hia, results_dir = "tmp", crfs_version = "C40")

})


plot_rasters <- function(rs,
                         add_adm=T,
                         adm_level=2){

  if(is.list(rs)){
    rs <- raster::stack(rs)
  }

  grid <- rs[[1]] %>% raster::raster()

  adm <- creahelpers::get_adm(adm_level, res="low") %>%
    creahelpers::cropProj(grid)


  pl <- rasterVis::levelplot(rs) +
    latticeExtra::layer(sp::sp.lines(adm, lwd=2, col='darkgray'),
          data=list(adm=adm))

  print(pl)
  return(pl)
}
