test_that("wrapper 2 images works", {

  dir <- get_examples_dir()
  perturbation_rasters <- list(
    "no2"=raster::raster(get_test_file(file.path("example_ph", "rank(0)_no2_8760hr_conc_opr_all.tif"))),
    "pm25"=raster::raster(get_test_file(file.path("example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif"))),
    "so2"=raster::raster(get_test_file(file.path("example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif"))),
    "o3"=raster::raster(get_test_file(file.path("example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif")))
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
                                           pop_year=2020
                                           )

  dir.create("tmp", showWarnings = F)
  cost <- creahia::compute_econ_costs(hia=hia,
                                      results_dir = "tmp",
                                      valuation_version="viscusi",
                                      projection_years=c(2020),
                                      GDP_scaling=T
                                      )

  # Really a minimal test...
  testthat::expect_gt(sum(cost$hia_cost$cost_mn_currentUSD, na.rm=T), 0)

})
