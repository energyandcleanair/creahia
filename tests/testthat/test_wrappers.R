library(testthat)

get_examples_dir <- function(){
  if(dir.exists("../../../examples")) "../../../examples" else "examples"
}

test_that("wrapper 2 images works", {

  dir <- get_examples_dir()
  perturbation_rasters <- list(
    "no2"=raster::raster(file.path(dir, "example_ph", "rank(0)_no2_8760hr_conc_opr_all.tif")),
    "pm25"=raster::raster(file.path(dir, "example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif")),
    "so2"=raster::raster(file.path(dir, "example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif")),
    "o3"=raster::raster(file.path(dir, "example_ph", "rank(0)_pm25_8760hr_conc_opr_all.tif"))
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

})

test_that("running average", {

  values <- seq(1,365)
  dates <- seq(lubridate::date("2018-01-01"),
               lubridate::date("2018-12-31"),
               by="days")

  df <- tibble::tibble(value=values, date=dates)

  # Shuffle it
  df <- df[sample(nrow(df)),]

  # 0 and 1 running width should have no effect (except day averaging at some point?)
  df_0 <- utils.rolling_average(df, average_by = "day", average_width = 0,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  expect_true(all(df_0 %>% dplyr::arrange(date) %>% dplyr::pull(value)
                  == df %>% dplyr::arrange(date) %>% dplyr::pull(value)))

  df_1 <- utils.rolling_average(df, average_by = "day", average_width = 1,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  expect_true(all(df_1 %>% dplyr::arrange(date) %>% dplyr::pull(value) == df_1 %>% dplyr::arrange(date) %>% dplyr::pull(value)))

  # 2
  df_2 <- utils.rolling_average(df, average_by = "day", average_width = 2,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  expect_true(is.na(df_2$value[1]))
  expect_true(all(df_2$value[2:365]==seq(1.5,364.5)))

  # 5
  df_5 <- utils.rolling_average(df, average_by = "day", average_width = 5,
                                vars_to_avg = "value", group_by_cols = NULL) %>%
    dplyr::arrange(date)
  all(is.na(df_5$value[1:4]))
  expect_true(all(df_5$value[5:365]==seq(3,363)))

  #  with NAs
  values <- c(1,2,3,NA,NA,NA,7,8,9)
  dates <- seq(lubridate::date("2018-01-01"),
               lubridate::date("2018-01-09"),
               by="days")
  df <- tibble::tibble(value=values, date=dates, group="1")
  utils.rolling_average(df, average_by = "day", average_width = 2,
                       vars_to_avg = "value", group_by_cols = "group",
                       min_values=0) %>%
    dplyr::arrange(date)

  utils.rolling_average(df, average_by = "day", average_width = 3,
                        vars_to_avg = "value", group_by_cols = "group",
                        min_values=2) %>%
    dplyr::arrange(date)




})
