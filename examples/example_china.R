library(terra)
library(creahelpers)
library(dplyr)
library(creahia)
debugSource("~/development/crea/creahia/R/hia_computation.R")
maps <- readRDS("~/development/crea/studies/202312_china_aq_plan_hia/tmp/maps.RDS")
maps <- maps %>% creahelpers::to_rast()
scenario = "2020"

map_baseline <- readRDS("~/development/crea/studies/202312_china_aq_plan_hia/tmp/map_baseline.RDS") %>%
  creahelpers::to_rast()

hia <- creahia::wrappers.compute_hia_two_images.default(
  perturbation_rasters = list("pm25" = maps[[scenario]]),
  baseline_rasters = list("pm25" = map_baseline),
  administrative_iso3s = "CHN",
  administrative_level = 1,
  scenario_name = scenario,
  crfs_version = "C40",
  epi_version = "gbd2019"
)
