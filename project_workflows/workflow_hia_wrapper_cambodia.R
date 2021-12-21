# library(remotes)
# remotes::install_github("energyandcleanair/creahia")
# devtools::install_github('energyandcleanair/creahia')
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creahia)
library(creapuff)
# For development only
library(raster)
library(plyr)
library(readxl)
library(zoo)
library(magrittr)


# Parameters ####################################################################################
# ============================= Project specific ================================================
# Select macro-scenario
# scenario_prefix <- "ScAll" ; scenario_description='Operating&Proposed' # All stations : operating and proposed
scenario_prefix <- "ScB" ; scenario_description='Operating'          # Currently operating

# project_dir="G:/chile"        # calpuff_external_data persistent disk (project data)
project_dir="H:/cambodia"       # calpuff_external_data-2 persistent disk (project data)
output_dir <- file.path(project_dir,"calpuff_suite") # Where to write all generated files

# ================================ General ======================================================
gis_dir <- "F:/gis"                    # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# HIA ###########################################################################################
# Load CALMET parameters
calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# get_grid_from_calpuff.inp <- function(scenarios=NULL,
#                                       dir="C:/CALPUFF/CALPUFF_v7.2.1_L150618",
#                                       filename_suffix="_CALPUFF_7.0.inp",
#                                       file_paths=file.path(dir, paste0(scenarios, filename_suffix)),
#                                       params_to_read = c('IUTMZN','UTMHEM','XORIGKM','YORIGKM','DGRIDKM','NX','NY'),
#                                       ...) {
#   if(is.null(scenarios)) scenarios <- file_path %>% basename %>% gsub(filename_suffix, '', .)
#   file_paths %>%
#     lapply(creapuff::readPuffInp, ...) %>%
#     lapply('[', params_to_read) %>%
#     lapply(data.frame) %>% bind_rows %>% tibble(scenario=scenarios, .)
# }
#

# 01: Get coal additional concentrations from CALPUFF -------------------------------------------
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".csv"), gasunit = 'ug', dir=output_dir, hg_scaling=1)
# scenarios = unique(calpuff_files$scenario)
scenario=scenario_prefix
calpuff_files$scenario = scenario

# ================================ Define grids =================================================
# gridparams <- get_grid_from_calpuff.inp(scenarios[1:3]) %>% distinct(across(contains('UTM')))
# grids <- get_grids_calpuff(calpuff_files,
#                            utm_zone = as.numeric(gridparams$IUTMZN),
#                            utm_hem = gridparams$UTMHEM,
#                            map_res=1)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

# ============================ Select pollutants ================================================
calpuff_files %>%
  # filter(period=='annual' | !is.na(threshold)) %>%
  filter(period=='annual') %>%
  filter( speciesName=='NO2' | speciesName=='PM2.5'  | speciesName=='SO2') %>% make_tifs(grids = grids)

# =============================== Get Perturbation Raster ========================================
conc_perturbation <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".tif"), gasunit = 'ug', dir=output_dir)  %>%
  # filter(period=='annual' | !is.na(threshold)) %>%
  filter(period=='annual') %>%
  filter( speciesName=='NO2' | speciesName=='PM2.5'  | speciesName=='SO2') %>%
  dplyr::select(species, path, scenario) %>%
  tibble()

conc_perturbation$conc_perturbation <- lapply(conc_perturbation$path, raster)
conc_perturbation$scenario = scenario_prefix
species = unique(conc_perturbation$species)
names(conc_perturbation$conc_perturbation)=species
grid_raster = conc_perturbation$conc_perturbation[[1]] %>% raster


# 02: Get base concentration levels -------------------------------------------------------------
conc_base <- get_conc_baseline(species=species, grid_raster=grid_raster, no2_targetyear = 2020) # NULL ?


# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
regions <- get_adm(grid_raster, admin_level=0, res="full", iso3s=NULL)
# Input parameters:
#
# admin_level=2 -> Highest degree of res.
# admin_level=1 -> Main regions.
# admin_level=0 -> States
#
# res=null/""full -> Highest res of GADM file
# res="low"       -> Medium res
# res="coarse"    -> Lowest res
#
# iso3s=c("IDN", "KHM", "LAO", "VNM", "THA")


# 04: HIA Calculations:
# TODO : add no2_targetyear as input parameter in the wrapper, to calculate conc_base in the wrapper?
# TODO : change name no2_targetyear -> no2_target_year
# TODO : change name scale_base_year -> pop_base_year
# TODO : change value for scale_base_year from 2020 -> 2019, as default in wrapper
# TODO : change name scale_target_year -> pop_target_year
hia <-  wrappers.compute_hia_two_images(conc_perturbation$conc_perturbation,       # perturbation_rasters=raster::stack(perturbation_map)
                                        baseline_rasters=conc_base$conc_baseline,  # baseline_rasters=raster::stack(who_map)
                                        regions=regions,
                                        # administrative_level=0,    # Overridden by : regions --> get_adm
                                        # administrative_res="low",  # Overridden by : regions --> get_adm
                                        # administrative_iso3s=NULL, # Overridden by : regions --> get_adm
                                        scenario_name=scenario_prefix,
                                        scale_base_year=2019,        # Population base year : reference year of INPUT data, for total epidemiological and total population
                                        scale_target_year=2025,      # Population target year
                                        crfs_version="default",      # crfs_version="C40"
                                        epi_version="default",       # epi_version="C40"
                                        valuation_version="default") # valuation_version="viscusi"
saveRDS(hia, file.path(project_dir, paste0('hia','_',scenario_prefix,'.RDS')))
# hia <- readRDS(file.path(project_dir, paste0('hia','_',scenario_prefix,'.RDS')))


# 05: Create tables -----------------------------------------------------------------------------
hia_table <- hia %>% totalise_hia()

# Table by regions (admin area)
# hia_table_by_region <- hia_table %>%
#   group_by(region_id, region_name, iso3, scenario, cause, cause_name, unit, pollutant) %>%
#   summarise_if(is.numeric, sum) %>%
#   write_csv(file.path(project_dir, paste0('hia_totals_by_region','_',scenario_prefix,'.csv')))

# Total by country
hia_table_by_country <- hia_table %>%
  group_by(iso3, scenario, cause, cause_name, unit, pollutant) %>%
  summarise_if(is.numeric, sum) %>%
  write_csv(file.path(project_dir, paste0('hia_totals_by_country','_',scenario_prefix,'.csv')))

# Total over the entire domain
hia_table_full_domain <- hia_table %>%
  group_by(scenario, cause, cause_name, unit, pollutant) %>%
  summarise_if(is.numeric, sum) %>%
  write_csv(file.path(project_dir, paste0('hia_totals_full_domain','_',scenario_prefix,'.csv')))


# 06: Compute and extract economic costs --------------------------------------------------------
# TODO : change name scale_target_year -> pop_target_year
econ_costs <- hia %>% sel(-any_of('Deaths_Total')) %>%
  group_by(iso3, scenario, estimate) %>% summarise_if(is.numeric, sum, na.rm=T) %>%
  compute_econ_costs(results_dir=project_dir,
                     pop_targetyr=2025,  # Same as scale_target_year
                     projection_years=2025:2054,
                     iso3s_of_interest=NULL,
                     suffix=paste0("_",scenario_prefix),
                     valuation_version="default")

econ_costs$cost_forecast %>%
  group_by(estimate, Outcome_long, Cause_long, Pollutant, scenario, iso3) %>%
  summarise_if(is.numeric, sum) ->
  hia_cumu_by_country

econ_costs$cost_forecast %>%
  group_by(estimate, Outcome_long, Cause_long, Pollutant, scenario) %>%
  summarise_if(is.numeric, sum) ->
  hia_cumu_full_domain

hia_cumu_by_country %>% write_csv(file.path(project_dir, paste0('hia_cumulative_by_country','_',scenario_prefix,'.csv')))
hia_cumu_full_domain %>% write_csv(file.path(project_dir, paste0('hia_cumulative_full_domain','_',scenario_prefix,'.csv')))



