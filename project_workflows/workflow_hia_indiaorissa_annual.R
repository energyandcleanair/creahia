# library(remotes)
#remotes::install_github("energyandcleanair/creahia")
# devtools::install_github('energyandcleanair/creahia')
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))

# For development only
library(raster)
library(sf)
#library(plyr)
library(readxl)
library(zoo)
library(magrittr)
#library(tidyverse)
library(lubridate)

library(creahia)
library(creapuff)
require(rcrea)
require(creahelpers)

#list.files(path='R', full.names=T) %>% sapply(source)

project_dir="G:/indiaorissa"       # calpuff_external_data-2 persistent disk (project data)

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")


emis <- read_xlsx(file.path(emissions_dir, 'JSW_Orisha_steel_cropper_calc.xlsx'), sheet='CALPUFF input')

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('NO2', 'PM2.5', 'PM10', 'SO2')
pollutants_to_process=c('PM2.5')

# Load CALMET parameters
#calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

#list csvs
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)
grid_raster = grids$gridR

#make tifs
#scenarios_to_process=c('F1', 'F2', 'F3', 'D2', 'H2', 'F1.D2.H2')
#scenarios_to_process=c('F1.D2.H2')
#calpuff_files %>% filter(scenario %in% scenarios_to_process, period=='annual', speciesName %in% pollutants_to_process) %>% make_tifs(grids = grids, overwrite = T)

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)
grid_raster = grids$gridR

# 02: Get base concentration levels -------------------------------------------------------------
#conc_base <- get_conc_baseline(species='pm25', grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020)
saveRDS(conc_base, 'cached_data/conc_base.RDS')
conc_base <- readRDS('cached_data/conc_base.RDS')

conc_base %<>% filter(species=='pm25') %>%
  mutate(conc_baseline=lapply(conc_baseline, multiply_by, 1/.7),
         species='tpm10') %>%
  bind_rows(conc_base)

names(conc_base$conc_baseline) <- conc_base$species

# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
#regions <- creahia::get_adm(grid_raster, admin_level=2, res="low")
shp=readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_low.RDS'))
regions <- creahia::get_adm(grid_raster, shp=shp, admin_level=2)



#calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>%
#    filter(scenario %in% scenarios_to_process)
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

runs <- calpuff_files_all$scenario %>% unique
queue <- T


queue <- file.path(project_dir, 'HIA', paste0('hia_GBD__',runs,'.RDS')) %>% file.exists %>% not

#runs = c("d2" ,"d3","f1","f3",  "h3")
causes_to_include = get_calc_causes() %>% grep('Death|YLD', ., value=T)

# HIA ###########################################################################################
#require(doFuture)
#registerDoFuture()
#future::plan("multisession", workers = 4)
#Sys.setenv(GIS_DIR='F:/gis')

#foreach (scen = runs[queue]) %dopar% ({




for (scen in runs[queue]) {
  message(scen)
  # =============================== Get Perturbation Raster ========================================
  conc_perturbation <- calpuff_files_all  %>%
    filter(scenario==scen, period=='annual', speciesName %in% pollutants_to_process)

  conc_perturbation$conc_perturbation <- lapply(conc_perturbation$path, raster)
  names(conc_perturbation$conc_perturbation)=conc_perturbation$species

  pollutants_for_hia = intersect(conc_perturbation$species, conc_base$species)# %>% c('tpm10')


  # 04: HIA Calculations:
  # TODO : add no2_targetyear as input parameter in the wrapper, to calculate conc_base in the wrapper?
  # TODO : change name no2_targetyear -> no2_target_year
  # TODO : change name scale_base_year -> pop_base_year
  # TODO : change value for scale_base_year from 2020 -> 2019, as default in wrapper
  # TODO : change name scale_target_year -> pop_target_year
  hia <-  wrappers.compute_hia_two_images(perturbation_rasters=conc_perturbation$conc_perturbation[pollutants_for_hia],       # perturbation_rasters=raster::stack(perturbation_map)
                                          baseline_rasters=conc_base$conc_baseline,  # baseline_rasters=raster::stack(who_map)
                                          regions=regions,
                                          scenario_name=scen,
                                          scale_base_year=2019,        # Population base year : reference year of INPUT data, for total epidemiological and total population
                                          scale_target_year=2022,      #Population target year
                                          crfs_version="C40",
                                          epi_version="C40",       # epi_version="C40"
                                          valuation_version="viscusi",
                                          return_concentrations=T,
                                          gbd_causes='default',
                                          calc_causes=causes_to_include,
                                          pm2.5_to_pm10_ratio=.7
                                          ) # valuation_version="viscusi"


  saveRDS(hia, file.path(project_dir, 'HIA', paste0('hia_GBD__',scen,'.RDS')))
}

# emis data has a lifetime collumn


#read HIA data
hia <- runs %>% lapply(function(scen) readRDS(file.path(project_dir, 'HIA', paste0('hia_GBD__',scen,'.RDS')))$hia) %>% bind_rows


#select admin regions to include
#adm <- creahelpers::get_adm(level = 2, res='coarse')
adm <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_coarse.RDS'))

hia %>%
  left_join(adm@data %>% dplyr::select(region_id=GID_2, starts_with('NAME'))) %>%
  group_by(across(c(starts_with('NAME'), Outcome, Pollutant, Cause, AgeGrp, iso3, scenario, estimate, double_counted))) %>%
  summarise(across(number, sum)) %>%
  filter(Pollutant != 'PM25' | Cause != 'AllCause') %>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3)) ->
  hia_totals

hia_totals %>% group_by(scenario, Pollutant) %>% filter(!double_counted, Outcome=='Deaths', estimate=='central') %>%
  summarise(across(number, sum, na.rm=T))


# 06: Compute and extract economic costs --------------------------------------------------------
targetyears = 2022

hia_cost <- get_hia_cost(hia=hia_totals, valuation_version="viscusi")

#valuations <- read_csv('~/Rpackages/creahia/inst/extdata/valuation_viscusi.csv')
valuations <- get_valuation('viscusi')

usd_to_lcu=83.20

hia_cost %>%
  distinct(Outcome, valuation_world_2017, valuation_current_usd, iso3) %>%
  left_join(valuations %>% select(Outcome, reference)) %>%
  na.omit %>% add_long_names() %>%
  select(-Outcome, Outcome=Outcome_long) %>%
  mutate(valuation_current_lcu=valuation_current_usd*usd_to_lcu,
         across(is.numeric, function(x) x %>% signif(4) %>% scales::comma(accuracy=1))) %>%
  relocate(Outcome) %>%
  relocate(reference, .after=everything()) %>%
  write_csv(file.path(output_dir, 'valuations.csv'))

hia_fut <- hia_cost %>% get_econ_forecast(years=targetyears, pop_targetyr=2019)

hia_fut %>% add_long_names() %>%
  group_by(Outcome=Outcome_long, Cause=Cause_long, Pollutant, double_counted, scenario, estimate) %>%
  mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>% rename(cost_bn_currentLCU=cost_mn_currentLCU) %>%
  summarise(across(c(number, starts_with('cost')), sum, na.rm=T)) ->
  hia_totals

hia_totals %>% filter(!double_counted) %>% group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm=T)) %>%
  pivot_longer(is.numeric, names_to='Outcome', values_to='number') %>%
  bind_rows(hia_totals) %>% select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
  write_csv(file.path(output_dir, 'HIA results.csv'))






