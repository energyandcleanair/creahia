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

project_dir="I:/koreasteel"        # calpuff_external_data-2 persistent disk (project data)

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA_coal") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions_coal")

emis <- read_csv(file.path(emissions_dir,'emissions inputs, with clustering.csv')) %>%
  mutate(scenario=tolower(emission_names))

gis_dir <- "F:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

#setwd(get_gis_dir())
#system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('NO2', 'PM2.5', 'PM10', 'SO2')

# Load CALMET parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

#list csvs
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>%
  mutate(scenario=paste0(scenario, ifelse(grepl('_nps', name), '_nps', '')))
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)
grid_raster = grids$gridR

#make tifs
scenarios_to_process=calpuff_files$scenario %>% subset(grepl('202', .) | . %in% emis$scenario) %>% unique
calpuff_files %>% filter(scenario %in% scenarios_to_process, period=='annual', speciesName %in% pollutants_to_process) %>% make_tifs(grids = grids, overwrite = F)

# 02: Get base concentration levels -------------------------------------------------------------
conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
conc_base %>% saveRDS(file.path(output_dir, 'conc_base.RDS'))
conc_base <- readRDS(file.path(output_dir, 'conc_base.RDS'))

conc_base %<>% filter(species=='pm25') %>%
  mutate(conc_baseline=lapply(conc_baseline, multiply_by, 1/.7),
         species='tpm10') %>%
  bind_rows(conc_base)

names(conc_base$conc_baseline) <- conc_base$species

# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
#regions <- creahia::get_adm(grid_raster, admin_level=2, res="low")
shp=readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_low.RDS'))
regions <- creahia::get_adm(grid_raster, shp=shp, admin_level=2)


calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>%
  mutate(scenario=paste0(scenario, ifelse(grepl('_nps', name), '_nps', ''))) %>%
  filter(scenario %in% scenarios_to_process)


queue <- !file.exists(file.path(output_dir, paste0('hia_GBD__',scenarios_to_process,'.RDS')))

causes_to_include = get_calc_causes() %>% grep('Death|YLD', ., value=T)

# HIA ###########################################################################################
#require(doFuture)
#registerDoFuture()
#future::plan("multisession", workers = 4)
#Sys.setenv(GIS_DIR='F:/gis')

#foreach (scen = runs[queue]) %dopar% ({
for (scen in scenarios_to_process[queue]) {
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


  saveRDS(hia, file.path(output_dir, paste0('hia_GBD__',scen,'.RDS')))
}



#read HIA data
hia <- scenarios_to_process %>% lapply(function(scen) readRDS(file.path(output_dir, paste0('hia_GBD__',scen,'.RDS')))$hia) %>% bind_rows


#select admin regions to include
#adm <- creahelpers::get_adm(level = 2, res='coarse')
adm <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_coarse.RDS'))

hia %>%
  left_join(adm@data %>% dplyr::select(region_id=GID_2, starts_with('NAME'))) %>%
  group_by(across(c(starts_with('NAME'), outcome, pollutant, cause, age_group, iso3, scenario, estimate, double_counted))) %>%
  summarise(across(number, sum)) %>%
  filter(pollutant != 'PM25' | cause != 'AllCause') %>%
  mutate(number = number * case_when(pollutant != 'NO2' | cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3)) ->
  hia_totals

hia_totals %>% group_by(scenario, pollutant) %>% filter(!double_counted, outcome=='Deaths', estimate=='central') %>%
  summarise(across(number, sum, na.rm=T))

hia_totals %>% saveRDS(file.path(output_dir, 'hia_totals.RDS'))

# 06: Compute and extract economic costs --------------------------------------------------------
targetyears = 2022

hia_cost <- get_hia_cost(hia=hia_totals, valuation_version="viscusi")

valuations <- get_valuations_raw('viscusi')

usd_to_lcu=1292.16 #2022 average

hia_cost %>%
  distinct(outcome, valuation_world_2017, valuation_current_usd, iso3) %>%
  left_join(valuations %>% select(outcome, reference)) %>%
  na.omit %>% add_long_names() %>%
  select(-outcome, outcome=outcome_long) %>%
  mutate(valuation_current_lcu=valuation_current_usd*usd_to_lcu,
         across(is.numeric, function(x) x %>% signif(4) %>% scales::comma(accuracy=1))) %>%
  relocate(outcome) %>%
  relocate(reference, .after=everything()) %>%
  write_csv(file.path(output_dir, 'valuations.csv'))



hia_fut <- hia_cost %>% get_econ_forecast(forecast_years = targetyears, reference_year = 2019)

hia_fut %>% add_long_names() %>%
  group_by(outcome=outcome_long, cause=cause_long, pollutant, double_counted, scenario, estimate) %>%
  mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>% rename(cost_bn_currentLCU=cost_mn_currentLCU) %>%
  summarise(across(c(number, starts_with('cost')), sum, na.rm=T)) ->
  hia_totals

hia_totals %>% write_csv(file.path(output_dir, 'hia_totals.csv'))

hia_totals %>% filter(!double_counted) %>% group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm=T)) %>%
  pivot_longer(is.numeric, names_to='outcome', values_to='number') %>%
  bind_rows(hia_totals) %>% select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
  write_csv(file.path(output_dir, 'HIA results.csv'))


emis <- read_csv(file.path(emissions_dir,'emissions inputs, with clustering.csv')) %>%
  mutate(scenario=tolower(emission_names))


#read_csv(file.path(output_dir, 'heavy_metal_HIA.csv')) -> hm_hia
#hm_hia %>% group_by(outcome, year, estimate, unit_number) %>% summarise(across(c(number, cost), sum))

hia_totals %<>% filter(!double_counted) %>% group_by(scenario, estimate, double_counted) %>%
  summarise(across(starts_with('cost_'), sum)) %>%
  pivot_longer(starts_with('cost_'), names_to='outcome', values_to='number') %>%
  mutate(cause='all', pollutant='all') %>%
  bind_rows(hia_totals)

hia_totals %>% filter(grepl('death', outcome), !double_counted) %>%
  group_by(scenario, estimate) %>%
  summarise(across(number, ~sum(.x, na.rm=T))) %>%
  mutate(outcome='deaths', cause='all causes', pollutant='all', double_counted=T) %>%
  bind_rows(hia_totals) %>%
  filter(!double_counted | (pollutant == 'PM2.5' & cause != 'all') |
           (pollutant == 'all' & cause == 'all causes'),
         !(pollutant == 'PM2.5' & cause == 'all'),
         !grepl('years of life|children under 5|low birthweight', outcome)) %>%
  filter(grepl('202', scenario)) %>%
  #filter(iso3=='KOR') %>%
  #filter(!double_counted) %>%
  add_long_names() %>%
  mutate(outcome = case_when(outcome_long == cause_long | cause_long=='Asthma'~outcome_long,
                             outcome_long=='cost_mn_currentUSD'~'economic costs, mln USD',
                             outcome_long=='cost_bn_currentLCU'~'economic costs, bln KRW',
                             T~paste0(outcome_long, ', ', cause_long))) %>%
  group_by(scenario, outcome, pollutant, estimate) %>%
  summarise(across(number, sum, na.rm=T)) %>%
  spread(estimate, number) %>%
  relocate(high, .after=everything()) %>%
  write_csv(file.path(output_dir, 'total health impacts by scenario.csv'))

hia_totals %>% right_join(emis %>% distinct(scenario, plant)) %>%
  mutate(across(plant, capitalize_first)) %>%
  add_long_names() %>%
  mutate(outcome_long = case_when(outcome_long=='cost_mn_currentUSD'~'economic costs, mln USD',
                                  outcome_long=='cost_bn_currentLCU'~'economic costs, bln KRW',
                                  T~outcome_long)) %>%
  filter(!double_counted, grepl('deaths$|work abs|new cases|emergency|preterm|costs', outcome_long)) %>%
  group_by(plant, outcome=outcome_long, estimate) %>%
  summarise(across(number, sum, na.rm=T)) -> hia_byplant

hia_byplant %<>% full_join(emis %>% distinct(plant=capitalize_first(plant), NPS_share)) %>%
  mutate(impacts_from='whole plant')

hia_byplant %<>% mutate(number=number*NPS_share, impacts_from='NPS share of plant') %>%
  bind_rows(hia_byplant)

hia_byplant %>%
  mutate(across(number, ~.x %>% signif(3) %>% scales::comma(accuracy=1))) %>%
  group_by(plant, outcome, impacts_from, NPS_share) %>%
  summarise(value=paste0(number[estimate=='central'], ' (', number[estimate=='low'], ' – ', number[estimate=='high'], ')')) %>%
  spread(outcome, value) %>%
  write_csv(file.path(output_dir, 'health impacts by plant, 2022.csv'))
