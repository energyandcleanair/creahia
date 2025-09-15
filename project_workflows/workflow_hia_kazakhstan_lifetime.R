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

project_dir="H:/kazakhstan"       # calpuff_external_data-2 persistent disk (project data)

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")


#emis <- read_xlsx(file.path(emissions_dir, 'JSW_Orisha_steel_cropper_calc.xlsx'), sheet='CALPUFF input')

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('NO2', 'PM2.5', 'PM10', 'SO2')



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

hia_totals  %>% group_by(scenario, Pollutant) %>% filter(!double_counted, Outcome=='Deaths', estimate=='central') %>%
  #summarise(across(number, sum, na.rm=T))
  summarise(across(number, ~sum(.x, na.rm=T), na.rm=T))


# 06: Compute and extract economic costs --------------------------------------------------------
# TODO : change name scale_target_year -> pop_target_year

targetyears = c(seq(1996,2023,1))

hia_cost <- get_hia_cost(hia=hia, valuation_version="viscusi")
valuations <- get_valuations_raw('viscusi')

#usd_to_lcu=15447
usd_to_lcu=461.15



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

hia_fut <- get_econ_forecast(hia_cost, forecast_years = targetyears, reference_year = 2019)

hia_fut %>%
  left_join(hia_cost %>% distinct(Outcome, Cause, Pollutant, double_counted)) %>%
  group_by(scenario, Outcome) %>%
  #summarise(across(c(number, cost_mn_currentUSD), sum))
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T)))

years = list('allstack' = 1996:2023)


################################################################################################

# Scale impacts based on steel production, normalised to year 2023
Steel_Production = read_xlsx(file.path(output_dir, 'Temirtau_Proxies_for_AqQ.xlsx'), sheet='Clean')
hia_fut <- merge(hia_fut, Steel_Production, by = "year", all.x = TRUE)
hia_fut$number <- hia_fut$normalised * hia_fut$number
hia_fut$cost_mn_currentUSD <- hia_fut$normalised * hia_fut$cost_mn_currentUSD
hia_fut$cost_mn_currentLCU <- hia_fut$normalised * hia_fut$cost_mn_currentLCU



# cumulative  integrated over time and space
for(x in names(years)){
  hia_fut %>% filter(scenario == x) %>%
    filter(year %in% years[[x]], Pollutant != 'NO2' | Cause != 'AllCause') %>%
    group_by(scenario, estimate, Outcome, Cause, Pollutant) %>%
    summarise(across(c(number, cost_mn_currentUSD),  ~sum(.x, na.rm=T))) %>%
    #summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
    left_join(hia %>% distinct(scenario, Outcome, Cause, Pollutant, double_counted)) %>%
    mutate(double_counted = ifelse(Pollutant=='NO2', F, double_counted)) %>%
    add_long_names %>% ungroup %>% select(-Outcome, -Cause) %>% rename(Cause=Cause_long, Outcome=Outcome_long) %>%
    pivot_longer(c(number, cost_mn_currentUSD)) %>%
    spread(estimate, value) %>%
    arrange(desc(name), scenario, Outcome!='deaths', double_counted) %>%
    select(scenario, Outcome, Cause, Pollutant,  central, low, high, variable=name, double_counted) %>%
    write_csv(file.path(output_dir, glue::glue('{x}_Cumulative.csv')))
}












