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

#list.files(path='R', full.names=T) %>% sapply(source)

#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="C:/Users/lauri/Desktop/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

source('project_workflows/emissions_processing_SA.R')


gis_dir <- "~/GIS"                    # The folder where we store general GIS data
#gis_dir <- "C:/Users/lauri/Desktop/My Drive/GIS"

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('NO2', 'PM2.5', 'PM10', 'SO2')

# Load CALMET parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

#list csvs
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)
grid_raster = grids$gridR

#make tifs
calpuff_files %>% make_tifs(grids = grids)

calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)


# 02: Get base concentration levels -------------------------------------------------------------
#conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
conc_base <- readRDS('cached_data/conc_base.RDS')

# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
regions <- creahia::get_adm(grid_raster, admin_level=2, res="low")



calpuff_files_all %<>%
  mutate(source=scenario,
         subspecies=species,
         emitted_species=case_when(species %in% c('pm25', 'tpm10', 'tsp')~'all',
                                   species=='so4'~'so2',
                                   species %in% c('no2','no3')~'nox',
                                   T~species),
         scenario=case_when(grepl('lcpp', source)~source,
                            T~paste0(source, '_', emitted_species)),
         species=case_when(species %in% c('so4', 'no3', 'ppm25')~'pm25',
                             T~species),
         speciesName = case_when(species=='pm25'~'PM2.5', T~speciesName)) %>%
  filter(source != 'lcppmine' | grepl('pm|tsp|hg', species),
         !grepl('lcpp', source) | !grepl('ppm25|so4|no3', subspecies),
         grepl('lcpp', source) | emitted_species != 'all',
         type != 'deposition')

calpuff_files_all %<>% filter(grepl('ppm25|so4|no3', subspecies)) %>%
  mutate(species='tpm10', speciesName='PM10') %>%
  bind_rows(calpuff_files_all)


get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>%
  filter(!grepl('lcpp', scenario), species %in% c('pm25', 'tpm10', 'so2', 'no2'), period=='annual') %>%
  mutate(source=scenario) ->
  calpuff_files_fullplants

calpuff_files_all %<>% bind_rows(calpuff_files_fullplants)

runs <- calpuff_files_all$scenario %>% unique
queue <- F #grepl('_', runs)
queue=runs %in% calpuff_files_fullplants$scenario



# HIA ###########################################################################################
#scen = runs[6]
for (scen in runs[queue]) {
  # =============================== Get Perturbation Raster ========================================
  conc_perturbation <- calpuff_files_all  %>%
    filter(scenario==scen, period=='annual', speciesName %in% pollutants_to_process)

  conc_perturbation$conc_perturbation <- lapply(conc_perturbation$path, raster)
  names(conc_perturbation$conc_perturbation)=conc_perturbation$species

  pollutants_for_hia = intersect(conc_perturbation$species, conc_base$species)# %>% c('tpm10')

  causes_to_include=get_calc_causes()
  gbd_causes='default'

  if(grepl('_', scen)) {
    causes_to_include = get_calc_causes('GBD only') %>% grep('Death|YLD', ., value=T)
    gbd_causes='all'
  }


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
                                          scale_target_year=2021,      # 2025 # Population target year (same as no2_targetyear?)
                                          crfs_version="Krewski-South Africa",
                                          epi_version="C40",       # epi_version="C40"
                                          valuation_version="viscusi",
                                          return_concentrations=T,
                                          gbd_causes=gbd_causes,
                                          calc_causes=causes_to_include,
                                          pm2.5_to_pm10_ratio=.7
                                          ) # valuation_version="viscusi"


  saveRDS(hia, file.path(output_dir, paste0('hia','_',scen,'.RDS')))
}

#hia <- runs %>% lapply(function(scen) readRDS(file.path(output_dir, paste0('hia','_',scen,'.RDS'))) %>% '[['('hia')) %>% bind_rows
hia <- runs[9:10] %>% lapply(function(scen) readRDS(file.path(output_dir, paste0('hia','_',scen,'.RDS'))) %>% '[['('hia')) %>% bind_rows

calpuff_files_all %>%
  mutate(scenario_description=case_when(scenario=="lcppipp"~'Lephalale IPP',
                                        scenario=="lcppmine"~'Lephalale mine',
                                        T~capitalize_first(source))) %>%
  distinct(scenario, scenario_description) %>%
  left_join(hia, .) -> hia



# 06: Compute and extract economic costs --------------------------------------------------------
# TODO : change name scale_target_year -> pop_target_year


targetyears = c(seq(2025,2065,1))

hia_cost <- get_hia_cost(hia=hia, valuation_version="viscusi")

hia_fut <- get_econ_forecast(hia_cost, forecast_years = targetyears, reference_year = 2019)

adm <- creahelpers::get_adm(level = 2, res='coarse')

hia_fut %>%
  left_join(hia_cost %>% distinct(Outcome, Cause, Pollutant, double_counted)) %>%
  #left_join(adm@data %>% select(region_id=GID_2, NAME_1)) %>%
  #filter(iso3=='ZAF',
  #       NAME_1 %in% c('Mpumalanga', 'Gauteng', 'Limpopo')) %>%
  filter(estimate=='central', year %in% 2025:2060, !double_counted) %>%
  group_by(scenario, Outcome) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum))

hia_fut %>% filter(year %in% 2025:2060, Pollutant != 'NO2' | Cause != 'AllCause') %>%
  group_by(scenario, estimate, Outcome, Cause, Pollutant) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
  left_join(hia %>% distinct(scenario, scenario_description, Outcome, Cause, Pollutant, double_counted)) %>%
  mutate(double_counted = ifelse(Pollutant=='NO2', F, double_counted)) %>%
  add_long_names %>% ungroup %>% select(-Outcome, -Cause) %>% rename(Cause=Cause_long, Outcome=Outcome_long) %>%
  pivot_longer(c(number, cost_mn_currentUSD)) %>%
  spread(estimate, value) %>%
  arrange(desc(name), scenario, Outcome!='deaths', double_counted) %>%
  select(scenario_description, Outcome, Cause, Pollutant, central, low, high, variable=name, double_counted) %>%
  write_csv(file.path(output_dir, 'Lephalale_HIA.csv'))

econ_costs %>% saveRDS(file.path(output_dir, 'econ_costs.RDS'))

#scale by pathway
#SO2 -85%, NOx -29%, PM -6% https://www.eea.europa.eu/publications/carbon-capture-and-storage/download
#weighted by Zhou coefficients: 0.461546678-1

econ_costs <- readRDS(file.path(output_dir, 'econ_costs.RDS'))
proj <- read_xlsx(file.path(project_dir, 'emissions/Steel Production by Production Route.xlsx'))

proj %>% pivot_longer(matches('[0-9]{4}'), names_to='year', values_to='steel_output') %>%
  dplyr::mutate(across(year, as.numeric),
                route=case_when(route=='Blast Furnace'~'BF',
                                grepl('Furnace with hydrogen', route)~'BF-H2',
                                grepl('Furnace CCS', route)~'BF-CCS',
                                T~route),
                scaling_pm = case_when(route=='BF'~1,
                                       route=='BF-CCS'~0.461546678,
                                       route=='BF-H2'~.7, T~0) *  steel_output,
                scaling_no2 = case_when(route=='BF'~1,
                                        route=='BF-CCS'~0.71,
                                        route=='BF-H2'~.7, T~0) * steel_output) %>%
  group_by(pathway, year) %>%
  dplyr::summarise(across(starts_with('scaling_'), sum)) %>%
  group_by(pathway) %>%
  dplyr::mutate(across(starts_with('scaling_'), function(x) x/x[year==2020]),
                year=ifelse(year==2020, 2021, year)) ->
  scaling

scaling %>% ggplot(aes(year, scaling_no2, col=pathway)) + geom_line()

econ_costs$cost_forecast %>% full_join(scaling)

econ_costs$cost_forecast %>%
  dplyr::group_by(across(c(scenario, estimate, iso3, matches('Outcome|Cause'), Pollutant, year))) %>%
  dplyr::summarise(across(c(number, cost.mnUSD), sum, na.rm=T)) %>%
  write_excel_csv(file.path(output_dir, 'hia results by country and year.csv'))




econ_costs$cost_forecast %>%
  filter(!is.na(year)) %>%
  dplyr::group_by(across(c(scenario, estimate, iso3, matches('Outcome|Cause|region_'), Pollutant))) %>%
  dplyr::mutate(groupnumber=cur_group_id()) -> indata

indata %>%
  group_modify(function(df, ...) {
    message(df$groupnumber[1])
    df %>% dplyr::summarise(across(c(number, cost.mnUSD),
                            function(x) {
                              x.out=NA
                              if(all(!is.na(x)) & length(x)>1) x.out=approx(df$year, x, 2022:2050)$y %>% sum()
                              return(x.out)
                            }))
    }) -> hia_cumu


hia_cumu %>% write_excel_csv(file.path(output_dir, 'hia results by admin 2 area, 2022-2050 cumulative.csv'))
hia_cumu %>%
  dplyr::group_by(across(c(scenario, estimate, iso3, matches('Outcome|Cause'), Pollutant))) %>%
  dplyr::summarise(across(c(number, cost.mnUSD), sum, na.rm=T)) %>%
  write_excel_csv(file.path(output_dir, 'hia results by country and year, 2022-2050 cumulative.csv'))
