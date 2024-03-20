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

source('project_workflows/eskom_helpers.R')

#list.files(path='R', full.names=T) %>% sapply(source)

#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="G:/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA_MES") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

source('project_workflows/emissions_processing_SA_v3.R')
source('project_workflows/eskom_plot_emission_scenarios.R')

emis_byyear %>%
  group_by(year, plant, emitted_species, scenario) %>%
  summarise(across(emissions, sum)) ->
  emis_byyear_byplant

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "F:/gis"

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

# 02: Get base concentration levels -------------------------------------------------------------
#conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
#saveRDS(conc_base, 'cached_data/conc_base.RDS')
conc_base <- readRDS('cached_data/conc_base.RDS')

# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
regions <- creahia::get_adm(grid_raster, admin_level=2, res="low") %>% filter(country_id=='ZAF')



calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

calpuff_files_all %<>%
  mutate(source=scenario %>% capitalize.first(),
         subspecies=species,
         emitted_species=case_when(species %in% c('pm25', 'tpm10', 'tsp')~'all',
                                   species=='so4'~'so2',
                                   species %in% c('no2','no3')~'nox',
                                   T~species),
         scenario=case_when(grepl('lcpp', source)~source,
                            T~paste0(source, '_', emitted_species)),
         species=case_when(species %in% c('so4', 'no3', 'ppm25')~'pm25',
                             T~species),
         speciesName = case_when(species=='pm25'~'PM2.5', T~speciesName))

calpuff_files_all %<>% filter(grepl('ppm25|so4|no3', subspecies)) %>%
  mutate(species='tpm10', speciesName='PM10') %>%
  bind_rows(calpuff_files_all)

calpuff_files_all %<>% filter(source %in% emis_byyear_byplant$plant)

runs <- calpuff_files_all$scenario %>% unique
queue <- grepl('_', runs) & !grepl('pm10', runs)

causes_to_include = get_calc_causes('GBD only') %>% grep('Death|YLD', ., value=T)

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
                                          scale_target_year=2021,      # 2025 # Population target year (same as no2_targetyear?)
                                          crfs_version="Krewski-South Africa",
                                          epi_version="C40",       # epi_version="C40"
                                          valuation_version="viscusi",
                                          return_concentrations=T,
                                          gbd_causes='all',
                                          calc_causes=causes_to_include,
                                          pm2.5_to_pm10_ratio=.7
                                          ) # valuation_version="viscusi"


  saveRDS(hia, file.path(project_dir, 'HIA', paste0('hia_GBD__',scen,'.RDS')))
}



#read HIA data

expand.grid(plant=emis_byyear_byplant$plant %>% unique,
            subspecies=c('so2', 'nox', 'ppm25')) %>%
  mutate(calpuff_name = substr(plant, 1, 7) %>% tolower,
         run=paste0(calpuff_name, '_', subspecies)) ->
  runs


hia <- runs$run %>% lapply(function(scen) readRDS(file.path(project_dir, 'HIA', paste0('hia_GBD__',scen,'.RDS')))$hia) %>% bind_rows


#select admin regions to include
adm <- creahelpers::get_adm(level = 2, res='coarse')

hia %>%
  left_join(adm@data %>% select(region_id=GID_2, NAME_1)) %>%
  filter(iso3=='ZAF') %>%
  mutate(region_id=NAME_1) %>%
  group_by(region_id, Outcome, Pollutant, Cause, AgeGrp, iso3, scenario, estimate, double_counted) %>%
  summarise(across(number, sum)) %>%
  filter(Pollutant != 'PM25' | Cause != 'AllCause') %>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3)) ->
  hia_totals

hia_totals %>% group_by(Pollutant) %>% filter(!double_counted, Outcome=='Deaths', estimate=='central') %>%
  summarise(across(number, sum))


# 06: Compute and extract economic costs --------------------------------------------------------
targetyears = unique(emis_byyear_byplant$year)

hia_cost <- get_hia_cost(hia=hia_totals, valuation_version="viscusi")

hia_cost %>% group_by(Pollutant) %>%
  filter(!double_counted, Outcome=='Deaths', estimate=='central') %>% summarise(across(number, sum))

#valuations <- read_csv('~/Rpackages/creahia/inst/extdata/valuation_viscusi.csv')
valuations <- get_valuation('viscusi')

usd_to_lcu=14.7912

hia_cost %>% filter(iso3=='ZAF') %>%
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

#add emissions projections to HIA data
hia_fut %<>% ungroup %>%
  separate(scenario, c('calpuff_name', 'emitted_species')) %>%
  left_join(runs %>% distinct(plant, calpuff_name)) %>%
  mutate(emitted_species = disambiguate(emitted_species, unique(emis_byyear_byplant$emitted_species), ignore.case=T))

hia_fut %>% saveRDS(file.path(output_dir, 'hia_fut.RDS'))

emissions_data %>%
  pivot_longer(c(Hg, NOx, PM, SO2), names_to='emitted_species', values_to='modeled_emissions') %>%
  group_by(plant, emitted_species) %>% summarise(across(modeled_emissions, sum)) %>%
  left_join(emis_byyear_byplant, .) ->
  emis_byyear_byplant

emis_byyear_byplant %>% write_csv(file.path(emissions_dir, 'emissions scaling for scenarios_v2.csv'))


hia_fut %>% right_join(emis_byyear_byplant) -> hia_scen

hia_scen %<>% mutate(across(c(number, cost_mn_currentUSD), multiply_by, emissions/modeled_emissions)) %>%
  group_by(region_id, plant, year, scenario, Outcome, Cause, Pollutant, double_counted, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum))

hia_scen %>%
  filter(Outcome=='Deaths', !double_counted, estimate=='central',
         region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T) %>%
  group_by(year, scenario, province=region_id) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) ->
  plotdata

plotdata %>% group_by(province, scenario) %>%
  summarise(max_value=max(number),
            line_start=number[year==2032]) %>%
  arrange(line_start) %>% ungroup %>%
  mutate(max_value=max(max_value)) %>%
  group_by(province) %>%
  mutate(line_end = max_value * (1.5+seq_along(scenario))/7.5) %>%
  pivot_longer(starts_with('line'), values_to='number') %>%
  mutate(year=ifelse(name=='line_start', 2032, 2050)) %>%
  filter(province=='Gauteng') ->
  label_pos

plotdata %>%
  ggplot(aes(year, number, col=scenario)) +
  facet_wrap(~province) +
  geom_line(size=1) +
  geom_label(aes(label=scenario), data=label_pos %>% filter(name=='line_end'), hjust=0, size=3) +
  geom_line(data=label_pos) +
  theme_crea(legend.position='top') +
  scale_color_manual(values=scenario_colors,
                     guide=guide_legend(nrow=1, override.aes = list(label='', linewidth=1))) +
  labs(title='Deaths attributed to Eskom emissions by province',
       y='cases per year', x='') +
  snug_x + x_at_zero() -> p
quicksave(file.path(output_dir, 'deaths by province and scenario.png'), plot=p)

hia_scen %>%
  filter(Outcome=='Deaths', !double_counted, estimate=='central',
         region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T) %>%
  group_by(year, scenario) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) ->
  plotdata

plotdata %>% group_by(scenario) %>%
  summarise(max_value=max(number),
            line_start=number[year==2032]) %>%
  arrange(line_start) %>%
  mutate(line_end = max_value * (1.5+seq_along(scenario))/7.5) %>%
  pivot_longer(starts_with('line'), values_to='number') %>%
  mutate(year=ifelse(name=='line_start', 2032, 2050)) ->
  label_pos

plotdata %>%
  ggplot(aes(year, number, col=scenario)) +
  geom_line(size=2) +
  geom_label(aes(label=scenario), data=label_pos %>% filter(name=='line_end'), hjust=0, size=5) +
  geom_line(data=label_pos, size=1) +
  theme_crea(legend.position='top') +
  scale_color_manual(values=scenario_colors,
                     guide=guide_legend(nrow=1, override.aes = list(label='', linewidth=1))) +
  labs(title='Deaths attributed to Eskom emissions',
       y='cases per year', x='') +
  snug_x + x_at_zero() -> p
quicksave(file.path(output_dir, 'deaths by scenario.png'), plot=p)


hia_scen %>%
  filter(Outcome=='Deaths', !double_counted, estimate=='central',
         region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T,
         year>=2025) %>%
  group_by(scenario, province=region_id) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
  ggplot(aes(scenario, number, fill=scenario)) + geom_col() + facet_wrap(~province) +
  theme_crea(legend.position='top', axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=scenario_colors, guide=guide_legend(nrow=1)) +
  x_at_zero(labels=scales::comma) +
  labs(title='Cumulative deaths attributed to Eskom emissions',
       subtitle='2025 until end-of-life',
       y='cases', x='') -> p
quicksave(file.path(output_dir, 'cumulative deaths by province and scenario.png'), plot=p)

#hia_scen <- readRDS(file.path(output_dir, 'hia_scen.RData'))

hia_scen %>% ungroup %>%
  filter(region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T, year>=2025) %>%
  output_tables('cumulative', bad_scenario='Eskom plan', good_scenario = 'compliance')
hia_scen %>% ungroup %>%
  filter(region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T, year==2026) %>%
  output_tables('2026', bad_scenario='Eskom plan', good_scenario = 'compliance')
hia_scen %>% ungroup %>%
  filter(region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T, year>=2025) %>%
  output_tables('cumulative', bad_scenario='Eskom plan', good_scenario = 'delayed compliance')
hia_scen %>% ungroup %>% filter(region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T, year==2032) %>%
  output_tables('2032', bad_scenario='Eskom plan', good_scenario = 'compliance')
hia_scen %>% ungroup %>% filter(region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T, year>=2025) %>%
  output_tables('cumulative', bad_scenario='compliance', good_scenario = 'BAT')

#emissions reductions
emis_byyear %>%
  group_by(emitted_species, year, scenario) %>%
  summarise(across(emissions, sum)) %>%
  summarise(change = emissions[scenario=='BAT']/emissions[scenario=='no improvements']-1) %>%
  filter(year==2031) %>%
  mutate(reduction = paste0(emitted_species, ' by ', scales::percent(-change), ', ')) %>%
  use_series(reduction) %>% rev %>% paste(collapse='')




