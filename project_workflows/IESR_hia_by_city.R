
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
require(pbapply)

#list.files(path='R', full.names=T) %>% sapply(source)

project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

#make tifs
calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
scenarios_to_process=calpuff_files$scenario %>% subset(!grepl('^v|\\.aux', .)) %>% unique()

source('../CALPUFF/creapuff/project_workflows/IndonesiaIESR/read_IESR_emissions.R')

targetyears = emis$year %>% unique

hia <- readRDS(file.path(project_dir, 'HIA', paste0('hia_GEMM_1ug.RDS')))

hia$hia %<>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3))


# 06: Compute and extract economic costs --------------------------------------------------------
hia_cost <- get_hia_cost(hia$hia, valuation_version="viscusi")

hia_fut <- hia_cost %>% get_econ_forecast(years=targetyears, pop_targetyr=2019)

hia_fut_indo <- hia_fut %>% filter(iso3=='IDN') %>% #year==2022
  select(region_id, iso3, pop, Pollutant, Outcome, Cause, double_counted, year, estimate, unit, number, cost_mn_currentUSD)
regions_to_process <- unique(hia_fut_indo$region_id)
#hia_fut_indo %<>% split(hia_fut_indo$region_id)

#get costs and deaths per t emissions
require(pbapply)
modeled_emissions %<>% ungroup
scenarios_to_process %>%
  pblapply(function(run) {
    message(run)
    paste0('exp_',run,'.csv') %>% file.path(output_dir, .) %>%
      read_csv() %>%
      filter(region_id %in% regions_to_process) %>%
      pivot_longer(-region_id, names_to='subspecies', values_to='exposure') %>%
      mutate(Pollutant=case_when(subspecies %in% c('so4', 'no3', 'ppm25')~'PM25',
                                 T~toupper(subspecies)),
             emitted_species=case_when(subspecies %in% c('so2', 'so4')~'SOx',
                                       subspecies %in% c('no2','no3')~'NOx',
                                       subspecies=='ppm25'~'PM'),
             cluster=run) %>%
      left_join(modeled_emissions) %>%
      inner_join(hia_fut_indo) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x * exposure/pop/modeled_emissions)) %>%
      distinct(region_id, cluster, year, Outcome, Cause, emitted_species, Pollutant, double_counted, estimate, unit, .keep_all=T) %>%
      saveRDS(file.path(output_dir, paste0('hia_per_t_region-',run,'.RDS')))
  })

adm2 <- readRDS(file.path(gis_dir, 'boundaries/gadm36_2_low.RDS')) %>% subset(NAME_0=='Indonesia')

#aggregate HIA per t emissions to ADM1
scenarios_to_process %>%
  pblapply(function(scen) {
    paste0('hia_per_t_region-', scen,'.RDS') %>%
      file.path(output_dir, .) %>% readRDS -> hia_per_t_region

    hia_per_t_region %>% left_join(adm2@data %>% select(region_id=GID_2, GID_1)) %>%
      group_by(cluster, emitted_species, GID_1, Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
      summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>%
      saveRDS(file.path(output_dir, paste0('hia_per_t_adm1-',scen,'.RDS')))
  })

#total impacts by adm1
emis %>% ungroup %>%
  filter(COD<year, year_retire>=year,
         Status=='operating' | year>2022,
         year>2022 | scenario=='BAU',
         year<=2040 | !grepl('1\\.5', scenario) | (Owner=='captive' & grepl('excluding captive', scenario))) %>%
  rename(emitted_species=pollutant) %>%
  group_by(cluster, year) %>%
  mutate(cur_group_id=cur_group_id()) ->
  emis_for_calcs

emis_for_calcs %>%
  group_modify(function(df, group) {
    outname <- file.path(output_dir, paste0('hia_adm1_', group$cluster, '_', group$year, '.RDS'))
    message(unique(df$cur_group_id))

    if(!file.exists(outname)) {
      paste0('hia_per_t_adm1-', group$cluster,'.RDS') %>% file.path(output_dir, .) %>% readRDS -> hia_per_t_adm1



      df %>%
        inner_join(hia_per_t_adm1 %>% filter(year==group$year)) %>%
        group_by(scenario, GID_1, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
        mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
        summarise(across(c(number, cost_mn_currentUSD), sum),
                  across(c(MW, utilization), unique)) %>%
        mutate(across(c(number_per_TWh=number, cost_mn_currentUSD_per_TWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6)) %>%
        saveRDS(outname)
    }

    return(tibble(outname=outname))
  }) -> adm1_result_files

adm1_result_files %>% group_by(year) %>%
  group_modify(function(df, group) {
    message(group$year)
    df$outname %>% lapply(readRDS) %>% bind_rows %>%
      group_by(GID_1, scenario, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
      summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T)))
  }) -> hia_fut_adm1

hia_fut_adm1 %>% saveRDS(file.path(output_dir, 'hia_fut_adm1.RDS'))

hia_fut_adm1 %>% ungroup %>%
  filter(grepl('BAU$|1\\.5 degrees$|captive$|2022$', scenario),
         grepl('Asthma.Inci|Death|Absence', Outcome),
         !double_counted,
         year>=2023 | scenario=='BAU',
         year<2023 | scenario != 'BAU') %>%
  group_by(scenario, GID_1, Outcome, Pollutant, year, estimate) %>%
  summarise(across(number, sum)) %>%
  add_long_names() %>%
  mutate(scenario=ifelse(scenario=='BAU', 'Historical', scenario),
         number=number*10504.76/13152.04) %>%
  right_join(adm1 %>% select(NAME_0, NAME_1, GID_1) %>% st_drop_geometry(), .) %>%
  write_excel_csv(file.path(output_dir, 'hia_adm1_by_scenario_for_iesr.csv'))

#total impacts by adm1 and plant unit
emis_for_calcs %>%
  group_modify(function(df, group) {
    outname <- file.path(output_dir, paste0('hia_adm1_byplant_', group$cluster, '_', group$year, '.RDS'))
    message(unique(df$cur_group_id))

    if(!file.exists(outname)) {
      paste0('hia_per_t_adm1-', group$cluster,'.RDS') %>% file.path(output_dir, .) %>% readRDS -> hia_per_t_adm1



      df %>%
        inner_join(hia_per_t_adm1 %>% filter(year==group$year)) %>%
        group_by(CFPP.name, scenario, GID_1, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
        mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
        summarise(across(c(number, cost_mn_currentUSD), sum),
                  across(c(MW, utilization), unique)) %>%
        mutate(across(c(number_per_TWh=number, cost_mn_currentUSD_per_TWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6)) %>%
        saveRDS(outname)
    }

    return(tibble(outname=outname))
  }) -> adm1_byplant_result_files

adm1_byplant_result_files %>% group_by(year) %>% #filter(year==2038) %>%
  group_walk(function(df, group) {
    message(group$year)
    df$outname %>%
      pblapply(function(f) {
        f %>% readRDS() %>%
          filter(grepl('BAU$|1\\.5 degrees$|captive$|2022$', scenario),
                 grepl('Asthma.Inci|Death|Absence', Outcome),
                 !double_counted,
                 group$year>=2023 | scenario=='BAU',
                 group$year<2023 | scenario != 'BAU') %>%
          group_by(CFPP.name, scenario, GID_1, Outcome, Pollutant, estimate) %>%
          summarise(across(number, sum)) %>%
          add_long_names() %>%
          mutate(scenario=ifelse(scenario=='BAU', 'Historical', scenario),
                 number=number*10504.76/13152.04) %>%
          right_join(adm1 %>% select(NAME_0, NAME_1, GID_1) %>% st_drop_geometry(), .)
        }) %>% subset(sapply(.,nrow)>0) %>%
      bind_rows %>%
      write_excel_csv(file.path(output_dir, paste0('hia_adm1_by_plant_by_scenario_for_iesr_',group$year,'.csv')))
  })





#current impacts by adm2 and plant
paste0('hia_per_t_region-', .,'.RDS') %>% file.path(output_dir, .) %>%
  pblapply(function(infile) infile %>% readRDS %>% filter(year==2022)) %>% bind_rows() -> hia_per_t_region

emis %>% ungroup %>%
  filter(COD<=year, year_retire>=year,
         Status=='operating', year==2022, scenario=='BAU') %>%
  rename(emitted_species=pollutant) %>%
  group_by(cluster, year) %>%
  group_modify(function(df, group) {
    message(group)
    df %>%
      inner_join(hia_per_t_region %>% filter(cluster==group$cluster, year==group$year)) %>%
      group_by(CFPP.name, Owner, province, region_id, Latitude, Longitude, scenario, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
      summarise(across(c(number, cost_mn_currentUSD), sum),
                across(c(MW, utilization), unique)) %>%
      ungroup
  }) %>%
  mutate(across(c(number_per_TWh=number, cost_mn_currentUSD_per_TWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6)) ->
  hia_current

hia_current %>% saveRDS(file.path(output_dir, 'hia_current_adm2.RDS'))

hia_current %>% filter(!double_counted) %>%
  mutate(number = ifelse(grepl('Death', Outcome), number, 0)) %>%
  group_by(CFPP.name, Owner, region_id, scenario, year, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) ->
  hia_current_totals

hia_current_totals %<>% rename(GID_2=region_id) %>%
  right_join(adm2@data %>% select(starts_with('NAME_'), starts_with('GID_')), .) %>%
  right_join(plant_names %>% select(CFPP.name, unit_name), .)

hia_current_totals %>% write_csv(file.path(output_dir, 'deaths and total costs by CFPP unit and adm2 region.csv'))

hia_current %>%
  group_by(GID_2=region_id, scenario, year, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>%
  right_join(adm2@data %>% select(starts_with('NAME_'), starts_with('GID_')), .) ->
  hia_current_allplants

hia_current_allplants %>% write_csv(file.path(output_dir, 'current health impacts by adm2 region.csv'))
