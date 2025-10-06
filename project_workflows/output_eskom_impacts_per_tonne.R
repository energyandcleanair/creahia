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

#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="C:/Users/lauri/Desktop/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA_MES") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

source('project_workflows/emissions_processing_SA_v3.R')

emissions_data %>%
  pivot_longer(c(Hg, NOx, PM, SO2), names_to='emitted_species', values_to='modeled_emissions') %>%
  group_by(plant, emitted_species) %>% summarise(across(modeled_emissions, sum)) ->
  emissions_modeled

hia_fut <- readRDS(file.path(output_dir, 'hia_fut.RDS'))

hia_fut %>%
  left_join(emissions_modeled) %>%
  mutate(across(c(number, cost_mn_currentUSD), divide_by, modeled_emissions)) %>%
  mutate(number = ifelse(grepl('Deaths', outcome), number, 0)) %>%
  filter(!double_counted) %>%
  group_by(plant, emitted_species, year, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T)) ->
  hia_per

hia_per %<>%
  mutate(across(cost_mn_currentUSD, multiply_by, 1e6)) %>%
  rename(deaths_per_tonne=number, cost_per_tonne_USD=cost_mn_currentUSD)

hia_per %>%
  filter(year==2019, estimate=='central') %>%
  ggplot(aes(plant, cost_per_tonne_USD)) + facet_wrap(~emitted_species) + geom_col() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

hia_per %>%
  filter(estimate=='central', year %in% c(2019, seq(2030, 2080, 10))) %>%
  ggplot(aes(year, deaths_per_tonne, col=plant)) + facet_wrap(~emitted_species) + geom_line()

hia_per %>% filter(year %in% c(2019, seq(2030, 2080, 10))) %>%
  write_csv(file.path(output_dir, 'hia per tonne emissions.csv'))

emis_byyear_nofilter %>% filter(year==2080, !grepl('delay', scenario)) %>%
  group_by(plant, emitted_species, scenario) %>%
  summarise(across(emissions_wo_decomm, sum)) ->
  emis_byscen

kriel_peers <- emission_reductions %>% filter(decommissioning_start %in% 2026:2030) %>% use_series(plant) %>% unique

d %>% filter(grepl('MWh', V1) | V2=='Coal', Month>='2021-04-01', Month<='2022-03-31', plant %in% kriel_peers) %>%
  group_by(plant, V2) %>%
  summarise(across(value, sum, na.rm=T)) %>%
  summarise(coal_per=value[V2=='Coal']/value[V2=='Actual']) %>%
  summarise(across(coal_per, median, na.rm=T)) %>% unlist ->
  coal_per

d %>%
  filter(V2 %in% c('Actual', 'Coal'), Month>='2021-04-01', Month<='2022-03-31') %>%
  group_by(plant, V2) %>% summarise(across(value, mean, na.rm=T)) %>%
  mutate(value=case_when(V2=='Actual' & value==0~value[V2=='Coal']/coal_per, T~value)*12) %>%
  filter(V2=='Actual') %>% select(plant, MWh=value) -> gen

emis_byscen %<>% left_join(gen) %>%
  mutate(emissions_t_per_GWh=emissions_wo_decomm / MWh * 1000) %>%
  select(plant, scenario, emitted_species, emissions_t_per_GWh, emissions_t_per_year=emissions_wo_decomm,
         generation_MWh=MWh)

emis_byscen %>%
  ggplot(aes(plant, emissions_t_per_GWh, col=scenario)) + facet_wrap(~emitted_species, scales='free_y') +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

emis_byscen %>%
  write_csv(file.path(output_dir, 'emissions by plant and scenario.csv'))
