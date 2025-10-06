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
emis <- read_csv(file.path(emissions_dir, 'emissions scaling for scenarios_v2.csv'))


d %>% mutate(variable=case_when(grepl('Load Factor', V1)~'Load Factor',
                                V3=='Month tons'~'t',
                                grepl('Ave', V3)~'FGC')) %>%
  filter(!is.na(variable),
         plant %in% c('Kendal', 'Kusile'), V2 %in% c('SO2', 'Actual'), value>0,
         Month>='2021-07-01', Month<'2022-07-01') %>%
    group_by(plant, variable) %>%
    summarise(across(value, mean)) ->
  kusile_so2

so2_uplift <- kusile_so2$value[kusile_so2$plant=='Kendal' & kusile_so2$variable=='FGC'] /
  kusile_so2$value[kusile_so2$plant=='Kusile' & kusile_so2$variable=='FGC']

base_cf <- kusile_so2$value[kusile_so2$plant=='Kusile' & kusile_so2$variable=='Load Factor']
base_emis <- kusile_so2$value[kusile_so2$plant=='Kusile' & kusile_so2$variable=='t']
modeled_emis <- emis$modeled_emissions[emis$plant=='Kusile' & emis$emitted_species=='SO2'] %>% unique

so2 <- tibble(scenario = c('low utilization 13 months', 'high utilization 13 months',
                           'low utilization 36 months', 'high utilization 36 months'),
              months=c(13, 13, 36, 36),
              cf=c(.33,2000/2400, .33, 2000/2400)) %>%
  mutate(so2 = base_emis*so2_uplift*cf/base_cf*months,
         so2_normal = base_emis*cf/base_cf*months,
         so2_excess = so2 - so2_normal,
         so2_multiplier = so2_excess/modeled_emis)



hia_cost %>% filter(scenario=='kusile_so2', year==2019) %>%
  group_by(outcome=outcome.long, pollutant, estimate, double_counted, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
  full_join(so2 %>% select(scenario, so2_multiplier), by=character()) %>%
  mutate(across(c(number, cost_mn_currentUSD), multiply_by, so2_multiplier)) ->
  kusile_excess_hia


kusile_excess_hia %>%
  mutate(cost_mn_ZAR = cost_mn_currentUSD*14.7912,
         number = ifelse(grepl('deaths', outcome) & !double_counted, number, 0),
         cost_mn_ZAR = ifelse(!double_counted, cost_mn_ZAR, 0)) %>%
  group_by(scenario, estimate) %>%
  summarise(across(c(number, cost_mn_ZAR), sum, na.rm=T)) %>%
  pivot_longer(is.numeric) %>% unite(name, name, estimate) %>% pivot_wider %>%
  write_csv(file.path(output_dir, 'kusile bypass HIA.csv'))

so2 %>% mutate(so2 = so2*12/months) %>%
  select(scenario, so2, so2_excess) %>%
  bind_rows(tibble(scenario='base period', so2=base_emis*12), .) %>%
  mutate(across(is.numeric, signif, 3)) %>%
  copy.xl()
