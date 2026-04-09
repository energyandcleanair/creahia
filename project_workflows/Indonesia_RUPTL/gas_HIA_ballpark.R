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

#project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)
project_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP'
project_dir_new='G:/Shared drives/CREA-HIA/Projects/Indonesia_RUPTL2025' ; if (!dir.exists(project_dir_new)) dir.create(project_dir_new)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir_new,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

gis_dir <- "~/GIS"                    # The folder where we store general GIS data
#gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

#setwd(get_gis_dir())
#system("gsutil rsync -r gs://crea-data/gis .")



# 06: Compute and extract economic costs --------------------------------------------------------
#source('../CALPUFF/creapuff/project_workflows/IndonesiaIESR/read_RUPTL_emissions.R')
source('../creapuff/project_workflows/IndonesiaIESR/read_RUPTL_emissions.R')

targetyears = emis$year %>% unique

hia_per_t <- readRDS(file.path(project_dir, 'HIA', 'hia_per_t.RDS'))

emis %>% filter(!is.na(cluster)) %>%
  group_by(cluster, province) %>%
  summarise(across(ends_with('itude'), mean)) -> clusters

read_xlsx("~/../Downloads/MASTER_Indonesia gas fleet_092025.xlsx") %>% set_names(make_names(names(.))) ->
  pltg

pltg %<>% mutate(across(matches('start_year|COD'), as.numeric)) %>% replace_na(list(start_year=2010))

#assign cluster by coordinates
pltg$cluster <- clusters$cluster[st_nearest_feature(pltg %>% to_sf_points(),
                                                    clusters %>% to_sf_points())]


bind_rows(pltg %>% filter(status=='operating') %>% mutate(category='existing'),
          pltg %>% filter(status!='operating' & !grepl('RUPTL', status)) %>% mutate(category='new - GEM'),
          pltg %>% filter(!is.na(ruptl_2025_re_base_cod)) %>% mutate(category='new - RUPTL-base', start_year=ruptl_2025_re_base_cod),
          pltg %>% filter(!is.na(ruptl_2025_ared_cod)) %>% mutate(category='new - RUPTL-ARED', start_year=ruptl_2025_ared_cod)) %>%
  group_by(category, cluster, start_year) %>%
  summarise(across(c(MW=contains('MW')), sum)) ->
  pltg_by_cluster

#1.04 g/kWh
#CEDS 0.9217522 g/kWh
#130 mg/Nm3
#150 mg/Nm3 = 0.2592 g/kWh
#400 mg/Nm3 * 0.8 = 0.55296 g/kWh
#conversion: g/kWh = fgc * 240 / .5 * 3600/1e9


NOx_g_kWh = tibble(emission_estimate=c('low - FGC compilation', 'high - national standard'),
                   emitted_species='NOx',
                   g_kWh=c(0.2592, 0.55296))

pltg_by_cluster %>%
  cross_join(NOx_g_kWh) %>%
  mutate(plant_lifetime = 30,
         capacity_factor = .5,
         emissions_t = MW * 8.76 * capacity_factor * g_kWh) %>%
  group_by(category, cluster, emitted_species, emission_estimate, start_year, plant_lifetime) %>% summarise(across(emissions_t, sum)) ->
  emis_by_cluster

emis_by_cluster %>% group_by(category, emitted_species, emission_estimate) %>% summarise(across(emissions_t, sum))

emis_by_cluster %>%
  group_by(start_year, plant_lifetime) %>%
  group_modify(function(df, group) {
    year_retire <- max(group$start_year+group$plant_lifetime-1, 2035)
    df %>% cross_join(tibble(year=year(today()):year_retire))
  }) %>%
  left_join(hia_per_t) ->
  hia_fut

bind_rows(hia_fut %>% mutate(period='2025 to end of life, cumulative'),
          hia_fut %>% filter(year==2030) %>% mutate(period='2030')) %>%
  mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
  filter(!double_counted) %>%
  group_by(category, period, Outcome, emission_estimate, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~signif(sum(.x), 2))) %>%
  summarise(across(c(number, cost_mn_currentUSD),
                   ~paste0(.x[estimate=='central'],
                           ' (', .x[estimate=='low'], ' - ',
                           .x[estimate=='high'], ')'))) ->
  hia_totals


hia_totals %>% filter(Outcome=='Deaths')
