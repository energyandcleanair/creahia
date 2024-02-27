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
options(reticulate.conda_binary = "C:\\ProgramData\\anaconda3\\Scripts\\conda.exe")
library(creapuff)
require(rcrea)
require(creahelpers)

#list.files(path='R', full.names=T) %>% sapply(source)
#list.files(path='~/CALPUFF/creapuff/R', full.names=T) %>% sapply(source)

project_dir="G:/Indonesia_smelters/"       # calpuff_external_data-2 persistent disk (project data)
met_dir="G:/IndonesiaIESR"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

#setwd(get_gis_dir())
#system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('so2', 'no2', 'pm25')

# Load CALMET parameters
calmet_result <- readRDS(file.path(met_dir, "calpuff_suite", "calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

#list csvs
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=5)
grid_raster = grids$gridR

#make tifs
scenarios_to_process=calpuff_files$scenario %>% grep('^all', ., value=T) %>% unique
calpuff_files %>% filter(period=='annual', species %in% pollutants_to_process,
                         scenario %in% scenarios_to_process) %>%
  data.frame %>% make_tifs(grids = grids, overwrite = T)

calpuff_files <- get_calpuff_files(ext=".tif$", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>%
  filter(period=='annual', species %in% pollutants_to_process, scenario %in% scenarios_to_process)


# 02: Get base concentration levels -------------------------------------------------------------
conc_base <- get_conc_baseline(species=c('pm25', 'no2', 'so2'), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
conc_base %>% saveRDS(file.path(output_dir, 'conc_base.RDS'))
conc_base <- readRDS(file.path(output_dir, 'conc_base.RDS'))

conc_base$conc_baseline[[which(conc_base$species=='no2')]] %<>% max(1)

names(conc_base$conc_baseline) <- conc_base$species

# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
#regions <- creahia::get_adm(grid_raster, admin_level=2, res="low")
shp=readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_low.RDS'))
regions <- creahia::get_adm(grid_raster, shp=shp, admin_level=2)


causes_to_include = get_calc_causes() %>% grep('Death|YLD', ., value=T)

# HIA ###########################################################################################
#require(doFuture)
#registerDoFuture()
#future::plan("multisession", workers = 6)
#Sys.setenv(GIS_DIR='F:/gis')

region_ids <- regions %>% st_drop_geometry() %>% select(region_id)

pop <- creahia::get_pop(grid_raster)



for (scen in scenarios_to_process) {
  message(scen)
  # =============================== Get Perturbation Raster ========================================
  conc_perturbation <- calpuff_files  %>%
    filter(scenario==scen, period=='annual', species %in% pollutants_to_process)

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
                                          scale_target_year=2022,      # 2025 # Population target year (same as no2_targetyear?)
                                          crfs_version="C40",
                                          epi_version="C40",       # epi_version="C40"
                                          valuation_version="viscusi",
                                          return_concentrations=T,
                                          gbd_causes="default",
                                          calc_causes=causes_to_include,
                                          pm2.5_to_pm10_ratio=.7
  ) # valuation_version="viscusi"


  saveRDS(hia, file.path(project_dir, 'HIA', paste0('hia_',scen,'.RDS')))
}

file.path(project_dir, 'HIA', paste0('hia_',scenarios_to_process,'.RDS')) %>%
  lapply(readRDS) -> hia_list

hia <- list()
hia_list %>% lapply('[[', 'hia') %>% bind_rows -> hia$hia

hia$hia %<>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3))


# 06: Compute and extract economic costs --------------------------------------------------------
hia_cost <- get_hia_cost(hia$hia, valuation_version="viscusi")

#targetyears = c(2020:2030, seq(2035, 2060, 5))
hia_fut <- hia_cost %>% group_by(concentration_year=force_numeric(scenario)) %>%
  group_modify(function(df, group) {
    targetyears = group$concentration_year
    if(targetyears==2030) targetyears=seq(2030, 2060, 5)
    get_econ_forecast(df, years=targetyears, pop_targetyr=2019) %>% filter(year!=2019)
  })

#hia_fut_indo <- hia_fut %>% filter(iso3=='IDN')
read_csv(file.path(output_dir, 'all health impacts by year and emitting province.csv')) -> hia_fut_bottomup

hia_fut_bottomup %<>%
  group_by(year, estimate) %>%
  add_total_deaths_and_costs()


hia_fut %>% left_join(shp@data %>% select(region_id=GID_2, GID_1, Province=NAME_1)) %>%
  group_by(country, iso3, GID_1, Province, Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) ->
  hia_fut_totals

hia_fut_totals %<>%
  group_by(country, iso3, GID_1, Province, year, estimate) %>%
  add_total_deaths_and_costs()

#adjust for totals from bottom up calculations
hia_fut_bottomup %>% mutate(calculation_type='bottom-up') %>%
  bind_rows(hia_fut_totals %>% mutate(calculation_type='top-down')) %>%
  filter(grepl('deaths, total', Outcome)) %>%
  group_by(calculation_type, Outcome, year, estimate) %>% summarise(across(number, sum)) %>%
  group_by(year) %>% filter(all(c('bottom-up', 'top-down') %in% calculation_type)) %>%
  group_by(Outcome, year, estimate) %>%
  summarise(ratio=number[calculation_type=='bottom-up']/number[calculation_type=='top-down']) %>%
  filter(year>2020) %>% group_by(estimate) %>% summarise(across(ratio, mean)) ->
  adj

hia_fut_totals %<>% left_join(adj) %>% mutate(across(c(number, cost_mn_currentUSD), ~.x*ratio)) %>%
  select(-ratio)

hia_fut_totals %>% ungroup %>%
  filter(estimate=='central', Outcome=='deaths, total', year==2030) %>%
  arrange(number) %>% slice_max(number, n=10) %>%
  mutate(Province=factor(Province, Province)) %>%
  ggplot(aes(Province, number)) +
  geom_col(fill='darkred') +
  theme_crea() +
  theme(legend.position = 'top', plot.margin = unit(c(.25,.25,.1,.1), 'in')) +
  labs(title='Air pollution-related deaths linked to smelters and captive power in 2030',
       subtitle='by affected province, due to emissions from the three studied provinces',
       y='cases per year', x='') +
  x_at_zero() -> plt
quicksave(file.path(output_dir, 'Air pollution-related deaths by province.png'), plot=plt, footer_height=.03)


hia_fut_totals %>% rename(affected_province=Province) %>% filter(country=='Indonesia') %>%
  add_long_names() %>%
  write_csv(file.path(output_dir, 'hia by affected province.csv'))


#comparison
by_affected <- read_csv(file.path(output_dir, 'hia by affected province.csv')) %>% mutate(name='by_affected')
by_emitting <- read_csv(file.path(output_dir, 'all health impacts by year and emitting province.csv')) %>% mutate(name='by_emitting')

bind_rows(by_affected, by_emitting) %>%
  filter(!double_counted) %>%
  mutate(number=ifelse(grepl('Death', Outcome), number, 0)) %>%
  group_by(name, year, estimate) %>%
  summarise(across(c(deaths=number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>%
  pivot_longer(c(deaths, cost_mn_currentUSD), names_to='var') %>%
  ggplot(aes(year, value, col=estimate, linetype=name)) + geom_line() + facet_wrap(~var)
