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

project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('so2', 'no2', 'ppm25', 'so4', 'no3')

# Load CALMET parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

#list csvs
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=5)
grid_raster = grids$gridR

#make tifs
scenarios_to_process=calpuff_files$scenario %>% unique
calpuff_files %>% filter(period=='annual', species %in% pollutants_to_process) %>% make_tifs(grids = grids, overwrite = F)

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)


# 02: Get base concentration levels -------------------------------------------------------------
conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
conc_base %>% saveRDS(file.path(output_dir, 'conc_base.RDS'))
conc_base <- readRDS(file.path(output_dir, 'conc_base.RDS'))

conc_base$conc_baseline[[which(conc_base$species=='no2')]] %<>% max(1)

names(conc_base$conc_baseline) <- conc_base$species

# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
#regions <- creahia::get_adm(grid_raster, admin_level=2, res="low")
shp=readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_low.RDS'))
regions <- creahia::get_adm(grid_raster, shp=shp, admin_level=2)


queue <- scenarios_to_process %>% paste0('exp_', .,'.csv') %>% file.path(output_dir, .) %>% file.exists() %>% not

causes_to_include = get_calc_causes() %>% grep('Death|YLD', ., value=T)

# HIA ###########################################################################################
#require(doFuture)
#registerDoFuture()
#future::plan("multisession", workers = 4)
#Sys.setenv(GIS_DIR='F:/gis')

region_ids <- regions %>% st_drop_geometry() %>% select(region_id)
pop <- creahia::get_pop(grid_raster)

for (scen in scenarios_to_process[queue]) {
  message(scen)
  # =============================== Get Perturbation Raster ========================================
  exposure_rasters <- calpuff_files  %>%
    filter(scenario==scen, period=='annual', species %in% pollutants_to_process)

  exposure_rasters$conc <- lapply(exposure_rasters$path, raster) %>% lapply(multiply_by, pop)

  exposure_rasters$conc %>% stack %>% raster::extract(regions, sum, na.rm=T) %>%
    as_tibble() %>% set_names(exposure_rasters$species) %>% bind_cols(region_id=region_ids, .) %>%
    write_csv(file.path(output_dir, paste0('exp_',scen,'.csv')))
}

concs <- conc_base

concs$conc_baseline %<>% lapply(function(r) r %>% subtract(1) %>% max(0))
concs$conc_baseline %>% lapply(function(r) { r[]<-1; r}) -> concs$conc_perturbation
pollutants_for_hia = conc_base$species# %>% c('tpm10')

# 04: HIA Calculations:
hia <-  wrappers.compute_hia_two_images(perturbation_rasters=conc_base$conc_perturbation,       # perturbation_rasters=raster::stack(perturbation_map)
                                        baseline_rasters=conc_base$conc_baseline,  # baseline_rasters=raster::stack(who_map)
                                        regions=regions,
                                        scenario_name='1ug',
                                        scale_base_year=2019,        # Population base year : reference year of INPUT data, for total epidemiological and total population
                                        scale_target_year=2022,      #Population target year
                                        crfs_version="C40",
                                        epi_version="C40",       # epi_version="C40"
                                        valuation_version="viscusi",
                                        return_concentrations=T,
                                        gbd_causes='default',
                                        calc_causes=causes_to_include,
                                        pm2.5_to_pm10_ratio=.7)


saveRDS(hia, file.path(project_dir, 'HIA', paste0('hia_GEMM_1ug.RDS')))

hia <- readRDS(file.path(project_dir, 'HIA', paste0('hia_GEMM_1ug.RDS')))

hia$hia %<>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3))


# 06: Compute and extract economic costs --------------------------------------------------------
hia_cost <- get_hia_cost(hia$hia, valuation_version="viscusi")

source('../creapuff/project_workflows/read_IESR_emissions.R')

targetyears = emis$year %>% unique
hia_fut <- hia_cost %>% get_econ_forecast(forecast_years = targetyears, reference_year = 2019)


#get costs and deaths per t emissions
require(pbapply)
scenarios_to_process %>%
  pblapply(function(run) {
    message(run)
    paste0('exp_',run,'.csv') %>% file.path(output_dir, .) %>%
      read_csv() %>%
      pivot_longer(-region_id, names_to='subspecies', values_to='exposure') %>%
      mutate(Pollutant=case_when(subspecies %in% c('so4', 'no3', 'ppm25')~'PM25',
                               T~toupper(subspecies)),
             emitted_species=case_when(subspecies %in% c('so2', 'so4')~'SOx',
                                       subspecies %in% c('no2','no3')~'NOx',
                                       subspecies=='ppm25'~'PM'),
             cluster=run) %>%
      left_join(modeled_emissions) %>%
      left_join(hia_fut %>% select(region_id, iso3, pop, Pollutant, Outcome, Cause, double_counted, year, estimate, unit, number, cost_mn_currentUSD)) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x * exposure/pop/modeled_emissions)) %>%
      group_by(cluster, year, Outcome, Cause, emitted_species, Pollutant, double_counted, estimate, unit) %>%
      summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T))
  }) %>% bind_rows %>% ungroup -> hia_per_t

hia_per_t %>% saveRDS(file.path(output_dir, 'hia_per_t.RDS'))
hia_per_t <- readRDS(file.path(output_dir, 'hia_per_t.RDS'))

emis %>% ungroup %>%
  filter(COD<=year, year_retire>=year,
         Status=='operating' | year>2022,
         year<=2040 | !grepl('1\\.5', scenario) | (Owner=='captive' & grepl('excluding captive', scenario))) %>%
  rename(emitted_species=pollutant) %>%
  inner_join(hia_per_t) %>%
  group_by(CFPP.name, Owner, province, region, cluster, Latitude, Longitude, scenario, Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
  mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum),
            across(c(MW, utilization), unique)) %>%
  mutate(across(c(number_per_GWh=number, cost_mn_currentUSD_per_GWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6)) ->
  hia_scenarios

hia_scenarios %>% saveRDS(file.path(output_dir, 'hia_scenarios.RDS'))
hia_scenarios <- readRDS(file.path(output_dir, 'hia_scenarios.RDS'))

hia_scenarios %>%
  group_by(scenario, Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) ->
  hia_scenarios_totals

hia_scenarios_totals %>% saveRDS(file.path(output_dir, 'hia_scenarios_totals.RDS'))
hia_scenarios_totals <- readRDS(file.path(output_dir, 'hia_scenarios_totals.RDS'))

hia_scenarios_totals %<>% filter(!double_counted, !grepl("economic costs", Outcome)) %>%
  group_by(scenario, year, estimate) %>%
  summarise(across(c(number=cost_mn_currentUSD), sum)) %>%
  mutate(Outcome = "economic costs", unit="million USD", double_counted = F) %>%
  bind_rows(hia_scenarios_totals %>% filter(!grepl("economic costs", Outcome)))

hia_scenarios_totals %<>% filter(!double_counted, grepl("Death", Outcome)) %>%
  group_by(scenario, year, estimate) %>%
  summarise(across(c(number), sum)) %>%
  mutate(Outcome = "deaths, total", Cause='AllCause', Pollutant='All', unit="death", double_counted = T) %>%
  bind_rows(hia_scenarios_totals %>% filter(Pollutant!='All'))

hia_scenarios_totals %>% ungroup %>%
  filter(estimate=='central', Outcome=='deaths, total',
         grepl('1\\.5 degrees($| /w APC| excluding captive$)|PERPRES.*2022$', scenario), year>=2010) %>%
  group_by(scenario, year, estimate) %>% summarise(across(number, sum)) %>%
  complete(scenario, year=2000:2061, estimate) %>% replace_na(list(number=0)) %>%
  write_csv(file.path(output_dir, 'Air pollution-related deaths by scenario.csv')) %>%
  ggplot(aes(year, number, col=scenario)) + geom_line(size=1) +
  theme_crea() + scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') +
  labs(title='Air pollution-related deaths by scenario')

hia_scenarios_totals %>% ungroup %>%
  filter(estimate=='central', !double_counted,
         grepl('1\\.5 degrees($| /w APC| excluding captive$)|PERPRES.*2022$', scenario), year>=2010) %>%
  group_by(scenario, year, estimate) %>% summarise(across(cost_mn_currentUSD, sum, na.rm=T)) %>%
  complete(scenario, year=2000:2061, estimate) %>% replace_na(list(number=0)) %>%
  write_csv(file.path(output_dir, 'Air pollution-related costs by scenario.csv')) %>%
  ggplot(aes(year, cost_mn_currentUSD, col=scenario)) + geom_line(size=1) +
  theme_crea() + scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') +
  labs(title='Air pollution-related costs by scenario', y='mln USD/year')

hia_scenarios_totals %>% filter(year==2022, grepl("PERPRES.*2022$", scenario)) %>%
  add_long_names() %>%
  select(scenario, Outcome=Outcome_long, Cause=Cause_long, Pollutant, unit, double_counted, estimate, number) %>%
  pivot_wider(names_from=estimate, values_from=number) %>%
  relocate(high, .after = low) %>%
  write_csv('annual HIA 2022.csv')


hia_scenarios_totals %>% filter(year>2023) %>%
  group_by(scenario, estimate, Outcome, Cause, Pollutant, double_counted, unit) %>%
  summarise(across(number, sum)) ->
  hia_cum

hia_cum %>% filter(estimate=='central', !double_counted, grepl('Death', Outcome)) %>%
  group_by(scenario, estimate) %>% summarise(across(number, sum)) %>%
  ggplot(aes(scenario, number)) + geom_col() +
  theme_crea() + #scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') + coord_flip()


hia_cum %>% filter(!grepl("cofiring", scenario)) %>%
  add_long_names() %>%
  select(scenario, Outcome=Outcome_long, Cause=Cause_long, Pollutant, unit, double_counted, estimate, number) %>%
  pivot_wider(names_from=estimate, values_from=number) %>%
  relocate(high, .after = low) %>%
  write_csv('annual HIA 2024 to end-of-life.csv')

#results by plant
hia_per_t %>%
  mutate(number=number*ifelse(grepl('Death', Outcome), 1, 0)) %>%
  filter(!double_counted) %>%
  group_by(cluster, emitted_species, year, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T)) ->
  hia_per_t_total

hia_scenarios_totals %>% filter(scenario=='BAU', year==pmax(2022, COD), Outcome %in% c('deaths, total', 'economic costs'))

#totals of deaths and costs by plant (current), by province (current and cumulative total), by scenario (cumulative total)


hia_totals %>% filter(year==2022, scenario=='BAU') -> hia_plants

hia_plants %>%
  ggplot(aes(x=Owner, y=cost_mn_currentUSD_per_GWh)) + geom_violin()

hia_plants %>%
  ggplot(aes(x=province, y=cost_mn_currentUSD_per_GWh)) + geom_violin() + coord_flip()

adm0 <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_0_coarse.RDS'))
require(ggspatial)
hia_plants %>%
  ggplot(aes(Longitude, Latitude)) +
  annotation_spatial(data=adm0) +
  geom_point(aes(col=Owner, size=cost_mn_currentUSD_per_GWh))



hia_per_t_total %>% write_csv(file.path(output_dir, 'hia_per_t_total.csv'))
hia_per_t_total %>% mutate(emitted_species = paste0('per_t_', emitted_species)) %>% pivot_wider(names_from=emitted_species, values_from=c(number, cost_mn_currentUSD)) %>%
  left_join(hia_plants, .) %>% write_csv(file.path(output_dir, 'hia_plants_total.csv'))


# valuations <- get_valuation('viscusi')
# Using new version of valuations. Might break things
valuations <- get_valuations_raw('viscusi')

usd_to_lcu=15447

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
