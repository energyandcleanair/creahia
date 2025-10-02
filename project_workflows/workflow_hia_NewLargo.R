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

project_dir="H:/NewLargo"       # calpuff_external_data-2 persistent disk (project data)

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")


emis <- read_xlsx(file.path(emissions_dir, 'emission_NewLargo.xlsx'), sheet='CALPUFF input')

#gis_dir <- "~/GIS"                    # The folder where we store general GIS data
gis_dir <- "H:/gis"

creahia::set_env('gis_dir',gis_dir)
Sys.setenv(gis_dir=gis_dir)

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('NO2', 'PM2.5', 'PM10', 'SO2')
pollutants_to_process=c('PM2.5')

# Load CALMET parameters
#calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
calmet_result <- readRDS(file.path("H:/SouthAfrica/calpuff_suite/calmet_result.RDS" ))
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
conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)
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

# emis data has a lifetime collumn

#read HIA data
hia <- runs %>% lapply(function(scen) readRDS(file.path(project_dir, 'HIA', paste0('hia_GBD__',scen,'.RDS')))$hia) %>% bind_rows


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


#regions <- hia_totals$iso3 %>% unique
#regions
#
#
#hia_totals %>% group_by(scenario, pollutant) %>% filter(!double_counted, outcome=='Deaths', estimate=='central') %>%
#  summarise(across(number, sum, na.rm=T))
#
#hia_totals %>% filter(iso3!='ZWE')

hia_totals  %>% group_by(scenario, pollutant) %>% filter(!double_counted, outcome=='Deaths', estimate=='central') %>%
  #summarise(across(number, sum, na.rm=T))
  summarise(across(number, ~sum(.x, na.rm=T), na.rm=T))



# 06: Compute and extract economic costs --------------------------------------------------------
# TODO : change name scale_target_year -> pop_target_year

targetyears = c(seq(2020,2037,1))

hia_cost <- get_hia_cost(hia=hia, valuation_version="viscusi")
valuations <- get_valuations_raw('viscusi')

#usd_to_lcu=15447
usd_to_lcu=14.7912

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

hia_fut <- get_econ_forecast(hia_cost, forecast_years = targetyears, reference_year = 2019)



# adm <- creahelpers::get_adm(level = 2, res='coarse')


hia_fut %>%
  left_join(hia_cost %>% distinct(outcome, cause, pollutant, double_counted)) %>%
  group_by(scenario, outcome) %>%
  #summarise(across(c(number, cost_mn_currentUSD), sum))
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T)))



years = list('f1' = 2022:2037, 'f2' = 2022:2037, 'f3' = 2022:2037,
                               'd2' = 2020:2024, 'd3' = 2020:2024,
                               'h2' = 2023:2032, 'h3' = 2023:2032)

#summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>%

# cumulative  integrated over time and space
for(x in names(years)){
  hia_fut %>% filter(scenario == x) %>%
    filter(year %in% years[[x]], pollutant != 'NO2' | cause != 'AllCause') %>%
    group_by(scenario, estimate, outcome, cause, pollutant) %>%
    summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
    left_join(hia %>% distinct(scenario, outcome, cause, pollutant, double_counted)) %>%
    mutate(double_counted = ifelse(pollutant=='NO2', F, double_counted)) %>%
    add_long_names %>% ungroup %>% select(-outcome, -cause) %>% rename(cause=cause_long, outcome=outcome_long) %>%
    pivot_longer(c(number, cost_mn_currentUSD)) %>%
    spread(estimate, value) %>%
    arrange(desc(name), scenario, outcome!='deaths', double_counted) %>%
    select(scenario, outcome, cause, pollutant,  central, low, high, variable=name, double_counted) %>%
    write_csv(file.path(output_dir, glue::glue('{x}_Cumulative.csv')))
}


# for calulcting risk 7 integrated over time but not space
for(x in names(years)){
  hia_fut %>% filter(scenario == x) %>%
    filter(year %in% years[[x]], pollutant != 'NO2' | cause != 'AllCause') %>%
    group_by(scenario, estimate, outcome, cause, pollutant) %>%
    summarise(number = sum(number), pop = mean(pop), cost_mn_currentUSD=sum(cost_mn_currentUSD)) %>%
    left_join(hia %>% distinct(scenario, outcome, cause, pollutant, double_counted, pop, region_name, region_id)) %>%
    mutate(double_counted = ifelse(pollutant=='NO2', F, double_counted)) %>%
    add_long_names %>% ungroup %>% select(-outcome, -cause) %>% rename(cause=cause_long, outcome=outcome_long) %>%
    pivot_longer(c(number, cost_mn_currentUSD)) %>%
    spread(estimate, value) %>%
    arrange(desc(name), scenario, outcome!='deaths', double_counted) %>%
    select(scenario, outcome, cause, pollutant, central, low, high, variable=name, double_counted, pop, region_name, region_id) %>%
    write_csv(file.path(output_dir, glue::glue('{x}_Risk.csv')))
}


################################################################################################










# 06: Compute and extract economic costs --------------------------------------------------------
targetyears = 2022


hia_cost <- get_hia_cost(hia=hia_totals, valuation_version="viscusi")

valuations <- get_valuations_raw('viscusi')

#usd_to_lcu=15447
usd_to_lcu=14.7912

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

hia_totals %>% filter(!double_counted) %>% group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm=T)) %>%
  pivot_longer(is.numeric, names_to='outcome', values_to='number') %>%
  bind_rows(hia_totals) %>% select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
  write_csv(file.path(output_dir, 'HIA results.csv'))

# 06: Compute and extract economic costs --------------------------------------------------------



# 0stopped here -------------------------------------------------------

hia_fut %>%
  filter(outcome=='Deaths', !double_counted, estimate=='central') %>%
  group_by(scenario, province=NAME_1) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) ->
  plotdata



# stop here maybe









make_nice_numbers <- function(df, sigdigs=3, accuracy=1, columns=c('number', 'central', 'low', 'high')) {
  df %>% mutate(across(any_of(columns),
                       function(x) {
                         x %<>% signif(sigdigs)
                         ifelse(grepl('mln|bln', outcome) & !grepl('USD', outcome),
                                scales::comma(x, accuracy=accuracy/100),
                                scales::comma(x, accuracy=accuracy))
                       }))
}

output_tables <- function(hiadata, output_name='', rounding_function=make_nice_numbers,
                          bad_scenario='Eskom plan', good_scenario='compliance') {
  hiadata %<>% filter(!double_counted, !grepl('YLLs|LBW', outcome)) %>%
    add_long_names() %>% select(-outcome, -cause) %>% rename(outcome=outcome_long, cause=cause_long)

  hiadata %>% group_by(scenario, estimate) %>%
    summarise(across(cost_mn_currentUSD, sum, na.rm=T)) %>%
    rename(number=cost_mn_currentUSD) %>%
    mutate(outcome = 'total economic cost, mln USD',
           pollutant='all', cause='all', double_counted=F) -> cost_totals

  hiadata %>% filter(grepl('deaths', outcome)) %>%
    group_by(scenario, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(outcome = 'deaths',
           pollutant='all', cause='all causes', double_counted=F) -> death_totals

  hiadata %>% filter(grepl('disabi', outcome)) %>%
    group_by(scenario, outcome, pollutant, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(cause = 'all causes', double_counted=F) -> yld_totals

  hiadata %>% filter(grepl('deaths', outcome), pollutant=='PM2.5') %>%
    group_by(scenario, estimate, pollutant) %>%
    summarise(across(number, sum)) %>%
    mutate(outcome = 'deaths', cause='all causes', double_counted=T) -> pm25_death_totals

  hiadata$double_counted[grepl('disabi', hiadata$outcome)] <- T

  hiadata %>%
    filter(!grepl('prevalence', outcome)) %>%
    mutate(double_counted = grepl('deaths', outcome),
           across(cause, tolower)) %>%
    group_by(scenario, outcome, cause, pollutant, double_counted, estimate) %>%
    summarise(across(number, sum)) %>%
    bind_rows(cost_totals,
              pm25_death_totals,
              death_totals,
              yld_totals) -> hia_out

  hia_out %<>% mutate(number = number * ifelse(grepl('absence', outcome), 1e-6, 1),
                      cause = ifelse(outcome==cause, '', cause),
                      outcome = ifelse(grepl('absence', outcome), 'work absence (mln sick leave days)', outcome))

  if(good_scenario=='nocoal')
    hia_out %<>% filter(scenario=='Eskom plan') %>%
    mutate(number=0, scenario='nocoal') %>%
    bind_rows(hia_out)

  hia_out %<>% filter(grepl('economic', outcome)) %>%
    mutate(number=number*usd_to_lcu/1000,
           outcome='total economic cost, bln R') %>%
    bind_rows(hia_out)

  hia_out %>%
    group_by(outcome, cause, pollutant, estimate, double_counted) %>%
    summarise(number=number[scenario==bad_scenario] - number[scenario==good_scenario]) %>%
    spread(estimate, number) %>%
    arrange(!grepl('deaths', outcome), !grepl('asthma', outcome), !grepl('births', outcome), grepl('economic', outcome),
            outcome, pollutant != 'all', pollutant!='PM2.5', double_counted, cause, pollutant) %>%
    select(outcome, cause, pollutant, central, low, high, double_counted) %>% filter(!is.na(outcome)) ->
    hia_avoided

  hia_avoided %>%
    filter(!(pollutant=='PM2.5' & cause=='all causes')) %>%
    rounding_function %>%
    write_csv(file.path(output_dir,
                        paste0('avoided health impacts, ',good_scenario,' vs ',
                               bad_scenario,', ',output_name,'.csv')))

  hia_out %>% ungroup %>% filter(pollutant == 'all') %>%
    rounding_function %>%
    spread(estimate, number) %>%
    arrange(outcome, scenario) %>%
    select(scenario, outcome, central, low, high) %T>%
    print() %>%
    write_csv(file.path(output_dir, paste0('deaths and total costs, all scenarios, ',output_name,'.csv')))

  hia_avoided %<>% rounding_function(sigdigs=2, accuracy=10)

  statements <-character()

  hia_avoided %>% filter(!(outcome == 'deaths' & cause != 'all causes')) %>%
    mutate(statement=case_when(outcome=='deaths' & pollutant=='all'~
                                 paste0(central, ' ', outcome, ' (95% confidence interval: ',low, ' – ', high,')', ', of which '),
                               grepl('disabi', outcome) & cause=='all causes'~paste0(central, ' ', outcome, ', of which '),
                               outcome=='deaths' & cause=='all causes'~paste0(central, ' due to exposure to ', pollutant, ', '),
                               grepl('disability', outcome)~paste0(central, ' due to ', cause, ', '),
                               grepl('USD', outcome)~paste0('total economic costs of $', central, 'mln'),
                               grepl('absence', outcome)~paste0(central, ' million days of work absence, '),
                               T~paste0(central, ' ', outcome, ', '))) %>%
    use_series(statement) %>% paste(collapse='') %>%
    c(statements, .) -> statements

  hia_avoided %>% filter(outcome == 'deaths' & cause != 'all causes') %>%
    (function(df) {
      c(paste0('Of the deaths caused by PM2.5 exposure, ', df$central[1], ' are attributed to ', df$cause[1]),
        paste0(df$central[-1], ' to ', df$cause[-1])) %>% paste(collapse=', ')
    }) %>% c(statements, .) -> statements

  hia_out %>% filter(pollutant=='all') %>%
    group_by(outcome, estimate) %>%
    mutate(number=number[scenario==bad_scenario] - number) %>%
    spread(estimate, number) %>% filter(central>0) %>%
    make_nice_numbers() %>%
    arrange(!grepl('deaths', outcome)) %>%
    group_by(scenario) %>%
    summarise(statement = paste0('the ', scenario[1], ' scenario would avoid a projected ', central[1],
                                 ' deaths from air pollution (95% confidence interval: ',
                                 low[1], ' – ', high[1], ')',
                              ' and economic costs of USD', central[2], ' million',
                              ' (95% confidence interval: ', low[2], ' – ', high[2], ')'),
              central=central[1]) %>%
    arrange(central) %>% use_series(statement) %>%
    paste(collapse='; ') %>% paste('Compared to the', bad_scenario, 'scenario,', .) %>%
    c(statements, .) -> statements

  statements %>%
    c(paste('Compared to the', bad_scenario, 'scenario, the', good_scenario, 'scenario would avoid...'), .) %T>%
    print() %>%
    writeLines(file.path(output_dir, paste0(good_scenario, ' vs ', bad_scenario, ', ', output_name, '.txt')))
}


