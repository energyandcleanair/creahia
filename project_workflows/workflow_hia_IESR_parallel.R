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
  mutate(number = number * case_when(pollutant != 'NO2' | cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3))


# 06: Compute and extract economic costs --------------------------------------------------------
hia_cost <- get_hia_cost(hia$hia, valuation_version="viscusi")

source('../creapuff/project_workflows/read_IESR_emissions.R')

targetyears = emis$year %>% unique
hia_fut <- hia_cost %>% get_econ_forecast(forecast_years = targetyears, reference_year = 2019)
hia_fut %>% saveRDS(file.path(output_dir, 'hia_fut.RDS'))

#get costs and deaths per t emissions
require(doFuture)
require(foreach)
registerDoFuture()
future::plan("multisession", workers = 8)

rm(hia_fut)
foreach (run = scenarios_to_process) %dopar% ({
  message(run)
  hia_fut <- readRDS(file.path(output_dir, 'hia_fut.RDS'))

  paste0('exp_',run,'.csv') %>% file.path(output_dir, .) %>%
    read_csv() %>%
    pivot_longer(-region_id, names_to='subspecies', values_to='exposure') %>%
    mutate(pollutant=case_when(subspecies %in% c('so4', 'no3', 'ppm25')~'PM25',
                               T~toupper(subspecies)),
           emitted_species=case_when(subspecies %in% c('so2', 'so4')~'SOx',
                                     subspecies %in% c('no2','no3')~'NOx',
                                     subspecies=='ppm25'~'PM'),
           cluster=run) %>%
    left_join(modeled_emissions) %>%
    left_join(hia_fut %>% select(region_id, iso3, pop, pollutant, outcome, cause, double_counted, year, estimate, unit, number, cost_mn_currentUSD)) %>%
    mutate(across(c(number, cost_mn_currentUSD), ~.x * exposure/pop/modeled_emissions)) %>%
    group_by(cluster, year, outcome, cause, emitted_species, pollutant, double_counted, estimate, unit) %>%
    summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T)) -> result

  result %>% saveRDS(file.path(output_dir, paste0('hia_per_t_', run, '.RDS')))
})



hia_per_t %>%
  mutate(number=number*ifelse(grepl('Death', outcome), 1, 0)) %>%
  filter(!double_counted) %>%
  group_by(cluster, emitted_species, year, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T)) ->
  hia_per_t_total

emis %>% filter(COD<=year, year_retire>=year,
                Status=='operating' | year>2022,
                year<=2040 | !grepl('1\\.5', scenario) | Owner=='captive',
                year<=2040 | !grepl('1\\.5.*captive', scenario)) %>%
  rename(emitted_species=pollutant) %>%
  inner_join(hia_per_t) %>%
  group_by(CFPP.name, Owner, province, region, cluster, Latitude, Longitude, scenario, outcome, cause, pollutant, double_counted, year, estimate, unit) %>%
  mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum),
            across(c(MW, utilization), unique)) %>%
  mutate(across(c(number_per_GWh=number, cost_mn_currentUSD_per_GWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6)) ->
  hia_scenarios

hia_scenarios %>% ungroup %>%
  filter(estimate=='central', !double_counted, grepl('Death', outcome),
         grepl('1\\.5|PERPRES.*2022$', scenario)) %>%
  group_by(scenario, year, estimate) %>% summarise(across(number, sum)) %>%
  ggplot(aes(year, number, col=scenario)) + geom_line(size=1) +
  theme_crea() + scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') +
  labs(title='Air pollution-related deaths by scenario')

hia_scenarios %>% filter(year>=2023) %>%
  group_by(scenario, estimate, outcome, cause, pollutant, double_counted, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
  add_long_names() ->
  hia_cum

hia_cum %>% filter(estimate=='central', !double_counted, grepl('Death', outcome)) %>%
  group_by(scenario, estimate) %>% summarise(across(number, sum)) %>%
  ggplot(aes(scenario, number)) + geom_col() +
  theme_crea() + #scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') + coord_flip()


hia_cum %>% filter(!double_counted) %>% group_by(scenario, estimate) %>%
  summarise(across(c(number=cost_mn_currentUSD), sum)) %>%
  mutate(outcome_long = "economic costs")
  bind_rows(hia_cum)

hia_cum %>%
  select(-cost_mn_currentUSD) %>%
  pivot_longer(names_from=estimate, values_from=number)


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



valuations <- get_valuations_raw('viscusi')

usd_to_lcu=15447

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




hia_fut %>% add_long_names() %>%
  group_by(outcome=outcome_long, cause=Cause_long, pollutant, double_counted, scenario, estimate) %>%
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

hia_fut %>%
  filter(outcome=='Deaths', !double_counted, estimate=='central') %>%
  group_by(scenario, province=NAME_1) %>%
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
  filter(outcome=='Deaths', !double_counted, estimate=='central',
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
  filter(outcome=='Deaths', !double_counted, estimate=='central',
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
    add_long_names() %>% select(-outcome, -cause) %>% rename(outcome=outcome_long, cause=Cause_long)

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


