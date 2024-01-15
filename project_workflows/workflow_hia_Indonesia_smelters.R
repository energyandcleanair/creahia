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


pollutants_to_process=c('so2', 'no2', 'ppm25', 'pm10', 'so4', 'no3')

# Load CALMET parameters
calmet_result <- readRDS(file.path(met_dir, "calpuff_suite", "calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

#list csvs
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=5)
grid_raster = grids$gridR

#make tifs
scenarios_to_process=calpuff_files$scenario %>% unique
calpuff_files %>% filter(period=='annual', species %in% pollutants_to_process) %>% make_tifs(grids = grids, overwrite = F)

calpuff_files <- get_calpuff_files(ext=".tif$", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)


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

#build model on the effect of release height
read_csv(file.path(emissions_dir, 'emissions_with_cluster v2.csv')) -> emis
read_csv(file.path(emissions_dir, 'emissions_by_cluster.csv')) -> emis_cluster

calpuff_files %<>%
  mutate(loc_cluster=force_numeric(scenario),
         release_height=gsub('^[a-z]+[0-9]+', '', scenario))


calpuff_files %>%
  group_by(loc_cluster) %>%
  filter(all(c('l', 'm', 'h') %in% release_height)) ->
  height_model_inputs


emis_cluster %>%
  distinct(loc_cluster, lat, lon) %>%
  group_by(loc_cluster) %>%
  group_modify(function(df, group) {
    message(group)
    df %>% mutate(flag=1) %>%
      to_spdf %>%
      spTransform(crs(grids$gridR)) %>% rasterize(grids$gridR, 'flag') %>% raster::distance() %>% list() %>%
      tibble(df, r=.)
  }) -> dist_rasters

force_decreasing=function(x) { x %>% rev %>% pmax(., cummax(.)) %>% rev }

require(mgcv)
height_model_inputs %>%
  group_by(loc_cluster, species, period) %>%
  group_modify(function(df, group) {
    df$path %>% stack -> r
    r %<>% stack(dist_rasters$r[[which(dist_rasters$loc_cluster==group$loc_cluster)]])
    names(r) <- c(df$release_height, 'dist')
    r %>% as.data.frame() %>% mutate(l=l/2) %>% filter(l<sort(l, decreasing=T)[20]) -> r_df

    degs=case_when(group$species %in% c('no3', 'so2', 'ppm25')~2, T~3)

    lm(l~poly(pmin(.1, dist^-1), degs), data=r_df) -> m_l
    lm(m~poly(pmin(.1, dist^-1), degs), data=r_df) -> m_m
    lm(h~poly(pmin(.1, dist^-1), degs), data=r_df) -> m_h

    #r_df %>% slice_sample(n=2.5e4) %>% pivot_longer(-dist) %>%
    #  ggplot(aes(dist, value, col=name)) + geom_point(size=.5, alpha=.5) + geom_smooth()


    tibble(dist=1:1000) %>%
      mutate(l=predict(m_l, .), m=predict(m_m, .), h=predict(m_h, .),
             across(c(l,m,h), force_decreasing)) ->
      curves

    curves %>%
      pivot_longer(-dist) %>%
      mutate(name=recode(name, h='high', m='medium', l='low') %>% factor(levels=c('low', 'medium', 'high'))) %>%
      ggplot(aes(dist, value, col=name)) + geom_line(linewidth=1) + theme_crea() +
      labs(color='release height', y='µg/m3', x='distance from source, km', title=paste0(toupper(group$species), ' concentrations'),
           subtitle='as a function of distance and release height') +
      scale_color_manual(values=unname(crea_palettes$change[5:7])) -> p_curves
    quicksave(file.path(output_dir, paste0("release_height_correction_curves_", group$species, '_', group$period, '.png')), plot=p_curves)

    curves %>% mutate(l_to_m=l/m, h_to_m=h/m,
                      across(contains('to'), function(x) x %>% pmin(3) %>% pmax(.5))) %>%
      select(dist, contains('to')) -> ratios

    ratios %>%
      pivot_longer(-dist) %>%
      mutate(name=recode(name, l_to_m='low to medium', h_to_m='high to medium')) %>%
      ggplot(aes(dist, value, col=name)) + geom_line(linewidth=1)  + theme_crea() +
      labs(color='release heights', y='', x='distance from source, km', title=paste0(toupper(group$species), ' concentration ratios'),
           subtitle='as a function of distance and release height') +
      scale_color_manual(values=unname(crea_palettes$change[c(2,6)])) -> p_ratios
    quicksave(file.path(output_dir, paste0("release_height_correction_ratios_", group$species, '_', group$period, '.png')), plot=p_ratios)

    return(tibble(ratios=list(ratios)))
  }) -> release_height_ratios

clusters_to_process <- unique(calpuff_files$loc_cluster)
queue <- clusters_to_process %>% paste0('exp_cluster_', .,'.csv') %>% file.path(output_dir, .) %>% file.exists() %>% not

for (clust in clusters_to_process[queue]) {
  message(clust)
  # =============================== Get Perturbation Raster ========================================
  exposure_rasters <- calpuff_files %>%
    filter(loc_cluster==clust, period=='annual', species %in% pollutants_to_process)

  exposure_rasters$conc <- lapply(exposure_rasters$path, raster)

  distR <- dist_rasters$r[[which(dist_rasters$loc_cluster==unique(exposure_rasters$loc_cluster))[1]]]

  heights_to_process = c('l', 'h') %whichnotin% exposure_rasters$release_height

  for(rh in heights_to_process) {
    exposure_rasters %<>% filter(release_height=='m') %>% group_by(species, period) %>%
      group_modify(function(df, group) {
        release_height_ratios %>% inner_join(group) %>% use_series(ratios) %>% '[['(1) -> ratios
        df$conc[[1]] <- df$conc[[1]] * approx(ratios$dist, ratios[[paste0(rh,"_to_m")]], values(distR), rule=2)$y
        df %>% mutate(scenario=gsub('m$', rh, scenario), release_height=rh)
      }) %>% bind_rows(exposure_rasters)
  }

  exposure_rasters$conc %<>% lapply(multiply_by, pop)

  exposure_rasters$conc %>% stack %>% raster::extract(regions, sum, na.rm=T) %>%
    as_tibble() %>% set_names(paste0(exposure_rasters$species, '_', exposure_rasters$release_height)) %>%
    bind_cols(region_id=region_ids, .) %>%
    write_csv(file.path(output_dir, paste0('exp_cluster_',clust,'.csv')))
}


hia <- readRDS(file.path(met_dir, 'HIA', paste0('hia_GEMM_1ug.RDS')))

hia$hia %<>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause'~1,
                                     estimate=='central'~1/2,
                                     estimate=='low'~1/2,
                                     estimate=='high'~2/3))


# 06: Compute and extract economic costs --------------------------------------------------------
hia_cost <- get_hia_cost(hia$hia, valuation_version="viscusi")

targetyears = c(2020:2030, seq(2035, 2060, 5))
hia_fut <- hia_cost %>% get_econ_forecast(years=targetyears, pop_targetyr=2019)

hia_fut_indo <- hia_fut %>% filter(iso3=='IDN')

read_csv(file.path(emissions_dir, 'emissions_by_cluster.csv')) %>%
  dplyr::select(loc_cluster, emitted_species=pollutant, emissions_tpa_modeled=emissions_tpa) -> modeled_emissions

#get costs and deaths per t emissions
require(pbapply)
clusters_to_process %>%
  pblapply(function(run) {
    message(run)
    paste0('exp_cluster_',run,'.csv') %>% file.path(output_dir, .) %>%
      read_csv() %>%
      pivot_longer(-region_id, names_to='subspecies', values_to='exposure') %>%
      separate(subspecies, c('subspecies', 'release_height')) %>%
      mutate(Pollutant=case_when(subspecies %in% c('so4', 'no3', 'ppm25')~'PM25',
                               T~toupper(subspecies)),
             emitted_species=case_when(subspecies %in% c('so2', 'so4')~'SO2',
                                       subspecies %in% c('no2','no3')~'NOx',
                                       subspecies=='ppm25'~'PM2.5'),
             loc_cluster=run) %>%
      left_join(modeled_emissions) %>%
      left_join(hia_fut %>% select(region_id, iso3, pop, Pollutant, Outcome, Cause, double_counted, year, estimate, unit, number, cost_mn_currentUSD)) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x * exposure/pop/emissions_tpa_modeled)) %>%
      group_by(loc_cluster, release_height, year, Outcome, Cause, emitted_species, Pollutant, double_counted, estimate, unit) %>%
      summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T))
  }) %>% bind_rows %>% ungroup -> hia_per_t

hia_per_t %>% saveRDS(file.path(output_dir, 'hia_per_t.RDS'))
hia_per_t <- readRDS(file.path(output_dir, 'hia_per_t.RDS'))

emis %>% ungroup %>%
  full_join(tibble(year=targetyears), by = character()) %>%
  rename(emitted_species=pollutant) %>%
  group_by(loc_cluster, release_height, year) %>%
  group_modify(function(df, group) {
    message(group)
    df %>%
      inner_join(hia_per_t %>% inner_join(group)) %>% select(-all_of(names(group))) %>%
      group_by(Tracker.ID, smelter_company, Commodity_broad, Province, Latitude, Longitude, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_tpa)) %>%
      summarise(across(c(number, cost_mn_currentUSD), sum))
  }) -> hia_scenarios



hia_scenarios %>% saveRDS(file.path(output_dir, 'hia_scenarios.RDS'))
hia_scenarios <- readRDS(file.path(output_dir, 'hia_scenarios.RDS'))

hia_scenarios %<>% left_join(emis %>% distinct(Tracker.ID, smelter_company, Commodity_broad, MW, capacity_output_tpa, fuel, type, COD=Year))

hia_scenarios %>%
  group_by(type, Province, COD,
           Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) ->
  hia_scenarios_totals

hia_scenarios_totals %>% saveRDS(file.path(output_dir, 'hia_scenarios_totals.RDS'))
hia_scenarios_totals <- readRDS(file.path(output_dir, 'hia_scenarios_totals.RDS'))

hia_scenarios_totals %<>%
  group_by(type, Province, COD, year, estimate) %>%
  add_total_deaths_and_costs()

hia_scenarios_totals %>% ungroup %>%
  filter(estimate=='central', Outcome=='deaths, total', COD<year) %>%
  group_by(Province, year, estimate) %>% summarise(across(number, sum)) %>% ungroup %>%
  write_csv(file.path(output_dir, 'Air pollution-related deaths by scenario.csv')) %>%
  ggplot(aes(year, number, fill=Province)) +
  #facet_wrap(~type, scales='free_y') +
  geom_area() +
  theme_crea() + scale_fill_manual(values=unname(crea_palettes$dramatic), guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top', plot.margin = unit(c(.25,.25,.1,.1), 'in')) +
  labs(title='Air pollution-related deaths linked to smelters and captive power', y='cases per year', x='') +
  x_at_zero(labels=scales::comma) + snug_x -> plt
quicksave(file.path(output_dir, 'Air pollution-related deaths by scenario.png'), plot=plt, scale=1.1, footer_height=.03)

hia_scenarios_totals %>% ungroup %>%
  filter(estimate=='central', !double_counted, COD<year) %>%
  group_by(Province, year, estimate) %>% summarise(across(cost_mn_currentUSD, sum)) %>% ungroup %>%
  write_csv(file.path(output_dir, 'Air pollution-related costs by scenario.csv')) %>%
  ggplot(aes(year, cost_mn_currentUSD, fill=Province)) +
  geom_area() +
  theme_crea() + scale_fill_manual(values=unname(crea_palettes$dramatic), guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top', plot.margin = unit(c(.25,.25,.1,.1), 'in')) +
  labs(title='Air pollution-related costs linked to smelters and captive power', y='mln USD/year', x='') +
  x_at_zero(labels=scales::comma) + snug_x -> plt
quicksave(file.path(output_dir, 'Air pollution-related costs by scenario.png'), plot=plt, scale=1.1, footer_height=.03)


hia_scenarios_totals %>% filter(COD<year) %>%
  group_by(type, emitting_province=Province,
           Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>%
  write_csv(file.path(output_dir, 'all health impacts by year and emitting province.csv'))

hia_scenarios %>% ungroup %>% filter(year==2030, !double_counted) %>%
  mutate(across(matches('^deaths|^number'), ~.x * ifelse(grepl('Death', Outcome), 1, 0))) %>%
  group_by(smelter_company, Tracker.ID, Province, loc_cluster, Commodity_broad, capacity_output_tpa, captive_MW=MW, fuel, type, COD,
           Latitude, Longitude, year, estimate) %>%
  summarise(across(c(number, matches('^deaths|^cost|^number')), sum)) %>%
  rename(deaths=number) ->
  hia_plants

hia_plants %>% write_csv(file.path(output_dir, 'hia by plant, 2030.csv'))

hia_plants %>% group_by(smelter_company, estimate) %>%
  summarise(across(matches('^deaths|^cost'), ~sum(.x, na.rm=T))) %>%
  arrange(-deaths) %>% filter(estimate=='central') ->
  plant_ranking

hia_plants %>% mutate(Commodity_broad=ifelse(type=='captive power', 'Captive power', Commodity_broad)) %>%
  mutate(smelter_company=factor(smelter_company, levels=plant_ranking$smelter_company %>% rev)) %>%
  filter(smelter_company %in% plant_ranking$smelter_company[1:15], estimate=='central') %>%
  ggplot(aes(smelter_company, deaths, fill=Commodity_broad)) + geom_col() +
  coord_flip() +
  scale_fill_manual(values=unname(crea_palettes$dramatic[c(1,2,6)])) +
  theme_crea() + x_at_zero() +
  labs(title='Smelter companies with the largest projected health impact',
       y='deaths per year linked to air pollution', x='', fill='Commodity') -> p
quicksave(file.path(output_dir, 'Smelter companies with the largest projected health impact.png'), plot=p)

hia_plants %>% to_spdf %>% raster::extract(pop, ., fun=mean, buffer=100) -> hia_plants$population_density_100km
hia_plants %>% ggplot(aes(population_density_100km, deaths_per_TWh))+geom_point()

hia_plants %>%
  ggplot(aes(x=Owner, y=cost_mn_currentUSD_per_TWh)) + geom_violin()

hia_plants %>%
  ggplot(aes(x=province, y=cost_mn_currentUSD_per_TWh)) + geom_violin() + coord_flip()

adm0 <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_0_coarse.RDS'))
require(ggspatial)
hia_plants %>%
  ggplot(aes(Longitude, Latitude)) +
  annotation_spatial(data=adm0) +
  geom_point(aes(col=Owner, size=cost_mn_currentUSD_per_TWh))








#valuations <- read_csv('~/Rpackages/creahia/inst/extdata/valuation_viscusi.csv')
valuations <- get_valuation('viscusi')

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


#get impacts by affected province and emitting plant
hia_fut %>% ungroup %>%
  filter(year==2022, !double_counted) %>%
  mutate(across(number, ~.x * ifelse(grepl('Death', Outcome), 1, 0))) %>%
  group_by(region_id, iso3, pop, Pollutant, year, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T)) ->
  hia_fut_totals

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
      left_join(hia_fut_totals) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x * exposure/pop/modeled_emissions)) %>%
      select(cluster, year, Pollutant, region_id, estimate, number, cost_mn_currentUSD)
  }) %>% bind_rows %>% ungroup -> region_hia_per_t

shp@data %>% select(region_id=GID_2, affected_province=NAME_1) %>%
  right_join(region_hia_per_t) %>%
  group_by(cluster, affected_province, emitted_species, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) ->
  province_hia_per_t

emis %>% ungroup %>%
  filter(scenario=='BAU') %>%
  inner_join(plant_cod) %>%
  rename(emitted_species=pollutant) %>%
  group_by(cluster) %>%
  group_modify(function(df, group) {
    message(group)
    df %>%
      inner_join(province_hia_per_t %>% filter(cluster==group$cluster)) %>%
      mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
      group_by(CFPP.name, Owner, province, affected_province, Latitude, Longitude, scenario, year, estimate) %>%
      summarise(across(c(number, cost_mn_currentUSD), sum))
  }) -> province_hia_plants

province_hia_plants %>% group_by(emitting_province=province, affected_province, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum, na.rm=T)) %>%
  pivot_wider(names_from=estimate, values_from=c(number, cost_mn_currentUSD)) %>%
  arrange(-number_central) -> blame_matrix

blame_matrix %>% write_csv(file.path(output_dir, 'province blame matrix.csv'))

blame_matrix %>% group_by(province=emitting_province) %>%
  summarise(across(matches('number|cost_'), sum, na.rm=T)) %>%
  slice_max(number_central, n=10) %>% arrange(-number_central) %>%
  mutate(province=factor(province, province)) %>%
  ggplot(aes(province, number_central)) + geom_col(aes(fill=number_central)) +
  geom_errorbar(aes(ymin=number_low, ymax=number_high), width=.2) +
  theme_crea() + theme(axis.text.x = element_text(angle=15, hjust=1)) +
  scale_fill_gradientn(colors=crea_palettes$change[5:7], guide='none') +
  x_at_zero() +
  labs(title='Provinces responsible for largest health toll',
       subtitle='Top 10 provinces: Air pollution deaths linked to coal power plants located in each province',
       y='cases/year', x='') -> plt
quicksave(file.path(output_dir, 'Provinces responsible for largest health toll.png'), plot=plt)

blame_matrix %>% group_by(province=affected_province) %>%
  summarise(across(matches('number|cost_'), sum, na.rm=T)) %>%
  slice_max(number_central, n=10) %>% arrange(-number_central) %>%
  mutate(province=factor(province, province)) %>%
  ggplot(aes(province, number_central)) + geom_col(aes(fill=number_central)) +
  geom_errorbar(aes(ymin=number_low, ymax=number_high), width=.2) +
  theme_crea() + theme(axis.text.x = element_text(angle=15, hjust=1)) +
  scale_fill_gradientn(colors=crea_palettes$change[5:7], guide='none') +
  x_at_zero() +
  labs(title='Provinces most affected by coal power emissions',
       subtitle='Top 10 provinces: Air pollution deaths linked to coal power pollution taking place in each province',
       y='cases/year', x='') -> plt
quicksave(file.path(output_dir, 'Provinces most affected by coal power emissions.png'), plot=plt)


