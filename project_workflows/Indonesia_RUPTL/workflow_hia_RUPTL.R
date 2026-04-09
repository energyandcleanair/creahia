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

emis %>% ungroup %>%
  filter(COD<=year, year_retire>=year,
         Status=='operating' | year>2022,
         year<=2040 | !grepl('1\\.5', scenario) | (Owner=='captive' & grepl('excluding captive', scenario))) %>%
  rename(emitted_species=pollutant) %>%
  group_by(cluster, year) %>%
  group_modify(function(df, group) {
    message(group)
    suppressMessages(
      df %>%
        inner_join(hia_per_t %>% filter(cluster==group$cluster, year==group$year)) %>%
        group_by(CFPP.name, Owner, province, region, cluster, Latitude, Longitude, scenario, Outcome, Cause, Pollutant, double_counted, year, estimate, unit) %>%
        mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
        summarise(across(c(number, cost_mn_currentUSD), sum),
                  across(c(MW, utilization), unique)) %>%
        ungroup %>% select(-cluster, -year)
    )
  }) %>%
  mutate(across(c(number_per_TWh=number, cost_mn_currentUSD_per_TWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6)) ->
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
         !grepl('excluding', scenario)) %>%
  group_by(scenario, year, estimate) %>% summarise(across(number, sum)) %>% ungroup %>%
  complete(scenario, year=2000:2056, estimate) %>% replace_na(list(number=0)) %>%
  filter(year>=2010, !grepl('1.5', scenario) | number < 1e4) %>%
  write_csv(file.path(output_dir, 'Air pollution-related deaths by scenario.csv')) %>%
  ggplot(aes(year, number, col=scenario)) + geom_line(linewidth=1) +
  theme_crea() + scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') +
  labs(title='Air pollution-related deaths by scenario', y='cases per year', x='') +
  x_at_zero() + snug_x
quicksave(file.path(output_dir, 'Air pollution-related deaths by scenario.png'), scale=1)

hia_scenarios_totals %>% ungroup %>%
  filter(estimate=='central', !double_counted,
         !grepl('excluding', scenario), year>=2010) %>%
  group_by(scenario, year, estimate) %>% summarise(across(cost_mn_currentUSD, sum, na.rm=T)) %>% ungroup %>%
  complete(scenario, year=2000:2056, estimate) %>% replace_na(list(cost_mn_currentUSD=0)) %>%
  filter(year>=2010, !grepl('1.5', scenario) | cost_mn_currentUSD < 7500) %>%
  write_csv(file.path(output_dir, 'Air pollution-related costs by scenario.csv')) %>%
  ggplot(aes(year, cost_mn_currentUSD, col=scenario)) + geom_line(size=1) +
  theme_crea() + scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  theme(legend.position = 'top') +
  labs(title='Air pollution-related costs by scenario', y='mln USD/year', x='') +
  x_at_zero() + snug_x
quicksave(file.path(output_dir, 'Air pollution-related costs by scenario.png'), scale=1)

hia_scenarios_totals %>% filter(year>2024) %>%
  group_by(scenario, estimate, Outcome, Cause, Pollutant, double_counted, unit) %>%
  summarise(across(number, ~sum(.x, na.rm=T))) ->
  hia_cum

hia_cum %>% filter(estimate=='central', !double_counted, grepl('Death', Outcome),
                   !grepl('excluding', scenario)) %>%
  group_by(scenario, estimate) %>% summarise(across(number, sum)) %>%
  copy.xl

hia_cum %>% filter(!grepl("cofiring|excluding", scenario)) %>%
  add_long_names() %>%
  select(scenario, Outcome=Outcome_long, Cause=Cause_long, Pollutant, unit, double_counted, estimate, number) %>%
  pivot_wider(names_from=estimate, values_from=number) %>%
  relocate(high, .after = low) %>%
  write_csv(file.path(output_dir, 'cumulative HIA 2025 to end-of-life.csv'))



#totals of deaths and costs by plant (current), by province (current and cumulative total), by scenario (cumulative total)
hia_scenarios_totals %>% filter((grepl("PERPRES.*2022$", scenario) & year==2022) | year>=2025,
                                !grepl("cofiring|excluding", scenario),
                                Outcome %in% c('deaths, total', 'economic costs')) %>%
  pivot_wider(names_from=estimate, values_from=number) %>%
  relocate(high, .after = low) %>%
  write_csv(file.path(output_dir, 'deaths and total costs by year, all scenarios.csv'))

hia_scenarios_totals %>% filter(year>=2025,
                                !grepl("cofiring|excluding", scenario),
                                Outcome %in% c('deaths, total', 'economic costs')) %>%
  group_by(scenario, Outcome, unit, estimate) %>% summarise(across(number, sum)) %>%
  pivot_wider(names_from=estimate, values_from=number) %>%
  relocate(high, .after = low) %>%
  write_csv(file.path(output_dir, 'deaths and total costs cumulative, all scenarios.csv'))

emis %>% ungroup %>% distinct(CFPP.name, new_in_ruptl) %>%
  left_join(hia_scenarios) %>%
  filter(new_in_ruptl, year==2034, grepl('RUPTL 2025', scenario),
         !double_counted) %>%
  group_by(CFPP.name, estimate) %>%
  summarise(deaths=sum(number[Outcome=='Deaths']),
            across(cost_mn_currentUSD, sum)) %>%
  write_csv(file.path(output_dir, 'deaths and total costs from new plants in RUPTL, per year.csv'))

emis %>% ungroup %>% distinct(CFPP.name, new_in_ruptl) %>%
  left_join(hia_scenarios) %>%
  filter(new_in_ruptl, grepl('RUPTL 2025', scenario),
         !double_counted) %>%
  group_by(CFPP.name, estimate) %>%
  summarise(deaths=sum(number[Outcome=='Deaths']),
            across(cost_mn_currentUSD, sum)) %>%
  write_csv(file.path(output_dir, 'deaths and total costs from new plants in RUPTL, cumulative.csv'))
