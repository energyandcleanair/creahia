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
source('../creapuff/R/02_calpuff.R') #library(creapuff)
source('../creapuff/R/helpers.R') #library(creapuff)
require(rcrea)
require(creahelpers)

source('project_workflows/eskom_helpers.R')

#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="G:/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
hia_dir <- file.path(project_dir,"HIA_MES")
output_dir = file.path(project_dir, 'HIA_IRP_2024') ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

source('project_workflows/emissions_processing_SA_v3.R')


emis <- read_csv(file.path(emissions_dir, 'emissions scaling for scenarios_v2.csv'))
hia_fut <- readRDS(file.path(hia_dir, 'hia_fut.RDS'))

emission_reductions %>% ungroup %>% distinct(plant, decommissioning_start, decommissioning_end) %>%
  left_join(plantdata %>% ungroup %>% distinct(plant, MW=plant_MW)) -> decomm

read_xlsx(file.path(emissions_dir, "Eskom shutdown plan.xlsx")) %>%
  pivot_longer(-...1, names_to='plant', values_to='decommissioning_date') %>%
  mutate(across(decommissioning_date, date),
         scenario='Eskom Shutdown Plan 2023') %>% na.omit ->
  decomm_dates

decomm_dates$plant %<>% recode(Grootviel='Grootvlei', Matia='Matla', Medupl='Medupi')

decomm %>% distinct(plant, MW) %>% full_join(decomm_dates, .) -> decomm_dates

decomm_dates %<>% group_by(plant) %>% mutate(MW_plant=as.numeric(MW), MW=MW_plant/n())

#add delay scenario
days_in_10_years = ymd('2045-01-01') - ymd('2035-01-01')
decomm_dates %<>%
  filter(scenario=='Eskom Shutdown Plan 2023') %>% group_by(plant) %>%
  mutate(decommissioning_date=decommissioning_date +
           case_when(year(max(decommissioning_date))>=2035~
                       as.numeric(days_in_10_years + pmax(0,ymd('2035-01-01')-min(decommissioning_date))),
                     T~0),
         scenario='10-year delay') %>% bind_rows(decomm_dates)



decomm_dates %<>%
  filter(scenario=='10-year delay') %>% group_by(plant) %>%
  mutate(decommissioning_date=decommissioning_date +
           case_when(plant=='Tutuka'~365*15+(seq_along(plant)-1)*365, T~0),
         scenario='10-year delay and Tutuka delay') %>% bind_rows(decomm_dates)



#plot by month
seq.Date(ymd('2021-01-01'), ymd('2050-12-31'), by='month') %>%
  lapply(function(dt) {
    decomm_dates %>% filter(decommissioning_date>dt) %>%
      group_by(scenario) %>% summarise(across(MW, sum)) %>% mutate(date=dt)
  }) %>% bind_rows -> cap_by_month

#calculate percentage of operation by year
get_current_MW <- function(decommissioning_date, MW, year) {
  year_start <- ymd(paste(year,1,1))
  days_in_year <- as.numeric(ymd(paste(year+1,1,1))-year_start)
  current_MW <- MW * as.numeric(decommissioning_date - year_start) / days_in_year

  case_when(year(decommissioning_date)>year~MW,
            year(decommissioning_date)<year~0,
            T~current_MW)
}


#capacity plot
scens_to_plot = decomm_dates$scenario %>% unique %>% '['(c(3,2,1))

scenario_colors = function(guide=T) {
  guide_color='none'
  guide_fill='none'
  col.index=c(1,5,6)

  if(guide) {
    guide_color=guide_legend(nrow=1, override.aes = list(label='', linewidth=1))
    guide_fill=guide_legend(nrow=1)
  }

  list(scale_color_crea_d('change', col.index = col.index, guide=guide_color, limits=scens_to_plot),
       scale_fill_crea_d('change', col.index = col.index,
                         guide=guide_fill, limits=scens_to_plot),
       theme(legend.position = 'top'))
}


cap_by_month %>%
  ggplot(aes(date, MW, col=scenario)) + geom_line(linewidth=1) +
  theme_crea() + scenario_colors() +
  labs(title='Eskom operating coal power capacity by retirement scenario', y='MW', x='') +
  x_at_zero(labels=scales::comma) -> p
quicksave(file.path(output_dir, 'operating coal power capacity.png'), plot=p, scale=1.15, footer_height=.03)



(2020:2050) %>% lapply(function(yr) {
  decomm_dates %>% mutate(MW_current=get_current_MW(decommissioning_date, MW, yr)) %>%
    group_by(plant, scenario) %>% summarise(across(c(MW, MW_current), sum)) %>%
    mutate(year=yr)
}) %>% bind_rows -> cap_by_year

emis_byyear_no_decomm %>% ungroup %>%
  filter(scenario=='Eskom plan', year>=2020) %>%
  select(-starts_with('decomm'), -scenario) %>%
  left_join(cap_by_year) %>%
  mutate(emissions = emissions_wo_decomm * MW_current / MW) %>%
  group_by(plant, scenario, emitted_species) %>%
  filter(year<=max(year[emissions>0])+1) -> emis_byyear

emissions_data %>%
  pivot_longer(c(Hg, NOx, PM, SO2), names_to='emitted_species', values_to='modeled_emissions') %>%
  group_by(plant, emitted_species) %>% summarise(across(modeled_emissions, sum)) %>%
  left_join(emis_byyear, .) ->
  emis_byyear


hia_fut %>% right_join(emis_byyear) -> hia_scen

hia_scen %<>% mutate(across(c(number, cost_mn_currentUSD), multiply_by, emissions/modeled_emissions)) %>%
  group_by(region_id, plant, year, scenario, Outcome, Cause, Pollutant, double_counted, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum))

#output
hia_scen %>%
  filter(Outcome=='Deaths', !double_counted, estimate=='central') %>%
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
  geom_line(linewidth=2) +
  geom_label(aes(label=scenario), data=label_pos %>% filter(name=='line_end'), hjust=0, size=5) +
  geom_line(data=label_pos, size=1) +
  theme_crea(legend.position='top') +
  scale_color_crea_d('dramatic', col.index = c(1:4)) +
  labs(title='Deaths attributed to Eskom emissions',
       y='cases per year', x='') +
  snug_x + x_at_zero() -> p
quicksave(file.path(output_dir, 'deaths by scenario.png'), plot=p)


hia_scen %>%
  filter(Outcome=='Deaths', !double_counted, year %in% 2030:2050) %>%
  group_by(scenario, estimate) %>%
  summarise(across(c(number), sum)) %>% #cost_mn_currentUSD
  spread(estimate, number) ->
  hia_cumu

base_scenario='Eskom Shutdown Plan 2023'

hia_cumu %>% ungroup %>%
  mutate(across(c(central, low, high), ~.x-.x[scenario==base_scenario])) %>%
  filter(scenario != base_scenario) %>%
  ggplot(aes(scenario, central, fill=scenario)) + geom_col() + theme_crea() +
  scenario_colors(guide=F) +
  geom_errorbar(aes(ymin=low, ymax=high), width=.1, linewidth=.75) +
  x_at_zero(labels=scales::comma) +
  labs(title='Projected excess deaths due to delayed decommissioning',
       subtitle='Cumulative impacts 2023-2050, compared to IRP 2019 schedule',
       y='deaths attributed to air pollutant emissions from Eskom coal plants', x='') -> p
quicksave(file.path(output_dir, 'excess deaths by scenario.png'), plot=p, scale=1.15, footer_height=.03)



hia_scen %>%
  filter(Outcome=='Deaths', !double_counted) %>%
  mutate(across(c(number, cost_mn_currentUSD), ~ifelse(year %in% 2030:2050, .x, 0))) %>%
  group_by(scenario, plant, estimate) %>%
  summarise(across(c(number), sum)) %>% #cost_mn_currentUSD
  spread(estimate, number) ->
  hia_cumu_byplant

hia_cumu_byplant %>% group_by(plant) %>%
  mutate(across(c(central, low, high), ~.x-.x[scenario==base_scenario])) %>%
  filter(scenario==scens_to_plot[3], central>0) %>%
  ungroup %>% arrange(central) %>%
  mutate(plant=factor(plant, levels=plant)) %>%
  ggplot(aes(plant, central)) + geom_col(fill=crea_palettes$dramatic[1]) + theme_crea() +
  geom_errorbar(aes(ymin=low, ymax=high), width=.1, linewidth=.75) +
  x_at_zero(labels=scales::comma) +
  labs(title='Projected excess deaths due to delayed decommissioning',
       subtitle='Cumulative impacts 2030–2050, compared to Eskom 2023 schedule, by plant',
       y='deaths attributed to air pollutant emissions from Eskom coal plants', x='') -> p
quicksave(file.path(output_dir, 'deaths by plant.png'), plot=p, scale=1.15, footer_height=.03)


usd_to_lcu = 18.8

#
hia_scen %>% ungroup %>%
  filter(year %in% 2030:2050, scenario %in% scens_to_plot) %>%
  output_tables('cumulative',
                good_scenario=base_scenario,
                bad_scenario = scens_to_plot[3])


hia_scen %>% ungroup %>%
  filter(year %in% 2030:2050, scenario %in% scens_to_plot) %>%
  make_nice_data() ->
  hia_out

hia_out %>%
  group_by(Outcome, Cause, Pollutant, double_counted, estimate) %>%
  mutate(number=number - number[scenario==base_scenario]) %>%
  filter(scenario != base_scenario) %>%
  spread(estimate, number) %>%
  make_nice_numbers() %>%
  mutate(number=paste0(central, ' (', low, ' – ', high, ')')) %>%
  select(-c(low, central, high)) %>%
  spread(scenario, number) %>%
  arrange(!grepl('deaths', Outcome), !grepl('asthma', Outcome), !grepl('births', Outcome), grepl('economic', Outcome),
          Outcome, Pollutant != 'all', Pollutant!='PM2.5', double_counted, Cause, Pollutant) %>%
  select(Outcome, Cause, Pollutant, any_of(scens_to_plot), double_counted) %>% filter(!is.na(Outcome)) ->
  hia_avoided

hia_avoided %>% write_excel_csv(file.path(output_dir, 'avoided health impacts, all, cumulative.csv'))


