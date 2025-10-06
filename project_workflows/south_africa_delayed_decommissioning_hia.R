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
project_dir="G:/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
hia_dir <- file.path(project_dir,"HIA_MES")
output_dir = file.path(project_dir, 'HIA_delayed_retirement') ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

source('project_workflows/emissions_processing_SA_v3.R')


emis <- read_csv(file.path(emissions_dir, 'emissions scaling for scenarios_v2.csv'))
hia_fut <- readRDS(file.path(hia_dir, 'hia_fut.RDS'))

#Grootvlei, Camden, Hendrina, Arnot, Kriel and Matla
emission_reductions %>% ungroup %>% distinct(plant, decommissioning_start, decommissioning_end) %>%
  left_join(plantdata %>% ungroup %>% distinct(plant, MW=plant_MW)) %>%
  mutate(scenario='ERP 2022',
         subject_to_delay=decommissioning_start<=2030 & decommissioning_start>2022 & plant != 'Tutuka',
         decommissioning_duration=decommissioning_end-decommissioning_start) -> decomm

decomm %<>%
  mutate(scenario='delay all to 2030',
         decommissioning_start=case_when(subject_to_delay~2031,T~decommissioning_start),
         decommissioning_end  =case_when(subject_to_delay~2031+decommissioning_duration,T~decommissioning_end)) %>%
  bind_rows(decomm)

decomm %<>% filter(scenario=='ERP 2022') %>%
  mutate(scenario='delay all to 2030s',
         decommissioning_start=case_when(subject_to_delay~decommissioning_start+8,T~decommissioning_start),
         decommissioning_end  =case_when(subject_to_delay~decommissioning_end+8,T~decommissioning_end)) %>%
  bind_rows(decomm)

calc_delay = function(yr, flag) {
  yr_out <- yr+approx(c(2031,2051),c(8,0),yr,rule=2)$y
  yr_out[!flag] <- yr[!flag]
  return(yr_out)
}

decomm %<>% filter(scenario=='ERP 2022') %>%
  mutate(scenario='delay all to 2030s, with knock-on effects',
         subject_to_delay = plant!='Tutuka' & decommissioning_start>2022,
         decommissioning_start=calc_delay(decommissioning_start, subject_to_delay),
         decommissioning_end  =decommissioning_start + decommissioning_duration) %>%
  bind_rows(decomm)

read_xlsx(file.path(emissions_dir, 'IRP_retirement.xlsx')) -> irp_decomm
decomm %<>% filter(scenario=='ERP 2022') %>%
  select(-starts_with('decomm')) %>%
  left_join(irp_decomm) %>%
  mutate(scenario='IRP 2019') %>%
  bind_rows(decomm)

decomm %>%
  cross_join(tibble(year=2018:2075)) %>% rowwise %>%
  mutate(MW_current=approx(c(0,decommissioning_start-1,decommissioning_end+1,9999),c(MW,MW,0,0),year)$y) ->
  decomm_by_year




#capacity plot
scens_to_plot = decomm$scenario %>% unique %>% subset(!grepl('to 2030$|Komati', .)) %>% '['(c(1,4,3,2))

scenario_colors = function(guide=T) {
  guide_color='none'
  guide_fill='none'
  col.index=c(1,2,5,6)

  if(guide) {
    guide_color=guide_legend(nrow=1, override.aes = list(label='', linewidth=1))
    guide_fill=guide_legend(nrow=1)
  }

  list(scale_color_crea_d('change', col.index = col.index, guide=guide_color, limits=scens_to_plot),
       scale_fill_crea_d('change', col.index = col.index,
                         guide=guide_fill, limits=scens_to_plot),
       theme(legend.position = 'top'))
}



decomm_by_year %>%
  group_by(year, scenario) %>% summarise(across(MW_current, sum)) %>%
  filter(year>=2020, scenario %in% scens_to_plot) %>%
  write_csv(file.path(output_dir, 'operating coal power capacity.csv')) %>%
  ggplot(aes(year, MW_current, col=scenario)) + geom_line(linewidth=1) +
  theme_crea() + scenario_colors() +
  labs(title='Eskom operating coal power capacity by retirement scenario', y='MW', x='') +
  x_at_zero(labels=scales::comma) -> p
quicksave(file.path(output_dir, 'operating coal power capacity.png'), plot=p, scale=1.15, footer_height=.03)


emis_byyear_no_decomm %>% ungroup %>%
  filter(scenario=='Eskom plan', year>=2020) %>%
  select(-starts_with('decomm'), -scenario) %>%
  left_join(decomm %>% select(plant, scenario, decommissioning_start, decommissioning_end)) %>%
  get_emis_w_decomm() -> emis_byyear


#avoided impacts from Komati closure
read_xlsx(file.path(emissions_dir, 'Komati', 'emissions compiled.xlsx')) %>% pivot_longer(-1) %>%
  set_names(c('emitted_species', 'FY', 'emissions')) %>%
  filter(emitted_species != 'GWh') -> emis_komati

tibble(month=seq.Date(ymd('2015-01-01'), ymd('2023-12-01'), by='month')) %>%
  mutate(FY=year(month-90) %>% paste0(., '-', .+1-2000), year=year(month)) %>%
  full_join(emis_komati) %>% complete(emitted_species, nesting(month, year, FY)) %>%
  mutate(across(emissions, ~ifelse(is.na(.x)&year==2023, 0, .x/12))) %>%
  group_by(emitted_species, year) %>%
  fill(emissions, .direction='downup') %>%
  summarise(across(emissions, ~sum(.x))) %>%
  filter(!is.na(emitted_species)) ->
  emis_komati_year

emis_komati %>% filter(FY %in% sort(unique(FY))[1:2]) %>% group_by(emitted_species) %>%
  summarise(across(emissions, mean)) %>% cross_join(tibble(year=2020:2023)) ->
  emis_komati_baseline

emis_komati_year %>%
  ggplot(aes(year, emissions)) + geom_col(fill=crea_palettes$dramatic[1]) + facet_wrap(~emitted_species, scales='free_y') +
  geom_line(data=emis_komati_baseline, aes(linetype='baseline emissions at full operation (FY 2015–17 average)'), col=crea_palettes$dramatic[2], linewidth=1) +
  scale_linetype_manual(values='dashed', name='') +
  theme_crea(legend.position='top') +
  labs(title='Komati annual emissions', y='t/year', x='calendar year') +
  x_at_zero(labels=scales::comma) -> p
quicksave(file.path(output_dir, 'Komati emissions.png'), plot=p, scale=1.15, footer_height=.03)

emis_komati_year %>% filter(year %in% 2020:2023) %>%
  mutate(scenario='Komati actual emissions') %>%
  bind_rows(emis_komati_baseline %>% mutate(scenario='Komati full operation')) ->
  emis_komati_scen

emis_komati_scen %>% mutate(plant='Komati') %>%
  bind_rows(emis_byyear) -> emis_byyear


emis_byyear %>% group_by(year, scenario, emitted_species) %>%
  summarise(across(emissions, sum)) %>% ggplot(aes(year, emissions, col=scenario)) + geom_line() + facet_wrap(~emitted_species, scales='free_y')

emissions_data %>%
  pivot_longer(c(Hg, NOx, PM, SO2), names_to='emitted_species', values_to='modeled_emissions') %>%
  group_by(plant, emitted_species) %>% summarise(across(modeled_emissions, sum)) %>%
  left_join(emis_byyear, .) ->
  emis_byyear


hia_fut %>% right_join(emis_byyear) -> hia_scen

hia_scen %<>% mutate(across(c(number, cost_mn_currentUSD), multiply_by, emissions/modeled_emissions)) %>%
  group_by(region_id, plant, year, scenario, outcome, cause, pollutant, double_counted, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum))

#output
hia_scen %>%
  filter(outcome=='Deaths', !double_counted, estimate=='central', scenario %in% scens_to_plot) %>%
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
  scenario_colors(guide=F) +
  labs(title='Deaths attributed to Eskom emissions',
       y='cases per year', x='') +
  snug_x + x_at_zero() -> p
quicksave(file.path(output_dir, 'deaths by scenario.png'), plot=p)


hia_scen %>%
  filter(outcome=='Deaths', !double_counted, year %in% 2023:2050, scenario %in% scens_to_plot) %>%
  group_by(scenario, estimate) %>%
  summarise(across(c(number), sum)) %>% #cost_mn_currentUSD
  spread(estimate, number) ->
  hia_cumu

hia_cumu %>% ungroup %>%
  mutate(across(c(central, low, high), ~.x-.x[scenario=='IRP 2019'])) %>%
  filter(scenario != 'IRP 2019') %>%
  ggplot(aes(scenario, central, fill=scenario)) + geom_col() + theme_crea() + scenario_colors(guide=F) +
  geom_errorbar(aes(ymin=low, ymax=high), width=.1, linewidth=.75) +
  x_at_zero(labels=scales::comma) +
  labs(title='Projected excess deaths due to delayed decommissioning',
       subtitle='Cumulative impacts 2023-2050, compared to IRP 2019 schedule',
       y='deaths attributed to air pollutant emissions from Eskom coal plants', x='') -> p
quicksave(file.path(output_dir, 'excess deaths by scenario.png'), plot=p, scale=1.15, footer_height=.03)



hia_scen %>%
  filter(outcome=='Deaths', !double_counted) %>%
  mutate(across(c(number, cost_mn_currentUSD), ~ifelse(year %in% 2023:2050, .x, 0))) %>%
  group_by(scenario, plant, estimate) %>%
  summarise(across(c(number), sum)) %>% #cost_mn_currentUSD
  spread(estimate, number) ->
  hia_cumu_byplant

hia_cumu_byplant %>% group_by(plant) %>%
  mutate(across(c(central, low, high), ~.x-.x[scenario=='ERP 2022'])) %>%
  filter(scenario==scens_to_plot[3], central>0) %>%
  ungroup %>% arrange(central) %>%
  mutate(plant=factor(plant, levels=plant)) %>%
  ggplot(aes(plant, central)) + geom_col(fill=crea_palettes$dramatic[1]) + theme_crea() +
  geom_errorbar(aes(ymin=low, ymax=high), width=.1, linewidth=.75) +
  x_at_zero(labels=scales::comma) +
  labs(title='Projected excess deaths due to delayed decommissioning',
       subtitle='Cumulative impacts 2023–2050, compared to ERP 2022 schedule, by plant',
       y='deaths attributed to air pollutant emissions from Eskom coal plants', x='') -> p
quicksave(file.path(output_dir, 'deaths by plant.png'), plot=p, scale=1.15, footer_height=.03)


usd_to_lcu = 18.85

#
hia_scen %>% ungroup %>%
  filter(year %in% 2023:2050, scenario %in% scens_to_plot) %>%
  output_tables('cumulative', good_scenario='IRP 2019', bad_scenario = 'delay all by 8 years')

hia_scen %>% ungroup %>%
  filter(year %in% 2023:2050, scenario %in% scens_to_plot) %>%
  output_tables('cumulative', good_scenario='IRP 2019', bad_scenario = 'ERP 2022')

hia_scen %>% ungroup %>%
  filter(year %in% 2023:2050, scenario %in% scens_to_plot) %>%
  output_tables('cumulative', good_scenario='IRP 2019', bad_scenario = "delay all to 2030s, with knock-on effects")


hia_scen$scenario %>% grep('Komati', ., value=T) %>% unique

hia_scen %>% ungroup %>%
  filter(year %in% 2020:2023, grepl('Komati', scenario)) %>%
  output_tables('cumulative', good_scenario="Komati actual emissions", bad_scenario = "Komati full operation")


hia_scen %>% ungroup %>%
  filter(year %in% 2023:2050, scenario %in% scens_to_plot) %>%
  make_nice_data() ->
  hia_out

good_scenario = 'IRP 2019'

hia_out %>%
  group_by(outcome, cause, pollutant, double_counted, estimate) %>%
  mutate(number=number - number[scenario==good_scenario]) %>%
  filter(scenario != good_scenario) %>%
  spread(estimate, number) %>%
  make_nice_numbers() %>%
  mutate(number=paste0(central, ' (', low, ' – ', high, ')')) %>%
  select(-c(low, central, high)) %>%
  spread(scenario, number) %>%
  arrange(!grepl('deaths', outcome), !grepl('asthma', outcome), !grepl('births', outcome), grepl('economic', outcome),
          outcome, pollutant != 'all', pollutant!='PM2.5', double_counted, cause, pollutant) %>%
  select(outcome, cause, pollutant, any_of(scens_to_plot), double_counted) %>% filter(!is.na(outcome)) ->
  hia_avoided

hia_avoided %>% write_excel_csv(file.path(output_dir, 'avoided health impacts, all, cumulative.csv'))

decomm %>% mutate(plant=as.factor(plant)) %>%
  ggplot(aes(fill=scenario)) +
  geom_rect(aes(xmin=as.numeric(plant)-.4, xmax=as.numeric(plant)+.4,
                ymin=decommissioning_start, ymax=decommissioning_end), alpha=.3) +
  scale_x_continuous(labels=plant)



decomm %>% mutate(plant=as.factor(plant)) %>%
  ggplot(aes(fill=scenario)) +
  geom_rect(aes(xmin=as.numeric(plant)-.4, xmax=as.numeric(plant)+.4,
                ymin=decommissioning_start, ymax=decommissioning_end), alpha=.3) +
  scale_x_continuous()

decomm %>%
  group_by(plant) %>%
  filter(scenario %in% scens_to_plot, any(decommissioning_start<2040), !grepl('Tutuka|Komati', plant)) %>%
  mutate(scenario=factor(scenario, levels=rev(scens_to_plot))) %>%
  pivot_longer(matches('start|end')) %>%
  ggplot(aes(scenario, value-.5, col=scenario)) +
  facet_wrap(~plant, ncol = 1, strip.position = 'left') +
  geom_line(linewidth=4, alpha=.5) +
  coord_flip() +
  theme_crea() +
  theme(panel.grid.minor = element_line(linewidth = .1),
        panel.grid.major.y = element_blank(),
        strip.text.y.left = element_text(angle=0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(color='gray')) +
  scale_y_continuous(minor_breaks = function(x) round(seq(x[1],x[2]),0)-.5,
                     breaks = seq(2020,2045,5)) +
  scale_color_crea_d('change', col.index = c(6,5,2,1), guide=guide_legend(reverse=T)) +
  labs(title='Eskom coal plant decommissioning years by scenario',
       x='', y='') -> p
quicksave(file.path(output_dir, 'decommissioning years by scenario.png'), plot=p, scale=1.15, footer_height=.03)

