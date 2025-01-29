require(tidyverse)
require(magrittr)
require(creahia)

data_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP/results/'
file.path(data_dir, "hia_scenarios.RDS") %>% readRDS -> t1

t1 %>% names
t1 %>% filter(year==2022, scenario=='BAU') %>%
  mutate(number=ifelse(Outcome=='Asthma.Prev', 0, number),
         Outcome=ifelse(Outcome=='Asthma.Prev', 'Asthma.Inci', Outcome)) %>%
  replace_na(list(unit='case')) %>%
  group_by(CFPP.name, Owner, province, Outcome, Cause, double_counted, Pollutant, unit, year, scenario, estimate) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) ->
  t2

t2 %>%
  group_by(CFPP.name, Owner, province, year, scenario, estimate) %>%
  add_total_deaths_and_costs() %>%
  add_long_names() %>%
  select(-Outcome, -Cause) %>%
  rename(Outcome=Outcome_long, Cause=Cause_long) %>%
  pivot_wider(values_from=c(number, cost_mn_currentUSD), names_from=estimate) ->
  t3


t3 %>% filter(CFPP.name=='Cirebon Unit 1') %>% write_csv(file.path(data_dir, 'cirebon1_hia_2022.csv'))
t3 %>% write_csv(file.path(data_dir, 'all_plants_results_2022_v2.csv'))



