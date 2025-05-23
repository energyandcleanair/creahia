source('../creapuff/project_workflows/emissions_processing_SA.R')
source('project_workflows/Eskom_AEL_data_processing.R')
source('project_workflows/Eskom_plan_processing_v2.R')


#add scenario of no emission control improvements
emission_reductions$emission_reduction_nogain <- 1

#pivot
emission_reductions %<>%
  select(-starts_with('limit')) %>%
  pivot_longer(starts_with('emission_reduction'), values_to='emission_reduction',
               names_to='scenario') %>%
  mutate(scenario=case_when(grepl('delay', scenario)~'delayed compliance',
                            grepl('mes', scenario)~'compliance',
                            grepl('eskom', scenario)~'Eskom plan',
                            grepl('nogain', scenario)~'no improvements'),
         emission_reduction=pmin(emission_reduction, 1))

#fill missing limits from earlier period
emission_reductions %<>% group_by(plant, emitted_species, scenario) %>%
  fill(emission_reduction, compliance_change) %>% ungroup

#adjust period start years to match retrofit years
emission_reductions %<>%
  group_by(plant, scenario, period_start) %>%
  mutate(FGD = emission_reduction[emitted_species=='SO2']<1) %>%
  group_by(plant, scenario, emitted_species) %>%
  mutate(period_start=case_when(period_start==2030 & !is.na(retrofit_year) & scenario != 'no improvements'~retrofit_year+1,
                                T~period_start))

#add Hg
emission_reductions %<>% filter(emitted_species=='SO2') %>%
  mutate(emitted_species='Hg', elv=NA) %>%
  bind_rows(emission_reductions)

#add extra period for plants with retrofits before 2030
emission_reductions %>% filter(scenario=='Eskom plan', period_start>2025, period_start<2029) %>%
  mutate(period_start=2030) ->
  extra_rows

#add modeled emissions
emissions_data %>%
  group_by(plant) %>%
  summarise(across(Hg:SO2, sum)) %>%
  pivot_longer(Hg:SO2,
               values_to = 'emissions_t',
               names_to = 'emitted_species') ->
  emis_long

emission_reductions %<>% right_join(emis_long %>% rename(modeled_emissions=emissions_t))

d %>%
  filter(V2=='Coal', Month>='2021-04-01', Month<='2022-03-31') %>%
  group_by(plant) %>% summarise(Mt.coal.FY22=mean(value, na.rm=T)*12/1e6) ->
  coaluse_FY22

#add mercury control efficiency
hg_control %>% mutate(emitted_species='Hg') %>%
  rename(hg_modeled_elv=Current.Hg.FGC) %>%
  left_join(emission_reductions, .) %>%
  left_join(coaluse_FY22) %>%
  group_by(plant, scenario, period_start) %>%
  mutate(coal_use_adjustment=Mt.coal.FY22/Mt.coal,
         value_mean = ifelse(emitted_species=='Hg', hg_modeled_elv, value_mean),
         emission_reduction = ifelse(emitted_species=='Hg' & FGD, 1-Hg.reduction.from.SO2.abatement, emission_reduction)) %>%
  select(-hg_modeled_elv, starts_with('Mt.coal')) ->
  emission_reductions

#add BAT scenario
fgv_conversion = (21-6)/(21-10)

emission_reductions %<>% filter(scenario=='delayed compliance') %>%
  bind_rows(extra_rows) %>%
  mutate(emission_reduction=case_when(period_start<2030~emission_reduction,
                                      emitted_species=='SO2'~130/value_mean/fgv_conversion,
                                      emitted_species=='NOx'~150/value_mean/fgv_conversion,
                                      emitted_species=='PM'~8/value_mean/fgv_conversion,
                                      emitted_species=='Hg'~4/value_mean/fgv_conversion)) %>%
  mutate(scenario='BAT') %>%
  bind_rows(emission_reductions)


#calculate emissions
emission_reductions %<>% mutate(emissions=modeled_emissions * emission_reduction *
                                  case_when(emitted_species=='Hg'~coal_use_adjustment, T~1))

#calculate emissions by year
emission_reductions %>% ungroup %>%
  rename(year=period_start) %>%
  select(plant, scenario, emitted_species, year, emissions_wo_decomm=emissions) %>%
  complete(plant, scenario, emitted_species, year=2017:2080) ->
  emis_byyear_no_decomm

emission_reductions %>% ungroup %>% select(plant, scenario, starts_with('decomm')) %>%
  distinct() %>%
  left_join(emis_byyear_no_decomm, .) ->
  emis_byyear_no_decomm

emis_byyear_no_decomm %<>% group_by(plant, scenario, emitted_species) %>%
  mutate(emissions_wo_decomm = na.approx(emissions_wo_decomm, year, year, na.rm=F, rule=2))


get_emis_w_decomm <- function(emis_byyear_no_decomm) {
  emis_byyear_no_decomm %>%
    group_by(plant, scenario, emitted_species) %>%
    mutate(decommissioning_progress = case_when(year<=decommissioning_start~0,
                                                year>decommissioning_end~1)) %>%
    mutate(decommissioning_progress = approx(year, decommissioning_progress, year, rule=2)$y,
           emissions = approx(year, emissions_wo_decomm, year, rule=1:2)$y * (1-decommissioning_progress)) %>%
    filter(year<=max(year[emissions>0])+1)
}

emis_byyear_no_decomm %>% get_emis_w_decomm -> emis_byyear
