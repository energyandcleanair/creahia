#stack concentrations
d %>% filter(grepl('Ave', V3)) -> fgc

rangefuns=list(mean=~mean(.x, na.rm=T), max=~max(.x, na.rm=T), min=~min(.x, na.rm=T))

fgc %>% group_by(plant, V2) %>%
  filter(value<6e3, value>0) %>%
  summarise(across(value, rangefuns, na.rm=T)) ->
  fgc_stats

fgc_stats %>%
  ggplot(aes(plant, value_mean)) + geom_col() +
  geom_errorbar(aes(ymax=value_max, ymin=value_min)) +
  facet_wrap(~V2, scales='free_x') +
  coord_flip()

fgc %>%
  group_by(plant, V2, Month) %>%
  summarise(across(value, rangefuns, na.rm=T)) %>%
  filter(is.finite(value_mean+value_min+value_max)) %>%
  ggplot(aes(Month, value_mean)) + geom_line() +
  geom_ribbon(aes(ymin=value_min, ymax=value_max), alpha=.3) +
  facet_grid(V2~plant, scales='free')

#monthly emissions volumes
d %>% filter(V1=='Total emissions') %>%
  group_by(plant, V2, Month) %>%
  summarise(across(value, mean)) %>%
  ggplot(aes(Month, value, fill=V2)) + geom_col() +
  facet_grid(V2~plant, scales='free') +
  theme_crea(axis.text.x=element_text(angle=90)) +
  x_at_zero() +
  labs(title='Monthly emissions by plant', y='tonnes') +
  scale_fill_crea_d('dramatic', guide='none')

#coal consumption
d %>% filter(V2=='Coal') %>%
  group_by(plant, V2, Month) %>%
  summarise(across(value, mean)) %>%
  ggplot(aes(Month, value)) + geom_line() + facet_grid(V2~plant, scales='free')

#exceedances
d %>% filter(V3=='Compliance days', V4!='Normal') -> exceeds

exceeds %>%
  ggplot(aes(Month, value, fill=V4)) + geom_col() +
  facet_grid(V2~plant, scales='free_y') +
  theme_crea(axis.text.x=element_text(angle=90)) +
  x_at_zero() +
  scale_fill_crea_d('change', col.index=7:5) +
  labs(title='Emission limit exceedances by month',
       y='days', fill='type of exceedance')


exceeds %>% group_by(plant, V2, Month, V1) %>%
  summarise(across(value, sum, na.rm=T)) %>% #sum across exceedance types
  summarise(across(value, mean, na.rm=T)) %>% #average across stacks
  summarise(across(value, mean, na.rm=T)) -> #average across months
  exceed_stats

elvs %>% filter(grepl('by 1Apr 2020', period)) %>%
  group_by(plant, emitted_species) %>%
  summarise(across(c(elv=value), max)) ->
  current_elvs

exceed_stats %>% rename(average_exceeds=value) %>%
  full_join(fgc_stats) %>%
  rename(emitted_species=V2) %>%
  left_join(current_elvs) %>%
  pivot_longer(starts_with('value')) %>%
  group_by(has_exceeds = average_exceeds>0.1, emitted_species, name) %>%
  filter(has_exceeds, name=='value_mean')
mutate(ratio=value/elv, name=gsub('value_', '', name)) %>%
  pivot_longer(c(value, ratio), names_to='name2') %>%
  unite(name, name2, name) %>%
  pivot_wider() %>%
  summarise(across(matches('value_|ratio_'), mean),
            plants = plant %>% unique %>% paste(collapse = ', '))


pick_nonna <- function(x) {
  x %>% unique %>% na.omit -> x.out
  if(length(x.out)==0) x.out=NA
  return(x.out)
}

#g/kWh
d %>% lapply('[[', 'operating_data') %>%
  bind_rows() %>%
  group_by(plant, Month) %>%
  filter(col != '...9') %>%
  mutate(MWh=unique(value[grepl('MWh', V1) & V2=='Actual']),
         LF=value[grepl('Load Factor', V1)] %>% pick_nonna,
         coal_t=value[V2=='Coal'] %>% pick_nonna,
         MWh=case_when(LF>100~MWh/1000,
                       LF>1~MWh/10,
                       T~MWh)) %>%
  group_by(plant, V2, Month) %>%
  filter(V1=='Total emissions' | V2=='Coal', !is.na(value+MWh)) %>%
  mutate(V2=recode(V2, 'Coal'='Coal consumption')) %>%
  group_by(plant, V2) %>%
  summarise(across(c(value, MWh), sum)) %>%
  mutate(g_kWh=value/MWh*1000) %>%
  ggplot(aes(plant, g_kWh, fill=V2)) + geom_col() +
  facet_wrap(~V2, scales='free', nrow = 1) +
  theme_crea() +
  labs(title='Emissions intensity by plant', y='g/kWh') +
  scale_fill_crea_d('dramatic', guide='none') +
  coord_flip() +
  scale_y_continuous(expand=expansion(c(0,.05)))
