
scenario_colors = c(crea_palettes$change[c(1,2,5,7)], 'black')
names(scenario_colors) <- emis_byyear$scenario %>% unique %>% sort

emis_byyear %>% ungroup %>%
  mutate(emitted_species = recode(emitted_species, 'Hg'='Hg (tonnes)')) %>%
  group_by(year, emitted_species, scenario) %>%
  summarise(across(emissions, sum)) -> emission_totals

emission_totals %>% group_by(emitted_species, scenario) %>%
  summarise(max_emissions=max(emissions),
            line_start=emissions[year==2032]) %>%
  arrange(line_start) %>%
  mutate(line_end = max(max_emissions) * (1.5+seq_along(scenario))/7.5) %>%
  pivot_longer(starts_with('line'), values_to='emissions') %>%
  mutate(year=ifelse(name=='line_start', 2032, 2050)) ->
  label_pos

emission_totals %>%
  ggplot(aes(year, emissions/1e3, col=scenario)) + facet_wrap(~emitted_species, scales = 'free_y') +
  geom_line(size=1) +
  theme_crea(legend.position='top') +
  scale_color_manual(values=scenario_colors,
                     guide='none') +
  geom_label(aes(label=scenario), data=label_pos %>% filter(name=='line_end'), hjust=0) +
  geom_line(data=label_pos) +
  labs(y='kt/year', x='', title='Air pollutant emissions from Eskom plants by scenario',
       caption='scenarios have the same assumptions about plant retirement but vary in air pollutant control performance') +
  x_at_zero() +
  scale_x_continuous(breaks=c(2022, seq(2030,2070,10)), expand=expansion()) -> p

quicksave(file.path(output_dir,'emissions totals over time.png'), plot=p)

emis_byyear %>% ungroup %>% filter(emitted_species=='PM') %>%
  mutate(emitted_species = recode(emitted_species, 'Hg'='Hg (tonnes)')) %>%
  group_by(plant, year, emitted_species, scenario) %>%
  summarise(across(emissions, sum)) %>%
  ggplot(aes(year, emissions/1e3, col=scenario)) + facet_wrap(~plant, scales = 'free_y') + geom_line(size=1) +
  theme_crea(legend.position='top') + scale_color_crea_d('change', guide=guide_legend(nrow=1)) +
  labs(y='kt/year', x='', title='Air pollutant emissions from Eskom plants by scenario',
       caption='scenarios have the same assumptions about plant retirement but vary in air pollutant control performance') +
  x_at_zero() +
  scale_x_continuous(breaks=c(2022, seq(2030,2070,10)), expand=expansion()) -> p

rcrea::add_logo(p)


emis_byyear %>%
  mutate(emitted_species = recode(emitted_species, 'Hg'='Hg (tonnes)')) %>%
  group_by(emitted_species, scenario) %>%
  summarise(across(emissions, sum)) %>%
  ggplot(aes(scenario, emissions/1e3, fill=scenario)) + facet_wrap(~emitted_species, scales = 'free_y') +
  geom_col() +
  theme_crea(legend.position='top',
             axis.text.x=element_text(angle=20, hjust=1)) +
  scale_fill_manual(values=scenario_colors, guide='none') +
  labs(y='kt/year', x='', title='Air pollutant emissions from Eskom plants by scenario',
       subtitle='cumulative totals 2022 to end-of-life',
       caption='scenarios have the same assumptions about plant retirement but vary in air pollutant control performance') +
  x_at_zero(label=scales::comma) -> p

rcrea::add_logo(p)
quicksave(file.path(output_dir,'emissions totals, cumulative.png'), plot=p)
