source('../creapuff/project_workflows/emissions_processing_SA.R')

elv_file=file.path(emissions_dir, 'Eskom emission limits.xlsx')
read_xlsx(elv_file, sheet='limits', skip=2, col_names=F) -> elvs
read_xlsx(elv_file, sheet='limits', n_max=2, col_names=F) -> elv_header

elv_header %<>% t %>% as_tibble %>%
  set_names(c('period', 'var')) %>%
  fill(period, .direction = 'down') %>%
  mutate(col=names(elv_header))

emissions_columns <- elv_header %>% filter(!is.na(period) & !grepl('Decomm', period)) %>%
  use_series(col)

elvs %<>% pivot_longer(all_of(emissions_columns), names_to = 'col')

cols_to_rename <- grep('\\.{3}', names(elvs))
names(elvs)[cols_to_rename] <- elv_header$var[match(names(elvs)[cols_to_rename], elv_header$col)]

elvs %<>% left_join(elv_header) %>%
  mutate(across(Stack, as.numeric),
         var=recode(var, PM10='PM', NOX='NOx')) %>%
  filter(!is.na(var)) %>%
  select(plant='Power station', stack=Stack,
         decommissioning_50 = contains('50-year'),
         decommissioning_60 = contains('60-year'),
         emitted_species=var, period, value)

elvs %>% select(plant, stack, starts_with('decomm')) %>% distinct() ->
  decommissioning

decommissioning$decommissioning_50[decommissioning$plant=='Medupi'] <- '2064-2072'
decommissioning %<>% bind_rows(tibble(plant='Kusile', stack=1:2, decommissioning_50='2067-2074'))

elvs %>% select(-starts_with('decomm')) %>%
  rename(elv=value) %>%
  mutate(time_period = case_when(grepl('1Apr 2020', period)~'2020-2024',
                                 grepl('1Apr 2025', period)~'2025-2029'),
         elv_type = case_when(grepl('AEL', period)~'AEL',
                              grepl('alternative', period)~'alternative')) %>%
  filter(grepl('limit', period), !is.na(time_period), !is.na(elv_type)) %>%
  select(-period) ->
  emis_fut

#assume all Eskom alternative limits are applied, and NOx improvements are not happening
emis_fut %<>% group_by(plant, emitted_species, time_period) %>%
  summarise(across(elv, max, na.rm=T)) %>%
  group_by(plant, emitted_species) %>%
  mutate(elv=ifelse(is.finite(elv), elv, NA),
         elv=case_when(emitted_species=='SO2' & plant != 'Kusile' ~ min(elv[elv>1000]),
                       emitted_species=='NOx'~ unique(elv[time_period=='2020-2024']),
                       T~elv))

#fill missing limits from earlier period
emis_fut %<>% group_by(plant, emitted_species) %>% fill(elv) %>% ungroup

emis_fut %<>% filter(time_period=='2025-2029') %>%
  mutate(time_period='2030-2080') %>%
  bind_rows(emis_fut) %>%
  mutate(scenario='non-compliance')

#add current ELV
emis_fut %<>% filter(time_period=='2020-2024') %>% select(-time_period) %>%
  rename(modeled_elv=elv) %>% full_join(emis_fut, .)

#add MES compliance scenario
emis_fut %<>%
  mutate(elv=case_when(time_period!='2030-2080'~elv,
                       emitted_species=='SO2'~pmin(elv, 1000),
                       emitted_species=='NOx'~pmin(elv, 750),
                       emitted_species=='PM'~pmin(elv, 50))) %>%
  mutate(scenario='compliance') %>%
  bind_rows(emis_fut)

#add Hg
emis_fut %<>% filter(emitted_species=='SO2') %>%
  mutate(emitted_species='Hg', elv=NA) %>%
  bind_rows(emis_fut)

#add BAT scenario
fgv_conversion = (21-6)/(21-10)

emis_fut %<>% filter(scenario=='compliance') %>%
  mutate(elv=case_when(time_period!='2030-2080'~elv,
                       emitted_species=='SO2'~pmin(elv, 130/fgv_conversion),
                       emitted_species=='NOx'~pmin(elv, 150/fgv_conversion),
                       emitted_species=='PM'~pmin(elv, 8/fgv_conversion),
                       emitted_species=='Hg'~pmin(elv, 4/fgv_conversion, na.rm=T))) %>%
  mutate(scenario='BAT') %>%
  bind_rows(emis_fut)

#add modeled emissions
emissions_data %>% mutate(across(stack, as.numeric)) %>%
  pivot_longer(c('SO2', 'NOx', 'PM', 'Hg'),
               values_to = 'emissions_t',
               names_to = 'emitted_species') ->
  emis_long

emis_fut %<>% right_join(emis_long %>% rename(modeled_emissions=emissions_t))


#add mercury control efficiency
hg_control %>% mutate(emitted_species='Hg') %>% rename(hg_modeled_elv=Current.Hg.FGC) %>%
  left_join(emis_fut, .) %>%
  mutate(modeled_elv = ifelse(emitted_species=='Hg', hg_modeled_elv, modeled_elv)) %>%
  select(-hg_modeled_elv) ->
  emis_fut

#calculate emissions
emis_fut %<>% group_by(plant, scenario, stack, time_period) %>%
  mutate(FGD = elv[emitted_species=='SO2']<=1000,
         emissions=case_when(!is.na(elv) ~ modeled_emissions*elv/modeled_elv,
                             FGD~modeled_emissions * (1-Hg.reduction.from.SO2.abatement),
                             T~modeled_emissions))

#calculate emissions by year
emis_fut %>% ungroup %>%
  mutate(year = case_when(time_period=='2020-2024'~2022,
                          time_period=='2025-2029'~2025,
                          time_period=='2030-2080'~2030)) %>%
  select(plant, stack, scenario, emitted_species, year, emissions) %>%
  complete(nesting(plant, stack), scenario, emitted_species, year=2022:2080) ->
  emis_byyear

decommissioning %>%
  select(plant, stack, decommissioning=decommissioning_50) %>%
  left_join(emis_byyear, .) %>%
  separate(decommissioning, into=c('decommissioning_start', 'decommissioning_end'), convert=T) %>%
  mutate(decommissioning_delay=case_when(decommissioning_start<2030~5,
                                         T~0),
         across(starts_with('decomm'), add, decommissioning_delay),
         decommissioning_progress = case_when(year<=2023~0,
                                              year==decommissioning_start-1~0,
                                              year==decommissioning_end~1)) ->
  emis_byyear

emis_byyear %<>%
  group_by(plant, stack, scenario, emitted_species) %>%
  mutate(decommissioning_progress = approx(year, decommissioning_progress, year, rule=2)$y,
         emissions = approx(year, emissions, year, rule=1:2)$y * (1-decommissioning_progress))

emis_byyear %>%
  group_by(year, emitted_species, scenario) %>%
  summarise(across(emissions, sum)) %>%
  ggplot(aes(year, emissions, col=scenario)) + facet_wrap(~emitted_species, scales = 'free_y') + geom_line()


emis_byyear %>%
  group_by(year, plant, emitted_species, scenario) %>%
  summarise(across(emissions, sum)) ->
  emis_byyear_byplant

emis_byyear_byplant %>% filter(year %in% c(2022, 2025), emitted_species=='SO2', scenario=='compliance') %>%
  group_by(plant) %>% summarise(change=emissions[year==2025]-emissions[year==2022]) %>%
  arrange(change)


