
infile=file.path(emissions_dir, 'Eskom emission limits 2022.xlsx')

read_wide_xlsx(path=infile, sheet='Eskom plan', header_row_names=c('variable', 'emitted_species'),
               info_columns = c(1,32:34)) ->
  emission_reductions

emission_reductions %<>%
  mutate(period_start=force_numeric(variable),
         variable = variable %>% gsub('( in )?[0-9]*', '', .)) %>%
  mutate(variable = case_when(variable=='Emission limits'~'limit_mes',
                              grepl('Eskom', variable)~'limit_eskom',
                              grepl('Compliance', variable)~'compliance',
                              grepl('planned', variable)~'retrofit_planned',
                              grepl('completed', variable)~'retrofit_year')) %>%
  spread(variable, value) %>%
  mutate(across(matches('Decomm|period_start|limit|year'), as.numeric))

emission_reductions %<>%
  filter(is.na(period_start)) %>% select(plant, emitted_species, starts_with('retro')) %>%
  left_join(emission_reductions %>% select(-starts_with('retro'))) %>%
  filter(!is.na(period_start))

names(emission_reductions) %<>% gsub('_SO2', '', .)
emission_reductions %<>% rename(decommissioning_start='Decommissioning start', decommissioning_end='Decommissioning end')

#SO2 reductions under Eskom plan: Medupi 1000mg monthly average >> emissions * limit_mes / value_max
#NOx under Eskom: limit 750 & compliant >> elv * ratio
#PM under Eskom: change from non-compliant to compliant

emission_reductions %<>%
  group_by(plant, emitted_species) %>%
  mutate(compliance_change = compliance=='yes' & compliance[period_start==2022]!='yes') %>%
  ungroup

#current fgc by compliance status
d %>% filter(grepl('Ave', V3)) -> fgc

rangefuns=list(mean=~mean(.x, na.rm=T), max=~max(.x, na.rm=T), min=~min(.x, na.rm=T))

fgc %>% group_by(plant, emitted_species=V2) %>%
  filter(value<6e3, value>0, value<1000 | emitted_species != 'PM') %>%
  summarise(across(value, rangefuns, na.rm=T)) ->
  fgc_stats

emission_reductions %>% filter(period_start==2022) %>%
  distinct(plant, emitted_species, compliance, limit_mes, retrofit_planned) %>%
  left_join(fgc_stats, .) %>%
  mutate(ratio=value_mean/limit_mes) ->
  ratios

ratios %>% filter(emitted_species=='PM', compliance=='yes', limit_mes==50, value_mean>20) %>%
  ungroup %>% summarise(across(value_mean, mean)) %>%
  unlist -> pm_fgc

ratios %>% filter(emitted_species=='NOx', compliance=='yes', !grepl('Medupi|Kusile', plant), ratio>.5) %>%
  ungroup %>% summarise(across(ratio, mean)) %>% unlist -> nox_ratio

emission_reductions %<>% left_join(fgc_stats) %>%
  group_by(plant, emitted_species) %>%
  fill(compliance_change, limit_eskom, limit_mes, .direction='down') %>%
  mutate(emission_reduction_eskom = case_when(plant=='Medupi' & emitted_species=='SO2' & period_start==2030~limit_eskom/value_max,
                                              compliance_change & emitted_species=='PM' ~ pm_fgc / value_mean,
                                              compliance_change & emitted_species=='NOx' ~ limit_eskom * nox_ratio / value_mean,
                                              T~1),
         emission_reduction_delayedmes = case_when(period_start < 2030 | decommissioning_end<=2030 ~ emission_reduction_eskom,
                                                   emitted_species=='SO2'~limit_mes/value_max,
                                                   emitted_species=='PM' ~ pm_fgc / value_mean,
                                                   emitted_species=='NOx' ~ limit_mes * nox_ratio / value_mean),
         emission_reduction_mes = case_when(period_start < 2025 | decommissioning_end<=2030 ~emission_reduction_eskom,
                                            emitted_species=='SO2'~limit_mes/value_max,
                                            emitted_species=='PM' ~ pm_fgc / value_mean,
                                            emitted_species=='NOx' ~ limit_mes * nox_ratio / value_mean))


silent_read <- function(...) suppressMessages(read_xlsx(...))


read_wide_xlsx <- function(path, sheet=NULL,
                           header_rows = length(header_row_names),
                           skip=0,
                           info_columns=1,
                           header_row_names=paste0('V', 1:header_rows),
                           stop_at_blank_row=T, discard_empty_columns=T,
                           wide_format=F,
                           ...) {
  header <- silent_read(path, sheet=sheet, skip=skip, n_max=header_rows, col_names = F)
  data <- silent_read(path, sheet=sheet, skip=header_rows+skip, col_names = F, ...)

  if(stop_at_blank_row) {
    first_empty_row <- data %>% apply(1, function(x) sum(!is.na(x))) %>% equals(0) %>% which %>% min
    data %<>% slice_head(n=first_empty_row-1)
  }

  if(discard_empty_columns) {
    empty_columns <- data %>% apply(2, function(x) sum(!is.na(x))) %>% equals(0) %>% which
    data %<>% select(-all_of(empty_columns))
  }

  header %>% t %>% as_tibble() %>%
    fill(everything(), .direction='down') %>%
    apply(1, function(x) x %>% na.omit %>% matrix(nrow=1) %>% as_tibble) %>%
    bind_rows() ->
    header_df

  header_df %>% select(matches('^V[0-9]+$')) %>%
    apply(1, function(x) x %>% na.omit %>% paste(collapse='_')) ->
    header_colnames

  if(!is.null(header_row_names)) names(header_df) <- header_row_names
  header_df %<>% mutate(col=names(header))

  if(wide_format) names(data) <- header_colnames

  if(!wide_format) {
    names(data)[info_columns] <- header_colnames[info_columns]

    data %>%
      mutate(across(-all_of(info_columns), as.character)) %>%
      pivot_longer(-all_of(info_columns), names_to='col') %>%
      right_join(header_df, .) %>% select(-col)
  }
}

