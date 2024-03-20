output_tables <- function(hiadata, output_name='', rounding_function=make_nice_numbers,
                          bad_scenario='Eskom plan', good_scenario='compliance') {

  hiadata %>% make_nice_data() -> hia_out

  if(good_scenario=='nocoal')
    hia_out %<>% filter(scenario=='Eskom plan') %>%
    mutate(number=0, scenario='nocoal') %>%
    bind_rows(hia_out)

  hia_out %>%
    group_by(Outcome, Cause, Pollutant, estimate, double_counted) %>%
    summarise(number=number[scenario==bad_scenario] - number[scenario==good_scenario]) %>%
    spread(estimate, number) %>%
    arrange(!grepl('deaths', Outcome), !grepl('asthma', Outcome), !grepl('births', Outcome), grepl('economic', Outcome),
            Outcome, Pollutant != 'all', Pollutant!='PM2.5', double_counted, Cause, Pollutant) %>%
    select(Outcome, Cause, Pollutant, central, low, high, double_counted) %>% filter(!is.na(Outcome)) ->
    hia_avoided

  hia_avoided %>%
    filter(!(Pollutant=='PM2.5' & Cause=='all causes')) %>%
    rounding_function %>%
    write_csv(file.path(output_dir,
                        paste0('avoided health impacts, ',good_scenario,' vs ',
                               bad_scenario,', ',output_name,'.csv')))

  hia_out %>% ungroup %>% filter(Pollutant == 'all') %>%
    rounding_function %>%
    spread(estimate, number) %>%
    arrange(Outcome, scenario) %>%
    select(scenario, Outcome, central, low, high) %T>%
    print() %>%
    write_csv(file.path(output_dir, paste0('deaths and total costs, all scenarios, ',output_name,'.csv')))

  hia_avoided %<>% rounding_function(sigdigs=2, accuracy=10)

  statements <-character()

  hia_avoided %>% filter(!(Outcome == 'deaths' & Cause != 'all causes')) %>%
    mutate(statement=case_when(Outcome=='deaths' & Pollutant=='all'~
                                 paste0(central, ' ', Outcome, ' (95% confidence interval: ',low, ' – ', high,')', ', of which '),
                               grepl('disabi', Outcome) & Cause=='all causes'~paste0(central, ' ', Outcome, ', of which '),
                               Outcome=='deaths' & Cause=='all causes'~paste0(central, ' due to exposure to ', Pollutant, ', '),
                               grepl('disability', Outcome)~paste0(central, ' due to ', Cause, ', '),
                               grepl('bln R', Outcome)~paste0('total economic costs of R', central, 'bln'),
                               #grepl('USD', Outcome)~paste0('total economic costs of $', central, 'mln'),
                               grepl('USD', Outcome)~paste0(' (USD ', central, ' mln)'),
                               grepl('absence', Outcome)~paste0(central, ' million days of work absence, '),
                               T~paste0(central, ' ', Outcome, ', '))) %>%
    use_series(statement) %>% paste(collapse='') %>%
    c(statements, .) -> statements

  hia_avoided %>% filter(Outcome == 'deaths' & Cause != 'all causes') %>%
    (function(df) {
      c(paste0('Of the deaths caused by PM2.5 exposure, ', df$central[1], ' are attributed to ', df$Cause[1]),
        paste0(df$central[-1], ' to ', df$Cause[-1])) %>% paste(collapse=', ')
    }) %>% c(statements, .) -> statements

  hia_out %>% filter(Pollutant=='all') %>%
    group_by(Outcome, estimate) %>%
    mutate(number=number[scenario==bad_scenario] - number) %>%
    spread(estimate, number) %>% filter(central>0) %>%
    make_nice_numbers() %>%
    arrange(!grepl('deaths', Outcome)) %>%
    group_by(scenario) %>%
    summarise(statement = paste0('the ', scenario[1], ' scenario would avoid a projected ', central[1],
                                 ' deaths from air pollution (95% confidence interval: ',
                                 low[1], ' – ', high[1], ')',
                                 ' and economic costs of R', central[2], ' billion',
                                 ' (95% confidence interval: ', low[2], ' – ', high[2], ')'),
              central=central[1]) %>%
    arrange(central) %>% use_series(statement) %>%
    paste(collapse='; ') %>% paste('Compared to the', bad_scenario, 'scenario,', .) %>%
    c(statements, .) -> statements

  statements %>%
    c(paste('Compared to the', bad_scenario, 'scenario, the', good_scenario, 'scenario would avoid...'), .) %T>%
    print() %>%
    writeLines(file.path(output_dir, paste0(good_scenario, ' vs ', bad_scenario, ', ', output_name, '.txt')))
}


make_nice_numbers <- function(df, sigdigs=3, accuracy=1, columns=c('number', 'central', 'low', 'high')) {
  df %>% mutate(across(any_of(columns),
                       function(x) {
                         x %<>% signif(sigdigs)
                         ifelse(grepl('mln|bln', Outcome) & !grepl('USD', Outcome),
                                scales::comma(x, accuracy=accuracy/100),
                                scales::comma(x, accuracy=accuracy))
                       }))
}

make_nice_data <- function(hiadata) {
  hiadata %<>% filter(!double_counted, !grepl('YLLs|LBW', Outcome)) %>%
    add_long_names() %>% select(-Outcome, -Cause) %>% rename(Outcome=Outcome_long, Cause=Cause_long)

  hiadata %>% group_by(scenario, estimate) %>%
    summarise(across(cost_mn_currentUSD, sum, na.rm=T)) %>%
    rename(number=cost_mn_currentUSD) %>%
    mutate(Outcome = 'total economic cost, mln USD',
           Pollutant='all', Cause='all', double_counted=F) -> cost_totals

  hiadata %>% filter(grepl('deaths', Outcome)) %>%
    group_by(scenario, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(Outcome = 'deaths',
           Pollutant='all', Cause='all causes', double_counted=F) -> death_totals

  hiadata %>% filter(grepl('disabi', Outcome)) %>%
    group_by(scenario, Outcome, Pollutant, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(Cause = 'all causes', double_counted=F) -> yld_totals

  hiadata %>% filter(grepl('deaths', Outcome), Pollutant=='PM2.5') %>%
    group_by(scenario, estimate, Pollutant) %>%
    summarise(across(number, sum)) %>%
    mutate(Outcome = 'deaths', Cause='all causes', double_counted=T) -> pm25_death_totals

  hiadata$double_counted[grepl('disabi', hiadata$Outcome)] <- T

  hiadata %>%
    filter(!grepl('prevalence', Outcome)) %>%
    mutate(double_counted = grepl('deaths', Outcome),
           across(Cause, tolower)) %>%
    group_by(scenario, Outcome, Cause, Pollutant, double_counted, estimate) %>%
    summarise(across(number, sum)) %>%
    bind_rows(cost_totals,
              pm25_death_totals,
              death_totals,
              yld_totals) -> hia_out

  hia_out %<>% mutate(number = number * ifelse(grepl('absence', Outcome), 1e-6, 1),
                      Cause = ifelse(Outcome==Cause, '', Cause),
                      Outcome = ifelse(grepl('absence', Outcome), 'work absence (mln sick leave days)', Outcome))

  hia_out %>% filter(grepl('economic', Outcome)) %>%
    mutate(number=number*usd_to_lcu/1000,
           Outcome='total economic cost, bln R') %>%
    bind_rows(hia_out)
}

