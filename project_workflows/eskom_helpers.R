output_tables <- function(hiadata, output_name='', rounding_function=make_nice_numbers,
                          bad_scenario='Eskom plan', good_scenario='compliance') {

  hiadata %>% make_nice_data() -> hia_out

  if(good_scenario=='nocoal')
    hia_out %<>% filter(scenario=='Eskom plan') %>%
    mutate(number=0, scenario='nocoal') %>%
    bind_rows(hia_out)

  hia_out %>%
    group_by(outcome, cause, pollutant, estimate, double_counted) %>%
    summarise(number=number[scenario==bad_scenario] - number[scenario==good_scenario]) %>%
    spread(estimate, number) %>%
    arrange(!grepl('deaths', outcome), !grepl('asthma', outcome), !grepl('births', outcome), grepl('economic', outcome),
            outcome, pollutant != 'all', pollutant!='PM2.5', double_counted, cause, pollutant) %>%
    select(outcome, cause, pollutant, central, low, high, double_counted) %>% filter(!is.na(outcome)) ->
    hia_avoided

  hia_avoided %>%
    filter(!(pollutant=='PM2.5' & cause=='all causes')) %>%
    rounding_function %>%
    write_csv(file.path(output_dir,
                        paste0('avoided health impacts, ',good_scenario,' vs ',
                               bad_scenario,', ',output_name,'.csv')))

  hia_out %>% ungroup %>% filter(pollutant == 'all') %>%
    rounding_function %>%
    spread(estimate, number) %>%
    arrange(outcome, scenario) %>%
    select(scenario, outcome, central, low, high) %T>%
    print() %>%
    write_csv(file.path(output_dir, paste0('deaths and total costs, all scenarios, ',output_name,'.csv')))

  hia_avoided %<>% rounding_function(sigdigs=2, accuracy=10)

  statements <-character()

  hia_avoided %>% filter(!(outcome == 'deaths' & cause != 'all causes')) %>%
    mutate(statement=case_when(outcome=='deaths' & pollutant=='all'~
                                 paste0(central, ' ', outcome, ' (95% confidence interval: ',low, ' – ', high,')', ', of which '),
                               grepl('disabi', outcome) & cause=='all causes'~paste0(central, ' ', outcome, ', of which '),
                               outcome=='deaths' & cause=='all causes'~paste0(central, ' due to exposure to ', pollutant, ', '),
                               grepl('disability', outcome)~paste0(central, ' due to ', cause, ', '),
                               grepl('bln R', outcome)~paste0('total economic costs of R', central, 'bln'),
                               #grepl('USD', outcome)~paste0('total economic costs of $', central, 'mln'),
                               grepl('USD', outcome)~paste0(' (USD ', central, ' mln)'),
                               grepl('absence', outcome)~paste0(central, ' million days of work absence, '),
                               T~paste0(central, ' ', outcome, ', '))) %>%
    use_series(statement) %>% paste(collapse='') %>%
    c(statements, .) -> statements

  hia_avoided %>% filter(outcome == 'deaths' & cause != 'all causes') %>%
    (function(df) {
      c(paste0('Of the deaths caused by PM2.5 exposure, ', df$central[1], ' are attributed to ', df$cause[1]),
        paste0(df$central[-1], ' to ', df$cause[-1])) %>% paste(collapse=', ')
    }) %>% c(statements, .) -> statements

  hia_out %>% filter(pollutant=='all') %>%
    group_by(outcome, estimate) %>%
    mutate(number=number[scenario==bad_scenario] - number) %>%
    spread(estimate, number) %>% filter(central>0) %>%
    make_nice_numbers() %>%
    arrange(!grepl('deaths', outcome)) %>%
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
                         ifelse(grepl('mln|bln', outcome) & !grepl('USD', outcome),
                                scales::comma(x, accuracy=accuracy/100),
                                scales::comma(x, accuracy=accuracy))
                       }))
}

make_nice_data <- function(hiadata) {
  hiadata %<>% filter(!double_counted, !grepl('YLLs|LBW', outcome)) %>%
    add_long_names() %>% select(-outcome, -cause) %>% rename(outcome=outcome_long, cause=cause_long)

  hiadata %>% group_by(scenario, estimate) %>%
    summarise(across(cost_mn_currentUSD, sum, na.rm=T)) %>%
    rename(number=cost_mn_currentUSD) %>%
    mutate(outcome = 'total economic cost, mln USD',
           pollutant='all', cause='all', double_counted=F) -> cost_totals

  hiadata %>% filter(grepl('deaths', outcome)) %>%
    group_by(scenario, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(outcome = 'deaths',
           pollutant='all', cause='all causes', double_counted=F) -> death_totals

  hiadata %>% filter(grepl('disabi', outcome)) %>%
    group_by(scenario, outcome, pollutant, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(cause = 'all causes', double_counted=F) -> yld_totals

  hiadata %>% filter(grepl('deaths', outcome), pollutant=='PM2.5') %>%
    group_by(scenario, estimate, pollutant) %>%
    summarise(across(number, sum)) %>%
    mutate(outcome = 'deaths', cause='all causes', double_counted=T) -> pm25_death_totals

  hiadata$double_counted[grepl('disabi', hiadata$outcome)] <- T

  hiadata %>%
    filter(!grepl('prevalence', outcome)) %>%
    mutate(double_counted = grepl('deaths', outcome),
           across(cause, tolower)) %>%
    group_by(scenario, outcome, cause, pollutant, double_counted, estimate) %>%
    summarise(across(number, sum)) %>%
    bind_rows(cost_totals,
              pm25_death_totals,
              death_totals,
              yld_totals) -> hia_out

  hia_out %<>% mutate(number = number * ifelse(grepl('absence', outcome), 1e-6, 1),
                      cause = ifelse(outcome==cause, '', cause),
                      outcome = ifelse(grepl('absence', outcome), 'work absence (mln sick leave days)', outcome))

  hia_out %>% filter(grepl('economic', outcome)) %>%
    mutate(number=number*usd_to_lcu/1000,
           outcome='total economic cost, bln R') %>%
    bind_rows(hia_out)
}

