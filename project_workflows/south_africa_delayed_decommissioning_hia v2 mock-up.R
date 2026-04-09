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
seq.Date(ymd('2023-01-01'), ymd('2050-12-31'), by='month') %>%
  lapply(function(dt) {
    decomm_dates %>% filter(decommissioning_date>dt) %>%
      group_by(scenario) %>% summarise(across(MW, sum)) %>% mutate(date=dt)
  }) %>% bind_rows -> t1

t1 %>%
  ggplot(aes(date, MW, col=scenario)) + geom_line() + x_at_zero()

t1 %>% filter(date==max(date))

#calculate percentage of operation by year
get_current_MW <- function(decommissioning_date, MW, year) {
  year_start <- ymd(paste(year,1,1))
  days_in_year <- as.numeric(ymd(paste(year+1,1,1))-year_start)
  current_MW <- MW * as.numeric(decommissioning_date - year_start) / days_in_year

  case_when(year(decommissioning_date)>year~MW,
            year(decommissioning_date)<year~0,
            T~current_MW)
}
