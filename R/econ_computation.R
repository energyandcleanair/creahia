compute_econ_costs <- function(hia,
                               results_dir,
                               iso3s_of_interest=NULL,
                               gdp=get_gdp(), dict=get_dict(), valuation=get_valuation()){

  hia_cost <- get_hia_cost(hia, valuation, gdp, dict)

  cost_by_cause <- get_total_cost_by_cause(hia_cost) %T>% write_csv(file.path(results_dir, 'total_cost_by_cause.csv'))
  cost_by_region <- get_total_cost_by_region(hia_cost) %T>% write_csv(file.path(results_dir, 'total_cost_by_region.csv'))
  cost_by_country <- get_total_cost_by_country(hia_cost) %T>% write_csv(file.path(results_dir, 'total_cost_by_country.csv'))

  # One more detailed summary by country (with local currency)
  if(!is.null(iso3s_of_interest)){
    lapply(iso3s_of_interest, function(iso3){
      get_cost_by_cause_in_country(hia_cost, iso3, gdp, dict) %>%
        write_excel_csv(file.path(results_dir, sprintf('cost_by_cause_%s.csv',tolower(iso3))))
    })
  }


  # Forecast
  cost_forecast <- get_econ_forecast(hia_cost) %T>% write_csv(file.path(results_dir, 'health_and_cost_by_year.csv'))

  list(
    "hia_cost"=hia_cost,
    "cost_by_cause"=cost_by_cause,
    "cost_by_region"=cost_by_region,
    "cost_by_country"=cost_by_country,
    "cost_forecast"=cost_forecast
  )
}


get_hia_cost <- function(hia, valuation=get_valuation(), gdp=get_gdp(), dict=get_dict()){
  hia_cost <- hia %>%
    gather(Outcome, number, -scenario, -region_id, -region_name, -iso3, -estimate, -pop) %>%
    mutate(Outcome = Outcome %>%gsub('O3_8h', 'O3', .),
           Pollutant = Outcome %>% gsub('.*_', '', .) %>% toupper,
           Cause = Outcome %>% gsub('_.*', '', .)) %>%
    mutate(Outcome = Outcome %>% gsub('_[A-Za-z0-9]*$', '', .) %>%
             gsub('\\.[0-9]*to[0-9]*$', '', .) %>% gsub('.*_', '', .))

  hia_cost$Cause[grep('exac|sthma', hia_cost$Cause)] <- 'Asthma'

  hia_cost$AgeGrp <- "25+"
  hia_cost$AgeGrp[grepl("LRI\\.child", hia_cost$Cause)] <- "0-4"
  hia_cost$AgeGrp[grepl("PTB|LBW", hia_cost$Cause)] <- "Newborn"
  hia_cost$AgeGrp[grepl("0to17|1to18", hia_cost$Cause)] <- "0-18"


  suppressMessages(hia_cost %<>%
    left_join(valuation) %>%
    left_join(gdp) %>%
    filter(!is.na(Pollutant)) %>%
    dplyr::mutate(valuation = Valuation.2011.IntlDollars * (GDP.PPP.2011USD / 15914.05317)^Elasticity,
                  cost=number*valuation/1e6,
                  cost.USD = cost * GDP.currUSD / GDP.PPP.2011USD,
                  cost.LCU = cost * GDP.currLCU / GDP.PPP.2011USD) %>%
    ungroup)

  return(hia_cost)
}


get_total_cost_by_cause <- function(hia_cost, res_cols=c("low", "central", "high")){

  hia_cost %>%
    group_by(scenario, estimate, Outcome, Pollutant) %>%
    summarise_at('cost.USD', sum, na.rm=T) %>% na.omit %>%
    filter(Outcome != 'LBW') %>% spread(estimate, cost.USD) %>%
    mutate_at(res_cols, scales::comma, accuracy=0.01) %>% mutate(CI = paste0('(', low, ' - ', high, ')')) %>%
    sel(-low, -high)
}

get_total_cost_by_region <- function(hia_cost){

  hia_cost_total <- hia_cost %>%
    filter(Outcome != 'LBW') %>%
    group_by(scenario, iso3, region_id, region_name, estimate, Currency.Name, Currency.Code) %>%
    sel(starts_with('cost')) %>% summarise_all(sum, na.rm=T) %>%
    left_join(hia_cost %>% distinct(region_id, pop, GDP.PPP.2011USD)) %>%
    mutate(cost.percap.USD = cost.USD * 1e6 / pop,
           cost.perc = cost * 1e6 / (GDP.PPP.2011USD * pop))

  hia_cost_total %>%
    arrange(estimate, desc(cost.perc)) %>% sel(-starts_with('GDP'), -starts_with('valuation'), -cost)
}

get_total_cost_by_country <- function(hia_cost){

  hia_cost_national <- get_total_cost_by_region(hia_cost) %>%
    group_by(scenario, iso3, estimate) %>%
    summarise_at(c('cost.LCU', 'cost.USD'), sum)
}


get_cost_by_cause_in_country <- function(hia_cost, iso3, gdp=get_gdp(), dict=get_dict()){

  #valuations used
  currency_name=unique(gdp$Currency.Code[gdp$iso3==iso3])

  hia_focus_cost <- suppressMessages(hia_cost %>%
    distinct(iso3, Outcome, .keep_all=T) %>%
    filter(iso3==!!iso3) %>%
    mutate(valuation.USD = valuation * GDP.currUSD / GDP.PPP.2011USD,
           valuation.LCU = valuation * GDP.currLCU / GDP.PPP.2011USD) %>%
    sel(Outcome.Code=Outcome,
        Valuation.at.world.avg.GDP.2011.IntlDollars=Valuation.2011.IntlDollars,
        Valuation.in.COUNTRY.2011.IntlDollars=valuation,
        Valuation.in.COUNTRY.2019USD=valuation.USD,
        Valuation.in.COUNTRY.2019LCU=valuation.LCU) %>%
    distinct() %>% na.omit() %>%
    rename_with(function(x) x %>% gsub('COUNTRY', iso3, .) %>%
                  gsub('LCU', if(length(currency_name)> 0 && currency_name!="USD" && !is.na(currency_name)) currency_name else "LCU", .)) %>%
    filter(Outcome.Code != 'LBW') %>%
    right_join(dict %>% dplyr::rename(Outcome.Code=Code, Outcome=Long.name), .) %>%
    sel(-Outcome.Code))


  return(hia_focus_cost)
}


get_econ_forecast <- function(hia_cost, years=1980:2060,
                              pop_baseyr=2015, pop_targetyr=2019
){
  pop_proj <- get_pop_proj() %>%
    filter(iso3 %in% unique(hia_cost$iso3),
           year %in% years)

  #future impacts
  # names(hia3) %<>% recode('GID' = paste0('GID_',adm_level))
  # admLL@data %>% sel(starts_with('GID_'), starts_with('NAME_')) %>%
  #   right_join(hia3) -> hia3

  hia_cost_future <- hia_cost %>%
    filter(Outcome != 'LBW',
           Outcome %notin% c('Deaths', 'YLLs') | Cause %in% c('NCD.LRI', 'LRI.child', 'AllCause'),
           Outcome!='YLDs' | Cause != 'NCD.LRI') %>%
    group_by(scenario, estimate, iso3, region_name, Outcome, Cause, AgeGrp, Pollutant) %>%
    summarise_at(c('number', 'cost.USD'), sum, na.rm=T)

  #add new age groups to population data
  add_age_groups <- tibble(AgeGrp=c('25+','0-18','1-18','18-99', '20-65'),
                          AgeLow=c(25,0,0,20, 20),
                          AgeHigh=c(99,20,99, 99, 64),
                          multiplier=c(1,19/20,18/20, 82/80, 46/45))

  popproj_tot <- add_age_groups %>%
    group_by(AgeGrp) %>%
    group_modify(function(df, ...) {
      pop_proj %>% filter(Age_low>=df$AgeLow, Age_high<=df$AgeHigh) %>%
        group_by(LocID, iso3, Location, year) %>% sel(-contains('Age')) %>%
        mutate_if(is.numeric, multiply_by, df$multiplier) %>%
        summarise_all(sum) %>%
        mutate(death_rate = deaths / pop)
    }) %>% bind_rows(pop_proj) %>% distinct

  #flag mortality outcomes (to be scaled by number of deaths)
  hia_cost$fatal <- grepl('YLLs|YLDs|Deaths', hia_cost$Outcome)

  #gdp data
  gdp_historical <- get_gdp_historical()
  gdp_forecast <- get_gdp_forecast()

  gdp_all <- suppressMessages(full_join(gdp_historical, gdp_forecast)) %>%
    filter(iso3 %in% unique(hia_cost$iso3))

  gdp_all <- suppressMessages(gdp_all %>%
    left_join(popproj_tot) %>%
    mutate(GDP.realUSD = GDP.realUSD.tot*1000/pop) %>%
    group_by(iso3) %>%
    group_modify(function(df, ...) {
      PPP.scaling = df$GDP.PPP.2011USD[df$year==2019] / df$GDP.realUSD[df$year==2019]

      if(length(PPP.scaling)>0)
        df %<>% mutate(GDP.realUSD = GDP.realUSD)

      past.scaling = df %>% filter(!is.na(GDP.PPP.2011USD+GDP.currUSD)) %>% head(1)
      ind=df$year<past.scaling$year
      df$GDP.PPP.2011USD[ind] %<>% na.cover(df$GDP.currUSD[ind] * past.scaling$GDP.PPP.2011USD / past.scaling$GDP.currUSD)

      future.scaling = df %>% filter(!is.na(GDP.PPP.2011USD+GDP.realUSD)) %>% tail(1)
      ind=df$year>future.scaling$year
      df$GDP.PPP.2011USD[ind] %<>% na.cover(df$GDP.realUSD[ind] * past.scaling$GDP.PPP.2011USD / past.scaling$GDP.realUSD)

      return(df)
    }))

  # CHECK elast not used?
  # elast <- gdp_all %>% group_by(iso3) %>%
  #   group_map(function(df, iso3, ...) {
  #     df %<>% select_if(is.numeric)
  #     y1 = df %>% filter(year==2019) %>% distinct(GDP.PPP.tot, GDP.realUSD.tot)
  #     y0 = df %>% filter(year==2010) %>% distinct(GDP.PPP.tot, GDP.realUSD.tot)
  #     if(nrow(y0)==1 & nrow(y1)==1) { bind_cols(iso3, y1/y0)
  #     } else NULL
  #   })
  #
  # elast %>% subset(!is.null(.)) %>% bind_rows %>%
  #   mutate(elast = (GDP.PPP.tot-1) / (GDP.realUSD.tot-1)) %>% summarise_at('elast', mean, na.rm=T)

  # Check if any country missing population information
  missing_iso3s <- setdiff(unique(hia_cost$iso3), c(unique(popproj_tot$iso3), unique(gdp_forecast$iso3)))
  if(length(missing_iso3s)>0){
    warning("Missing population or GDP projection information of countries ",missing_iso3s,". These will be ignored")
  }

  pop_scaling <- suppressMessages(popproj_tot %>% ungroup %>%
    filter(iso3 %in% unique(hia_cost$iso3),
           AgeGrp %in% unique(hia_cost$AgeGrp),
           year %in% years) %>%
    full_join(gdp_all %>% sel(iso3, year, GDP.PPP.2011USD) %>%
                filter(year %in% years,
                       iso3 %in% unique(hia_cost$iso3),
                       !iso3 %in% missing_iso3s)) %>%
    pivot_longer(c(pop, deaths)) %>%
    group_by(iso3, AgeGrp, name) %>%
    mutate(scaling = value/value[year==pop_targetyr],
           GDPscaling = GDP.PPP.2011USD/GDP.PPP.2011USD[year==pop_targetyr]) %>%
    mutate(fatal=name=='deaths') %>% ungroup %>% sel(iso3, AgeGrp, year, fatal, scaling, GDPscaling) %>% distinct)

  hia_by_year <- suppressMessages(hia_cost %>% full_join(pop_scaling))

  hia_by_year_scaled <- hia_by_year %>% mutate(
    number = number*scaling,
    cost.USD = cost.USD*scaling*GDPscaling) %>%
    group_by(scenario, estimate, iso3, region_name, Outcome, Cause, Pollutant, year) %>%
    summarise_at(c('number', 'cost.USD'), sum)

  hia_by_year_scaled %>%
    filter(
      #fuel == 'COAL', #CHECK fuel hasn't been defined anywhere
      !is.na(scenario),
      year >= pop_baseyr) %>%
    # mutate(NAME_1 = ifelse(ISO3=='KOR', NAME_1, 'All')) %>%
    group_by(across(c(where(is.character), where(is.factor), year))) %>%
    summarise_all(sum, na.rm=T)
}
