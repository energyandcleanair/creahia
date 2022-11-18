compute_econ_costs <- function(hia,
                               results_dir,
                               iso3s_of_interest=NULL,
                               current_year=2019,
                               gdp=get_gdp(year=current_year),
                               dict=get_dict(),
                               valuation_version="default",
                               valuation=get_valuation(version=valuation_version),
                               projection_years=NULL,
                               suffix="",
                               gni_or_gdp='gni',
                               ...){

  hia_cost <- get_hia_cost(hia=hia,
                           valuation_version=valuation_version,
                           valuation=valuation,
                           current_year=current_year,
                           gdp=gdp,
                           dict=dict,
                           gni_or_gdp=gni_or_gdp) %T>%
    write_csv(file.path(results_dir, sprintf('cost_detailed_%s.csv', suffix)))


  cost_by_outcome <- get_total_cost_by_outcome(hia_cost) %T>% write_csv(file.path(results_dir, sprintf('cost_by_outcome%s.csv', suffix)))
  cost_by_region <- get_total_cost_by_region(hia_cost) %T>% write_csv(file.path(results_dir, sprintf('cost_by_region%s.csv', suffix)))
  cost_by_region_outcome <- get_total_cost_by_region_outcome(hia_cost) %T>% write_csv(file.path(results_dir, sprintf('cost_by_region_outcome%s.csv', suffix)))

#
#   # Forecast
#   if(length(projection_years)>0) {
#     cost_forecast <- get_econ_forecast(hia_cost, years=projection_years, ...) %T>%
#       write_csv(file.path(results_dir, sprintf('health_and_cost_by_year%s.csv', suffix)))
#   } else cost_forecast=NULL
#

  list(
    "hia_cost" = hia_cost,
    "cost_by_outcome" = cost_by_outcome,
    "cost_by_region" = cost_by_region,
    "cost_by_region_outcome" = cost_by_region_outcome
    # "cost_forecast" = cost_forecast
  ) %>%
    # lapply(add_total_deaths) %>%
    lapply(add_long_names)
}


get_hia_cost <- function(hia,
                         valuation_version="viscusi",
                         valuation=get_valuation(valuation_version),
                         current_year=2019,
                         gdp=get_gdp(year=current_year),
                         dict=get_dict(),
                         gni_or_gdp='gni'){

  hia_cost <- hia

  # wbstats::wb_data(c(gdp_intl2017='NY.GDP.PCAP.PP.KD', gni_intl2017='NY.GNP.PCAP.PP.KD'),
  #                  start_date=2017, end_date=2017,
  #                  country='World')
  # Values from above, at the time of creating valuation_viscusi.csv
  gdp_world_ppp_2017intl <- 16276
  gni_world_ppp_2017intl <- 15927

  nrow_before <- nrow(hia_cost)
  hia_cost <- hia_cost %>%
    left_join(dict %>% rename(Outcome=Code, Outcome.long=Long.name)) %>%
    left_join(valuation, by=c('Outcome')) %>%
    left_join(gdp, by=c('iso3')) %>%
    filter(!is.na(Pollutant)) %>%
    dplyr::mutate(
      valuation_intl2017_dollars = case_when(
        gni_or_gdp=='gni' ~ valuation_world_2017_intl2017_gni * (GNI.PC.PPP.2017USD / gni_world_ppp_2017intl)^elasticity,
        gni_or_gdp=='gdp' ~ valuation_world_2017_intl2017_gdp * (GDP.PC.PPP.2017USD / gdp_world_ppp_2017intl)^elasticity),
      cost_mn_2017USD = number * valuation_intl2017_dollars/1e6,
      cost_mn_currentUSD = cost_mn_2017USD * GDP.PC.PPP.currUSD / GDP.PC.PPP.2017USD,
      cost_mn_currentLCU = cost_mn_currentUSD * PPP.convLCUUSD,
      share_gdp = cost_mn_currentLCU / GDP.TOT.currLCU) %>%
    ungroup

  if(nrow(hia_cost) != nrow_before){stop('Wrong joins')}
  return(hia_cost)
}



get_total_cost_by_outcome <- function(hia_cost, res_cols=c("low", "central", "high")){

  gdp_gni <- hia_cost %>%
    ungroup() %>%
    distinct(region_id, GDP.TOT.currLCU, GDP.TOT.currUSD) %>%
    summarise_at(c('GDP.TOT.currLCU', 'GDP.TOT.currUSD'), sum)

  gdp_gni_tbl <-  tibble(unit=c('mn_currentLCU', 'mn_currentUSD'),
                         gdp=c(gdp_gni$GDP.TOT.currLCU / 1e6, gdp_gni$GDP.TOT.currUSD / 1e6))

  comma <- function(x){scales::comma(x, accuracy = 0.01)}

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(scenario, estimate, Outcome, Outcome.long) %>%
    summarise_at(c('number', 'cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm=T) %>%
    na.omit %>%
    tidyr::pivot_longer(cols=c(cost_mn_currentLCU, cost_mn_currentUSD, number),
                        names_prefix = 'cost_',
                        names_to='unit'
    ) %>%
    filter(Outcome != 'LBW') %>%
    left_join(gdp_gni_tbl) %>%
    mutate(share_gdp = value / gdp) %>%
    select(-c(gdp)) %>%
    tidyr::pivot_wider(names_from = unit,
                       values_from = c(value, share_gdp)) %>%
    select(-c(share_gdp_number)) %>%
    tidyr::pivot_wider(names_from='estimate',
                       values_from=c('value_mn_currentLCU', 'value_mn_currentUSD', 'value_number',
                                     'share_gdp_mn_currentLCU', 'share_gdp_mn_currentUSD')) %>%
    mutate(
      number = scales::comma(value_number_central, accuracy=1),
      CI_number = sprintf('(%s - %s)',
                          scales::comma(value_number_low, accuracy=1),
                          scales::comma(value_number_high, accuracy=1)),
      CI_mn_currentLCU = sprintf('(%s - %s)', comma(value_mn_currentLCU_low), comma(value_mn_currentLCU_high)),
      CI_mn_currentUSD = sprintf('(%s - %s)', comma(value_mn_currentUSD_low), comma(value_mn_currentUSD_high)),
      CI_share_gdp = sprintf('(%.1f%% - %.1f%%)', share_gdp_mn_currentLCU_low*100, share_gdp_mn_currentLCU_high*100),
      share_gdp = sprintf('%.1f%%', share_gdp_mn_currentLCU_central * 100),
    ) %>%
    mutate_at(c('value_mn_currentUSD_central', 'value_mn_currentUSD_central'), comma) %>%
    select(scenario,
           Outcome,
           Outcome.long,
           number,
           CI_number,
           cost_mn_currentUSD=value_mn_currentUSD_central,
           CI_mn_currentUSD,
           cost_mn_currentLCU=value_mn_currentLCU_central,
           CI_mn_currentLCU,
           share_gdp,
           CI_share_gdp
    )
}

get_total_cost_by_region <- function(hia_cost){

  gdp_gni <- hia_cost %>%
    ungroup() %>%
    distinct(region_id, GDP.TOT.currLCU, GDP.TOT.currUSD)

  comma <- function(x){scales::comma(x, accuracy = 0.01)}

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(scenario, estimate, region_id, pop, GDP.TOT.currLCU, GDP.TOT.currUSD) %>%
    summarise_at(c('cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm=T) %>%
    mutate(share_gdp = sprintf('%.1f%%', cost_mn_currentLCU*1e6/GDP.TOT.currLCU*100),
           # number = scales::comma(number, accuracy=1),
           cost_mn_currentLCU = comma(cost_mn_currentLCU),
           cost_mn_currentUSD = comma(cost_mn_currentUSD)
           ) %>%
    ungroup() %>%
    select(-starts_with('GDP'))
}


# get_total_cost_by_country <- function(hia_cost){
#
#   hia_cost_national <- get_total_cost_by_region(hia_cost) %>%
#     group_by(scenario, iso3, estimate) %>%
#     summarise_at(c('cost.mnLCU', 'cost.mnUSD'), sum)
# }


get_total_cost_by_region_outcome <- function(hia_cost, iso3, gdp=get_gdp(), dict=get_dict()){

  gdp_gni <- hia_cost %>%
    ungroup() %>%
    distinct(region_id, GDP.TOT.currLCU, GDP.TOT.currUSD)

  comma <- function(x){scales::comma(x, accuracy = 0.01)}

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(scenario, estimate, region_id, pop, Outcome, Outcome.long, GDP.TOT.currLCU, GDP.TOT.currUSD) %>%
    summarise_at(c('cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm=T) %>%
    mutate(share_gdp = sprintf('%.1f%%', cost_mn_currentLCU*1e6/GDP.TOT.currLCU*100),
           # number = scales::comma(number, accuracy=1),
           cost_mn_currentLCU = comma(cost_mn_currentLCU),
           cost_mn_currentUSD = comma(cost_mn_currentUSD)
    ) %>%
    ungroup() %>%
    select(-starts_with('GDP'))
}


get_econ_forecast <- function(hia_cost, years, pop_targetyr=2019, GDP_scaling=F, discount_rate=.03
){
  pop_proj <- get_pop_proj() %>%
    filter(iso3 %in% unique(hia_cost$iso3),
           year %in% c(pop_targetyr, years))

  hia_cost_future <- hia_cost %>%
    filter(Outcome != 'LBW',
           Outcome %notin% c('Deaths', 'YLLs') | Cause %in% c('NCD.LRI', 'LRI.child', 'AllCause'),
           Outcome!='YLDs' | Cause != 'NCD.LRI') %>%
    group_by(across(c(scenario, estimate, any_of(c('iso3', 'region_name')),
                      Outcome, Cause, AgeGrp, Pollutant))) %>%
    summarise_at(c('number', 'cost.mnUSD'), sum, na.rm=T)

  #add new age groups to population data
  add_age_groups <- tibble(AgeGrp=c('25+','0-18','1-18','18-99', '20-65'),
                          AgeLow=c(25, 0, 0, 20, 20),
                          AgeHigh=c(99, 20, 99, 99, 64),
                          multiplier=c(1, 19/20, 18/20, 82/80, 46/45))

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

  pop_scaling <- suppressMessages(
    popproj_tot %>% ungroup %>%
      filter(iso3 %in% unique(hia_cost$iso3),
             AgeGrp %in% unique(hia_cost$AgeGrp),
             year %in% c(pop_targetyr, years)) %>%
      pivot_longer(c(pop, deaths)) %>%
      group_by(iso3, AgeGrp, name) %>%
      dplyr::mutate(scaling = value/value[year==pop_targetyr],
                    GDPscaling = 1) %>%
      mutate(fatal=name=='deaths') %>% ungroup %>%
      sel(iso3, AgeGrp, year, fatal, scaling, GDPscaling) %>%
      distinct
  )

  missing_iso3s <- setdiff(unique(hia_cost$iso3), c(unique(popproj_tot$iso3)))

  if(GDP_scaling) {
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

    pop_scaling %<>% full_join(gdp_all %>% sel(iso3, year, GDP.PPP.2011USD) %>%
                                 filter(year %in% years,
                                        iso3 %in% unique(hia_cost$iso3),
                                        !iso3 %in% missing_iso3s)) %>%
      mutate(GDPscaling = GDP.PPP.2011USD/GDP.PPP.2011USD[year==pop_targetyr] / (1+discount_rate)^(year-pop_targetyr))

    missing_iso3s <- setdiff(unique(hia_cost$iso3), c(unique(popproj_tot$iso3), unique(gdp_forecast$iso3)))
  }

  # Check if any country missing population information
  if(length(missing_iso3s)>0){
    warning("Missing population or GDP projection information of countries ",missing_iso3s,". These will be ignored")
  }

  hia_by_year <- suppressMessages(hia_cost %>% full_join(pop_scaling))

  hia_by_year_scaled <- hia_by_year %>% mutate(
    number = number*scaling,
    cost.mnUSD = cost.mnUSD*scaling*GDPscaling) %>%
    group_by(across(c(scenario, estimate, any_of(c('iso3', 'region_id', 'region_name')),
                      Outcome, Cause, Pollutant, year))) %>%
    summarise_at(c('number', 'cost.mnUSD'), sum)

  hia_by_year_scaled %>%
    group_by(across(c(where(is.character), where(is.factor), year))) %>%
    summarise_all(sum, na.rm=T)
}

#
# add_total_deaths <- function(df,
#                              include_PM_causes = 'NCD\\.LRI|LRI\\.child',
#                              include_NO2_causes = 'NCD\\.LRI|LRI\\.child|AllCause') {
#
#   if('Cause' %in% names(df)) {
#     df %>%
#       group_by(across(c(where(is.character), where(is.factor), -Cause))) %>%
#       filter(
#         (!Outcome %in% c('Deaths', 'YLLs')) | (tolower(Pollutant)=='total')
#        ) %>%
#       summarise_at(vars(c(starts_with('number'), starts_with('cost.'))), sum, na.rm=T) %>%
#       mutate(Cause='Total', Pollutant='Total') %>%
#       View()
#       bind_rows(df, .)
#   } else df
# }
