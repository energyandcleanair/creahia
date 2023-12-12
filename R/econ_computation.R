compute_econ_costs <- function(hia,
                               results_dir = NULL,
                               iso3s_of_interest = NULL,
                               current_year = 2019,
                               gdp = get_gdp(year = current_year),
                               dict = get_dict(),
                               valuation_version = "default",
                               valuation = get_valuation(version = valuation_version),
                               projection_years = NULL,
                               suffix = "",
                               ...) {

  hia_cost <- get_hia_cost(hia = hia,
                           valuation_version = valuation_version,
                           valuation = valuation,
                           current_year = current_year,
                           gdp = gdp,
                           dict = dict)

  cost_by_outcome <- get_total_cost_by_outcome(hia_cost)
  cost_by_region <- get_total_cost_by_region(hia_cost)

  if(!is.null(results_dir)) {
    write_csv(hia_cost, file.path(results_dir, sprintf('cost_detailed%s.csv', suffix)))

    # Ceate summary tables
    dir.create(file.path(results_dir, 'formatted'))
    cost_by_outcome %>%
      write_csv(file.path(results_dir, sprintf('cost_by_outcome%s.csv', suffix)))

    cost_by_outcome %>%
      format_hia_table() %>%
      write_csv(file.path(results_dir, sprintf('formatted/cost_by_outcome%s.csv', suffix)))

    cost_by_region %>%
      write_csv(file.path(results_dir, sprintf('cost_by_region%s.csv', suffix)))

    cost_by_region %>%
      format_hia_table() %>%
      write_csv(file.path(results_dir, sprintf('formatted/cost_by_region%s.csv', suffix)))

  }
  # cost_by_region_outcome <- get_total_cost_by_region_outcome(hia_cost) %T>% write_csv(file.path(results_dir, sprintf('cost_by_region_outcome%s.csv', suffix)))

  #   # Forecast
  #   if(length(projection_years)>0) {
  #     cost_forecast <- get_econ_forecast(hia_cost, years=projection_years, ...) %T>%
  #       write_csv(file.path(results_dir, sprintf('health_and_cost_by_year%s.csv', suffix)))
  #   } else cost_forecast=NULL
  #

  list("hia_cost" = hia_cost,
       "cost_by_outcome" = cost_by_outcome,
       "cost_by_region" = cost_by_region) %>%
    # "cost_by_region_outcome" = cost_by_region_outcome
    # "cost_forecast" = cost_forecast %>%
    # lapply(add_total_deaths) %>%
    lapply(add_long_names)
}


get_hia_cost <- function(hia,
                         valuation_version = "viscusi",
                         valuation = get_valuation(valuation_version),
                         current_year = 2019,
                         gdp = get_gdp(year = current_year),
                         dict = get_dict()) {

  hia_cost <- hia

  # Values from above, at the time of creating valuation_viscusi.csv
  gdp_world_2017_current <- 10825.9
  gdp_world_2017_constant2015 <- 10625.26
  gdp_world_ppp_2017intl <- 16276.48
  gni_world_ppp_2017intl <- 15927.18

  valuation <- valuation %>% select(-any_of('Outcome_name'))

  nrow_before <- nrow(hia_cost)
  hia_cost <- hia_cost %>%
    left_join(dict %>% rename(Outcome = Code, Outcome.long = Long.name)) %>%
    left_join(valuation, by = c('Outcome')) %>%
    left_join(gdp, by = c('iso3')) %>%
    filter(!is.na(Pollutant)) %>%
    dplyr::mutate(
      lcu_per_usd = GDP.PC.currLCU / GDP.PC.currUSD,
      valuation_current_usd = case_when(
        gni_or_gdp == 'gni' & ppp ~ valuation_world_2017 * (GNI.PC.PPP.2017USD / gni_world_ppp_2017intl)^elasticity * GNI.PC.PPP.currUSD / GNI.PC.PPP.2017USD * PPP.convLCUUSD / lcu_per_usd,
        gni_or_gdp == 'gdp' & ppp ~ valuation_world_2017 * (GDP.PC.PPP.2017USD / gdp_world_ppp_2017intl)^elasticity * GDP.PC.PPP.currUSD / GDP.PC.PPP.2017USD * PPP.convLCUUSD / lcu_per_usd,
        gni_or_gdp == 'gdp' & !ppp ~ valuation_world_2017 * (GDP.PC.2015USD / gdp_world_2017_constant2015)^elasticity * GDP.PC.currUSD / GDP.PC.2015USD ,
        T ~ NA_real_), # Other cases not yet supported
      valuation_current_lcu = valuation_current_usd * lcu_per_usd,
      cost_mn_currentUSD = number * valuation_current_usd / 1e6,
      cost_mn_currentLCU = cost_mn_currentUSD * lcu_per_usd,
      share_gdp = cost_mn_currentLCU * 1e6 / GDP.TOT.currLCU
    ) %>% ungroup %>%
    relocate(starts_with(c('GDP', 'GNI', 'PPP')), .after = everything())

  # checks
  if(nrow(hia_cost) != nrow_before) {stop('Wrong joins')}

  missing_outcome <- hia_cost %>% filter(is.na(valuation_current_usd)) %>%
    distinct(Outcome) %>% pull()
  if(length(missing_outcome) > 0) {
    warning('The following outome(s) do not have valuations: ',
            paste(missing_outcome, collapse = ', '))
  }
  return(hia_cost)
}


get_total_cost_by_outcome <- function(hia_cost) {

  gdp <- hia_cost %>%
    ungroup() %>%
    distinct(region_id, GDP.TOT.currLCU, GDP.TOT.currUSD) %>%
    summarise_at(c('GDP.TOT.currLCU', 'GDP.TOT.currUSD'), sum)

  gdp_tbl <-  tibble(unit = c('mn_currentLCU', 'mn_currentUSD'),
                     gdp = c(gdp$GDP.TOT.currLCU / 1e6, gdp$GDP.TOT.currUSD / 1e6))

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(across(c(any_of('scenario'), estimate, Outcome, Outcome.long))) %>%
    summarise_at(c('number', 'cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm = T) %>%
    na.omit %>%
    tidyr::pivot_longer(cols = c(cost_mn_currentLCU, cost_mn_currentUSD, number),
                        names_prefix = 'cost_',
                        names_to = 'unit') %>%
    filter(Outcome != 'LBW') %>%
    left_join(gdp_tbl) %>%
    mutate(share_gdp = value / gdp) %>%
    select(-c(gdp)) %>%
    tidyr::pivot_wider(names_from = unit,
                       values_from = c(value, share_gdp)) %>%
    select(-c(share_gdp_number, share_gdp_mn_currentUSD)) %>%
    rename(share_gdp = share_gdp_mn_currentLCU,
           number = value_number) %>%
    rename_with(~stringr::str_replace(.x, 'value_', 'cost_')) %>%
    mutate(valuation_currentUSD = cost_mn_currentUSD * 1e6 / number,
           valuation_currentLCU = cost_mn_currentLCU * 1e6 / number)
}


format_hia_table <- function(table, CI_underneath = F) {

  values <- intersect(names(table),
                      c('cost_mn_currentLCU', 'cost_mn_currentUSD', 'number', 'share_gdp'))
  groups <- intersect(names(table),
                      c('scenario', 'Outcome', 'Cause', 'Outcome.long', 'region_id',
                        'Pollutant', 'AgeGrp', 'double_counted'))

  formatted <- table %>%
    select_at(c(values, groups, 'estimate')) %>%
    tidyr::pivot_longer(cols = values,
                        names_to = 'indicator') %>%
    tidyr::pivot_wider(names_from = 'estimate', values_from = 'value') %>%
    mutate(CI = case_when(
      grepl('number', indicator) ~ sprintf('(%s - %s)', scales::comma(low, 1), scales::comma(high, 1)),
      grepl('share', indicator) ~ sprintf('(%.1f%% - %.1f%%)', low*100, high*100),
      grepl('cost_mn', indicator) ~ sprintf('(%s - %s)', scales::comma(low, 1), scales::comma(high, 1)),
      T ~ sprintf('(%s - %s)', scales::comma(low, 0.1), scales::comma(high, 0.1))),
      central = case_when(
        grepl('number', indicator) ~ scales::comma(central, 1),
        grepl('share', indicator) ~ scales::percent(central, .1),
        grepl('cost_mn', indicator) ~ scales::comma(central, 1),
        T ~ scales::comma(central, 1))
    ) %>%
    select(-c(low, high))

  if(CI_underneath) {
    formatted <- formatted %>%
      mutate(central = sprintf('%s\n%s', central, CI)) %>%
      select(-c(CI))
  }

  formatted %>%
    pivot_wider(names_from = indicator,
                values_from = intersect(names(.), c('central', 'CI'))) %>%
    rename_with(~gsub('central_', '', .x))
}


get_total_cost_by_region <- function(hia_cost) {

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(across(c(any_of('scenario'), estimate, region_id, pop, GDP.TOT.currLCU,
                      GDP.TOT.currUSD))) %>%
    summarise_at(c('cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm = T) %>%
    mutate(share_gdp = cost_mn_currentLCU * 1e6 / GDP.TOT.currLCU) %>%
    ungroup() %>%
    select(-starts_with('GDP'))
}


# get_total_cost_by_country <- function(hia_cost){
#
#   hia_cost_national <- get_total_cost_by_region(hia_cost) %>%
#     group_by(scenario, iso3, estimate) %>%
#     summarise_at(c('cost.mnLCU', 'cost.mnUSD'), sum)
# }


get_total_cost_by_region_outcome <- function(hia_cost, iso3, gdp = get_gdp(),
                                             dict = get_dict()) {

  gdp_gni <- hia_cost %>%
    ungroup() %>%
    distinct(region_id, GDP.TOT.currLCU, GDP.TOT.currUSD)

  comma <- function(x) {scales::comma(x, accuracy = 0.01)}

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(across(c(any_of('scenario'), estimate, region_id, pop, Outcome,
                      Outcome.long, GDP.TOT.currLCU, GDP.TOT.currUSD))) %>%
    summarise_at(c('cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm = T) %>%
    mutate(share_gdp = sprintf('%.1f%%', cost_mn_currentLCU * 1e6 / GDP.TOT.currLCU * 100),
           # number = scales::comma(number, accuracy=1),
           cost_mn_currentLCU = comma(cost_mn_currentLCU),
           cost_mn_currentUSD = comma(cost_mn_currentUSD)) %>%
    ungroup() %>%
    select(-starts_with('GDP'))
}


get_econ_forecast <- function(hia_cost, years, pop_targetyr = 2019,
                              GDP_scaling = F, discount_rate = .03) {

  if(!is.data.frame(hia_cost)) {
    hia_cost <- hia_cost$hia_cost
  }

  pop_proj <- get_pop_proj() %>% # from hia_data.R or gis_data.R?
    filter(iso3 %in% unique(hia_cost$iso3),
           year %in% c(pop_targetyr, years))

  # add new age groups to population data
  add_age_groups <- tibble(AgeGrp = c('25+','0-18','1-18','18-99', '20-65'),
                           AgeLow = c(25, 0, 0, 20, 20),
                           AgeHigh = c(99, 20, 99, 99, 64),
                           multiplier = c(1, 19/20, 18/20, 82/80, 46/45))

  popproj_tot <- add_age_groups %>%
    group_by(AgeGrp) %>%
    group_modify(function(df, ...) {
      pop_proj %>% filter(Age_low >= df$AgeLow, Age_high <= df$AgeHigh) %>%
        group_by(LocID, iso3, Location, year) %>%
        sel(-contains('Age')) %>%
        mutate_if(is.numeric, multiply_by, df$multiplier) %>%
        summarise_all(sum) %>%
        mutate(death_rate = deaths / pop)
    }) %>% bind_rows(pop_proj) %>% distinct

  # flag mortality outcomes (to be scaled by number of deaths)
  hia_cost$fatal <- grepl('YLLs|YLDs|Deaths', hia_cost$Outcome)

  pop_scaling <- suppressMessages(
    popproj_tot %>% ungroup %>%
      filter(iso3 %in% unique(hia_cost$iso3),
             AgeGrp %in% unique(hia_cost$AgeGrp),
             year %in% c(pop_targetyr, years)) %>%
      pivot_longer(c(pop, deaths)) %>%
      group_by(iso3, AgeGrp, name) %>%
      dplyr::mutate(scaling = value / value[year == pop_targetyr],
                    GDPscaling = 1) %>%
      mutate(fatal = name == 'deaths') %>%
      ungroup %>%
      sel(iso3, AgeGrp, year, fatal, scaling, GDPscaling) %>%
      distinct
  )

  missing_iso3s <- setdiff(unique(hia_cost$iso3), c(unique(popproj_tot$iso3)))

  if(GDP_scaling) {
    # TODO NOT WORKING YET
    stop("GDP SCALING NEEDS TO BE FIXED")
    #gdp data
    gdp_historical <- get_gdp()
    gdp_forecast <- get_gdp_forecast()

    gdp_all <- suppressMessages(full_join(gdp_historical, gdp_forecast)) %>%
      filter(iso3 %in% unique(hia_cost$iso3))

    gdp_all <- suppressMessages(
      gdp_all %>%
        left_join(popproj_tot) %>%
        mutate(GDP.realUSD = GDP.realUSD.tot * 1000 / pop) %>% # HT: Seems to be wrong: pop is the age group population
        group_by(iso3) %>%
        group_modify(function(df, ...) {
          PPP.scaling = df$GDP.PC.PPP.2017USD[df$year == 2019] / df$GDP.PC.currUSD[df$year == 2019]

          if(length(PPP.scaling) > 0)
            df <- df %>% mutate(GDP.realUSD = GDP.realUSD)

          past.scaling <- df %>% filter(!is.na(GDP.PPP.2011USD + GDP.currUSD)) %>% head(1)
          ind <- df$year < past.scaling$year
          df$GDP.PPP.2011USD[ind] <- df$GDP.PPP.2011USD[ind] %>%
            na.cover(df$GDP.currUSD[ind] * past.scaling$GDP.PPP.2011USD / past.scaling$GDP.currUSD)

          future.scaling = df %>% filter(!is.na(GDP.PPP.2011USD+GDP.realUSD)) %>% tail(1)
          ind <- df$year > future.scaling$year
          df$GDP.PPP.2011USD[ind] <- df$GDP.PPP.2011USD[ind] %>%
            na.cover(df$GDP.realUSD[ind] * past.scaling$GDP.PPP.2011USD / past.scaling$GDP.realUSD)

          return(df)
        })
    )

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

    pop_scaling <- pop_scaling %>%
      full_join(gdp_all %>% sel(iso3, year, GDP.PPP.2011USD) %>%
                  filter(year %in% years,
                         iso3 %in% unique(hia_cost$iso3),
                         !iso3 %in% missing_iso3s)) %>%
      mutate(GDPscaling = GDP.PPP.2011USD / GDP.PPP.2011USD[year == pop_targetyr] /
               (1 + discount_rate)^(year - pop_targetyr))

    missing_iso3s <- setdiff(unique(hia_cost$iso3),
                             c(unique(popproj_tot$iso3), unique(gdp_forecast$iso3)))
  }

  # Check if any country missing population information
  if(length(missing_iso3s) > 0) {
    warning("Missing population or GDP projection information of countries ",
            missing_iso3s, ". These will be ignored")
  }

  hia_by_year <- suppressMessages(hia_cost %>% select(-year) %>% full_join(pop_scaling))

  hia_by_year %>% mutate(number = number * scaling,
                         cost_mn_currentUSD = cost_mn_currentUSD * scaling * GDPscaling)
}
