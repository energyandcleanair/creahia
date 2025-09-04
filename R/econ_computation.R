compute_econ_costs <- function(hia,
                               results_dir = NULL,
                               iso3s_of_interest = NULL,
                               current_year = 2019,
                               dict = get_dict(),
                               valuation_version = "default",
                               projection_years = NULL,
                               suffix = "",
                               ...) {

  hia_cost <- get_hia_cost(hia = hia,
                           valuation_version = valuation_version,
                           current_year = current_year,
                           dict = dict)

  cost_by_outcome <- get_total_cost_by_outcome(hia_cost)
  cost_by_region <- get_total_cost_by_region(hia_cost)

  if(!is.null(results_dir)) {
    write_csv(hia_cost, file.path(results_dir, sprintf('cost_detailed%s.csv', suffix)))

    # Ceate summary tables
    dir.create(file.path(results_dir, 'formatted'), showWarnings = F, recursive = T)
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

  # Forecast
  if(length(projection_years)>0) {
    cost_forecast <- get_econ_forecast(hia_cost, years=projection_years, ...) %T>%
      write_csv(file.path(results_dir, sprintf('health_and_cost_by_year%s.csv', suffix)))
  } else cost_forecast=NULL


  list("hia_cost" = hia_cost,
       "cost_by_outcome" = cost_by_outcome,
       "cost_by_region" = cost_by_region,
       "cost_forecast" = cost_forecast) %>%
    lapply(add_long_names)
}


get_hia_cost <- function(hia,
                         valuation_version = "viscusi",
                         valuation = get_valuation(valuation_version),
                         current_year = 2019,
                         dict = get_dict()) {



  # Get valuation for the country/years in the HIA dataset
  iso3s <- unique(hia$iso3)
  years <- current_year
  valuation <- get_valuations(valuation_version=valuation_version,
                              iso3s=iso3s,
                              years=years)

  hia_cost <- hia %>%
    left_join(valuation,
              by=c("Outcome", "iso3")) %>%
    rename(valuation_current_usd = valuation_usd) %>%
    mutate(
      valuation_current_lcu = valuation_current_usd * lcu_per_usd,
      cost_mn_currentUSD = number * valuation_current_usd / 1e6,
      cost_mn_currentLCU = cost_mn_currentUSD * lcu_per_usd,
      share_gdp = cost_mn_currentLCU * 1e6 / gdp_curr_usd
    )

  # checks
  stopifnot(nrow(hia_cost) == nrow(hia))

  missing_outcome <- hia_cost %>% filter(is.na(valuation_current_usd)) %>%
    distinct(Outcome) %>% pull()
  if(length(missing_outcome) > 0) {
    message('The following outome(s) do not have valuations: ',
            paste(missing_outcome, collapse = ', '))
  }
  return(hia_cost)
}


get_total_cost_by_outcome <- function(hia_cost) {

  hia_cost %>%
    filter(!double_counted) %>%
    group_by(across(c(any_of('scenario'), estimate, Outcome))) %>%
    summarise_at(c('number', 'cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm = T) %>%
    ungroup() %>%
    na.omit %>%
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
    # remove low and high columns
    select(-any_of(c("low", "high")))

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
    group_by(across(c(any_of('scenario'), estimate, region_id, pop))) %>%
    summarise_at(c('cost_mn_currentUSD', 'cost_mn_currentLCU'), sum, na.rm = T) %>%
    ungroup()
}


get_total_cost_by_region_outcome <- function(hia_cost,
                                             iso3,
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


get_econ_forecast <- function(hia_cost,
                              years,
                              pop_targetyr = 2019,
                              GDP_scaling = F,
                              discount_rate = .03) {

  if(!is.data.frame(hia_cost)) {
    hia_cost <- hia_cost$hia_cost
  }

  pop_proj <- get_pop_proj() %>%
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
    popproj_tot %>%
    ungroup %>%
      filter(iso3 %in% unique(hia_cost$iso3),
             AgeGrp %in% unique(hia_cost$AgeGrp),
             year %in% c(pop_targetyr, years)) %>%
      pivot_longer(c(pop, deaths)) %>%
      group_by(iso3, AgeGrp, name) %>%
      dplyr::mutate(pop_scaling = value / value[year == pop_targetyr],
                    GDPscaling = 1) %>%
      mutate(fatal = name == 'deaths') %>%
      ungroup %>%
      sel(iso3, AgeGrp, year, fatal, pop_scaling, GDPscaling) %>%
      distinct
  )

  missing_iso3s_pop <- setdiff(unique(hia_cost$iso3), c(unique(popproj_tot[popproj_tot$year %in% years,]$iso3)))
  missing_iso3s_gdp <- c()

  if(GDP_scaling) {

    gdp_scaling <- get_gdp_scaling(iso3=unique(hia_cost$iso3))

    pop_scaling <- pop_scaling %>%
      full_join(gdp_scaling %>%
                  sel(iso3, year, GDP.PC.PPP.constUSD) %>%
                  filter(year %in% c(pop_targetyr, years),
                         iso3 %in% unique(hia_cost$iso3),
                         !iso3 %in% missing_iso3s_pop)) %>%
      group_by(iso3) %>%
      mutate(GDPscaling = GDP.PC.PPP.constUSD / GDP.PC.PPP.constUSD[year == pop_targetyr] /
               (1 + discount_rate)^(year - pop_targetyr)) %>%
      sel(-GDP.PC.PPP.constUSD) %>%
      ungroup()

    missing_iso3s_gdp <- setdiff(unique(hia_cost$iso3),
                                 c(unique(gdp_scaling[gdp_scaling$year %in% years,]$iso3)))
  }

  # Check if any country missing population information
  if(length(missing_iso3s_pop) > 0) {
    warning("Missing population projection information for the following countries: ",
            paste(missing_iso3s_pop, collapse = ', '), ". Will be ignored.")
  }
  if(length(missing_iso3s_gdp) > 0) {
    warning("Missing GDP information for the following countries: ",
            paste(missing_iso3s_gdp, collapse = ', '), ". Will be ignored.")
  }


  hia_by_year <- suppressMessages(hia_cost %>%
                                    select(-year) %>%
                                    full_join(pop_scaling,
                                              relationship = "many-to-many"
                                              ))

  hia_by_year %>% mutate(number = number * pop_scaling,
                         cost_mn_currentUSD = cost_mn_currentUSD * pop_scaling * GDPscaling)
}
