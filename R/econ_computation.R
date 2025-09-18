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
    cost_forecast <- get_econ_forecast(hia_cost, forecast_years = projection_years, ...) %T>%
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


#' Build age-group specific population scaling factors
#'
#' Internal helper that prepares population and death-based scaling indices by
#' `iso3`/`AgeGrp`/`fatal`/`year` for the requested horizon.
#'
#' @param hia_cost Data frame used only to derive `iso3` and `AgeGrp` coverage.
#' @param reference_year Integer reference year.
#' @param forecast_years Integer vector of forecast years.
#'
#' @return A list with:
#'   - `popproj_tot`: population projections including synthesized age groups
#'   - `pop_scaling`: tidy table with columns `iso3`, `AgeGrp`, `year`, `fatal`,
#'      `pop_scaling`.
#' @keywords internal
#' @noRd
compute_population_scaling <- function(hia_cost, reference_year, forecast_years) {

  pop_proj <- get_pop_proj() %>%
    filter(iso3 %in% unique(hia_cost$iso3),
           year %in% c(reference_year, forecast_years))

  # add new age groups to population data (heuristic multipliers; subject to revision)
  add_age_groups <- tibble(age_group = c('25+','0-18','1-18','18-99', '20-65'),
                           age_start = c(25, 0, 0, 20, 20),
                           age_end = c(99, 20, 20, 99, 64),
                           multiplier = c(1, 19/20, 18/20, 82/80, 46/45))

  popproj_tot <- add_age_groups %>%
    group_by(age_group) %>%
    group_modify(function(df, ...) {
      pop_proj %>%
        filter(age_start >= df$age_start, age_start + age_span - 1 <= df$age_end) %>%
        group_by(location_id, iso3, year) %>%
        sel(-contains('age')) %>%
        mutate_if(is.numeric, multiply_by, df$multiplier) %>%
        summarise_all(sum)
    }) %>%
    bind_rows(pop_proj) %>%
    distinct(iso3, year, age_group, pop, deaths) %>%
    ungroup()

  # build scaling factors for both population and deaths
  pop_scaling <- suppressMessages(
    popproj_tot %>%
      ungroup %>%
      filter(iso3 %in% unique(hia_cost$iso3),
             age_group %in% unique(hia_cost$AgeGrp),
             year %in% c(reference_year, forecast_years)) %>%
      pivot_longer(c(pop, deaths)) %>%
      group_by(iso3, age_group, name) %>%
      dplyr::mutate(pop_scaling = value / value[year == reference_year]) %>%
      mutate(fatal = name == 'deaths') %>%
      ungroup %>%
      sel(iso3, age_group, year, fatal, pop_scaling) %>%
      distinct
  )

  list(popproj_tot = popproj_tot, pop_scaling = pop_scaling)
}

#' Build GDP per-capita PPP scaling factors (and discounted variant)
#'
#' @param hia_cost Data frame used to derive `iso3` coverage.
#' @param reference_year Integer reference year.
#' @param forecast_years Integer vector of forecast years.
#' @param discount_rate Numeric annual discount rate.
#'
#' @return A data.frame with columns `iso3`, `year`, `gdp_pc_scaling`,
#'   `gdp_scaling`.
#' @keywords internal
#' @noRd
compute_gdp_scaling <- function(hia_cost, reference_year, forecast_years, discount_rate) {
  gdp_scaling <- get_gdp_scaling(iso3 = unique(hia_cost$iso3))
  # keep only needed years and columns; create both pc scaling and discounted scaling
  gdp_scaling %>%
    sel(iso3, year, GDP.PC.PPP.constUSD) %>%
    filter(year %in% c(reference_year, forecast_years),
           iso3 %in% unique(hia_cost$iso3)) %>%
    group_by(iso3) %>%
    mutate(gdp_pc_scaling = GDP.PC.PPP.constUSD / GDP.PC.PPP.constUSD[year == reference_year],
           gdp_scaling = gdp_pc_scaling / (1 + discount_rate)^(year - reference_year)) %>%
    sel(-GDP.PC.PPP.constUSD) %>%
    ungroup()
}

#' Apply population and GDP scaling to HIA rows and expand by year
#'
#' @param hia_cost HIA rows at reference year.
#' @param pop_scaling Output of `compute_population_scaling()`.
#' @param gdp_scaling_tbl Output of `compute_gdp_scaling()` or NULL.
#' @param reference_year Integer.
#' @param forecast_years Integer vector.
#'
#' @return A data.frame with rows replicated across requested years and scaled
#'   `number`, `cost_mn_currentUSD` and `cost_mn_currentLCU` (if present). Also
#'   contains `pop_scaling`, `gdp_pc_scaling`, `gdp_scaling`, and recomputed
#'   `share_gdp` when GDP totals are available.
#' @keywords internal
#' @noRd
apply_econ_scaling <- function(hia_cost, pop_scaling, gdp_scaling_tbl = NULL, reference_year, forecast_years) {
  # set outcome fatal flag: YLLs and Deaths are fatal; YLDs are non-fatal
  hia_cost <- hia_cost %>% mutate(fatal = grepl('YLLs|Deaths', Outcome))

  # ensure unique pop_scaling keys
  key_cols <- c('iso3','age_group','fatal','year')
  if(nrow(pop_scaling %>% sel(all_of(key_cols)) %>% distinct()) != nrow(pop_scaling)) {
    stop('Population scaling table contains duplicate keys for iso3/AgeGrp/fatal/year. Aborting.')
  }

  # build combined scaling table (add GDP columns; default to 1)
  if(!is.null(gdp_scaling_tbl)) {
    scaling <- pop_scaling %>% left_join(gdp_scaling_tbl, by = c('iso3','year'))
  } else {
    scaling <- pop_scaling %>% mutate(gdp_scaling = 1, gdp_pc_scaling = 1)
  }

  # warn on missing iso3s
  missing_iso3s_pop <- setdiff(unique(hia_cost$iso3), unique(scaling$iso3[scaling$year %in% forecast_years]))
  if(length(missing_iso3s_pop) > 0) {
    warning(sprintf('Missing population projection information for %d iso3(s): %s. These rows will be dropped in forecasting.',
                    length(missing_iso3s_pop), paste(missing_iso3s_pop, collapse = ', ')))
  }
  if(!is.null(gdp_scaling_tbl)){
    missing_iso3s_gdp <- setdiff(unique(hia_cost$iso3), unique(gdp_scaling_tbl$iso3[gdp_scaling_tbl$year %in% forecast_years]))
    if(length(missing_iso3s_gdp) > 0) {
      warning(sprintf('Missing GDP information for %d iso3(s): %s. Using population-only scaling for those.',
                      length(missing_iso3s_gdp), paste(missing_iso3s_gdp, collapse = ', ')))
    }
  }

  # join without year to expand across years, safer than many-to-many full_join
  base_cols <- setdiff(names(hia_cost), 'year')
  hia_by_year <- hia_cost %>%
    sel(all_of(base_cols)) %>%
    inner_join(scaling, by = c('iso3', 'AgeGrp'='age_group','fatal'))

  # duplication guard: after join, each original row should be replicated exactly length(ref+forecast) times
  expected_mult <- length(unique(c(reference_year, forecast_years)))
  dup_check <- hia_by_year %>%
    group_by(across(all_of(base_cols))) %>%
    summarise(n_years = n_distinct(year), .groups='drop')
  if(any(dup_check$n_years != expected_mult)) {
    stop('Unexpected expansion in join: some rows did not map to all requested years. Check iso3/AgeGrp/fatal coverage.')
  }

  # scale metrics
  out <- hia_by_year %>%
    mutate(
      number = number * pop_scaling,
      cost_mn_currentUSD = cost_mn_currentUSD * pop_scaling * gdp_scaling
    )
  if('cost_mn_currentLCU' %in% names(out)) {
    out <- out %>% mutate(cost_mn_currentLCU = cost_mn_currentLCU * pop_scaling * gdp_scaling)
  }

  # recompute share_gdp if possible; prefer matching currency
  recompute_share <- function(df) {
    # detect baseline GDP total columns
    use_lcu <- 'GDP.TOT.currLCU' %in% names(df)
    use_usd <- 'GDP.TOT.currUSD' %in% names(df)
    if(use_lcu) {
      df <- df %>% mutate(GDP_total_scaled = GDP.TOT.currLCU * pop_scaling * gdp_pc_scaling,
                          share_gdp = (cost_mn_currentLCU * 1e6) / GDP_total_scaled)
    } else if(use_usd) {
      df <- df %>% mutate(GDP_total_scaled = GDP.TOT.currUSD * pop_scaling * gdp_pc_scaling,
                          share_gdp = (cost_mn_currentUSD * 1e6) / GDP_total_scaled)
    } else if('share_gdp' %in% names(df)) {
      # cannot recompute reliably; drop stale column
      df <- df %>% sel(-share_gdp)
    }
    df %>% sel(-any_of('GDP_total_scaled'))
  }

  out <- recompute_share(out)

  out
}

#' Forecast HIA counts and costs across years using population and GDP scaling
#'
#' Scales HIA results from a reference year to the requested forecast years.
#' Health outcomes are scaled by age-group specific population factors. Fatal
#' outcomes (Deaths, YLLs) use death scaling, while non-fatal outcomes use
#' population scaling. When `use_gdp_scaling = TRUE`, monetary costs are also
#' scaled by per-capita GDP PPP growth and discounted back to the reference year.
#'
#' @param hia_cost A data.frame or a list with element `hia_cost` containing the
#'   HIA results for the reference year. Required columns: `iso3`, `year`,
#'   `AgeGrp`, `Outcome`, `number`, `cost_mn_currentUSD`. Optional columns such
#'   as `cost_mn_currentLCU`, `GDP.TOT.currLCU`, `GDP.TOT.currUSD` are used when
#'   present to recompute `share_gdp`.
#' @param forecast_years Integer vector of target years to produce.
#' @param reference_year Integer. Base year for scaling (default 2019). The
#'   output includes this year as well as `forecast_years`.
#' @param use_gdp_scaling Logical. If TRUE, apply GDP per-capita PPP growth and
#'   discount to `reference_year` when scaling costs (default FALSE).
#' @param discount_rate Numeric. Annual discount rate used when
#'   `use_gdp_scaling = TRUE` (default 0.03).
#'
#' @return A data.frame with the input rows expanded across years and the
#'   following additional columns:
#'   - `year`: reference and forecast years
#'   - `fatal`: outcome fatality flag
#'   - `pop_scaling`: age-group/cause-specific population scaling factor
#'   - `gdp_pc_scaling`: per-capita GDP PPP index relative to reference year
#'   - `gdp_scaling`: `gdp_pc_scaling` discounted to the reference year
#'   - `number`: scaled health outcome counts
#'   - `cost_mn_currentUSD`: scaled costs in million USD
#'   - `cost_mn_currentLCU`: scaled costs in million LCU (if present in input)
#'   - `share_gdp`: recomputed if GDP totals are provided; removed otherwise
#'
#' @details
#' - Fatal outcomes are identified as those with `Outcome` matching `"YLLs"` or
#'   `"Deaths"`. Outcomes with `"YLDs"` are treated as non-fatal.
#' - Age group synthesis for groups like `"25+"`, `"0-18"`, etc., uses heuristic
#'   multipliers over UN WPP bins; these multipliers are a pragmatic approximation
#'   and may be revised.
#' - The join expands each input row to all requested years using
#'   `iso3`/`AgeGrp`/`fatal` as keys. A duplication guard verifies that each base
#'   row expands to exactly the number of requested years and stops otherwise.
#' - Missing population projection years or GDP data for some `iso3`s trigger
#'   informative warnings; population-only scaling is used where GDP data are
#'   missing.
#'
#' @examples
#' # Minimal example
#' hia_cost <- data.frame(
#'   iso3 = "USA", region_id = "USA", Outcome = c("Deaths","Asthma.Prev"),
#'   year = 2019, number = c(100, 1000), cost_mn_currentUSD = c(1.2, 0.3),
#'   AgeGrp = "25+", double_counted = FALSE
#' )
#'
#' # Population-only scaling to 2020 and 2023
#' fut <- get_econ_forecast(hia_cost,
#'                          forecast_years = c(2020, 2023),
#'                          reference_year = 2019,
#'                          use_gdp_scaling = FALSE)
#'
#' @export
get_econ_forecast <- function(hia_cost,
                              forecast_years,
                              reference_year = 2019,
                              use_gdp_scaling = FALSE,
                              discount_rate = 0.03) {

  # accept list(hia_cost=...) or data.frame
  if(!is.data.frame(hia_cost)) {
    hia_cost <- hia_cost$hia_cost
  }

  # basic input validation
  required_cols <- c('iso3','year', 'AgeGrp','Outcome','number','cost_mn_currentUSD')
  missing_cols <- setdiff(required_cols, names(hia_cost))
  if(length(missing_cols) > 0) stop(sprintf('hia_cost is missing required columns: %s', paste(missing_cols, collapse = ', ')))
  if(length(forecast_years) < 1) stop('forecast_years must contain at least one year')

  # compute scaling tables
  pop_scaling_info <- compute_population_scaling(hia_cost, reference_year, forecast_years)
  pop_scaling <- pop_scaling_info$pop_scaling

  gdp_tbl <- NULL
  if(isTRUE(use_gdp_scaling)) {
    gdp_tbl <- compute_gdp_scaling(hia_cost, reference_year, forecast_years, discount_rate)
  }

  # apply scaling and return
  apply_econ_scaling(hia_cost, pop_scaling, gdp_tbl, reference_year, forecast_years)
}
