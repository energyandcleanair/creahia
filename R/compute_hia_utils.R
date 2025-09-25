get_nrt_conc <- function(region_ids, conc_name, nrt, conc_map,
                         units_multiplier = 1, nrt_flag = NULL, nrt_flag_value = 2) {
  conc_map[region_ids] %>%
    lapply(function(m) {
      if(!is.null(nrt_flag)) nrt = ifelse(m[,nrt_flag] == nrt_flag_value, 0, nrt)

      m[, conc_name] %>%
        multiply_by(units_multiplier) %>%
        subtract(nrt) %>%
        pmax(0) %>%
        weighted.mean(w = m[, 'pop'], na.rm = T)
    }) %>% unlist %>% unname
}


scale_hia_pop <- function(hia, base_year = 2015, target_year = 2019) {

  message(glue("Scaling population from {base_year} to {target_year}"))
  pop_proj <- get_pop_proj()

  # scale population from year of population data to target year of estimates
  pop_scaling <- pop_proj %>%
    filter(year %in% c(base_year, target_year),
           !is.na(iso3)) %>%
    group_by(iso3, year) %>%
    summarise_at('pop', sum) %>%
    mutate(year = case_when(year == base_year ~ 'base',
                            year == target_year ~ 'target'))

  hia_scaled <- pop_scaling %>%
    spread(year, pop) %>%
    mutate(scaling=target/base) %>%
    sel(iso3, scaling) %>%
    left_join(hia, .) %>%
    (function(df) df %>% mutate_if(is.numeric, multiply_by, df$scaling)) %>%
    sel(-scaling)

  return(hia_scaled)
}


totalise_hia <- function(hia, .groups = c("scenario", "iso3", "region_id", "region_name")) {

  hia_adm <- hia %>%
    group_by(across(c(scenario, estimate, all_of(.groups)))) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    pivot_longer(is.numeric, names_to = 'Outcome', values_to = 'Number')

  hia_total <- hia_adm %>%
    group_by(across(c(all_of(.groups), scenario, estimate, Outcome))) %>%
    summarise_if(is.numeric, sum, na.rm = T) %>%
    spread(estimate, Number)

  # Add cause long name & unit
  hia_total$pollutant <- lapply(str_split(hia_total$Outcome, "_"),
                                function(x) {tail(x, 1)}) %>% unlist
  hia_total$unit <- lapply(str_split(hia_total$Outcome, "_"),
                           function(x) {ifelse(length(x) == 3, x[2], "")}) %>% unlist
  hia_total$cause <- lapply(str_split(hia_total$Outcome, "_"),
                            function(x) {x[1]}) %>% unlist

  hia_total %>% left_join(get_dict() %>% rename(cause = Code, cause_name = Long.name)) %>%
    dplyr::select(region_id, region_name, iso3, scenario, cause, cause_name,
                  unit, pollutant, central, high, low) %>%
    arrange(region_name, cause)
}


#' Summarise hia results
#'
#' @param hia_total
#' @param make_ci_fun
#' @param res_cols
#' @param dict
#'
#' @return
#' @export
#'
#' @examples
make_hia_table <- function(hia_total,
                           make_ci_fun = make_nothing,
                           res_cols = c('low', 'central', 'high'),
                           dict = get_dict()) {

  if('Cause' %notin% names(hia_total)) {
    hia_total <- hia_total %>% separate(Outcome, c('Cause', 'Outcome', 'Pollutant'), '_')
    ind <- is.na(hia_total$Pollutant)
    hia_total$Pollutant[ind] <- hia_total$Outcome[ind]
    hia_total$Outcome[ind] <- hia_total$Cause[ind]
  }

  if('Outcome_long' %notin% names(hia_total)) {
    hia_table <- hia_total %>% add_long_names(dict = dict) %>%
      sel(scenario, Cause_long, Outcome_long, Pollutant, all_of(res_cols))
  }

  deaths <- hia_table %>% filter(Outcome_long == 'deaths')
  morb <- hia_table %>% filter(!grepl('deaths|life lost|prev|birthwe', Outcome_long),
                               !is.na(Outcome_long))

  deaths <- deaths %>% filter(Outcome_long == 'deaths') %>%
    filter(!(Cause_long == 'all' & Pollutant == 'PM25'))

  bind_rows(deaths %>% make_ci_fun %>% arrange(desc(Pollutant)),
            morb %>% make_ci_fun %>% arrange(Outcome_long)) %>%
    sel(Outcome_long, Cause_long, everything()) %>%
    mutate(Cause_long = recode(Cause_long, deaths = 'total'))
}


# a simple renaming function that accepts string variables as arguments; !!newname := !!oldname was causing grief
rename_str <- function(df, oldname, newname) { names(df)[names(df) == oldname] <- newname; df }


add_long_names <- function(df, cols = c('Outcome', 'Cause'), dict = get_dict()) {
  for(cn in intersect(names(df), cols)) {
    out_cn <- paste0(cn, '_long')
    df <- df %>%
      left_join(dict %>% rename_str('Code', cn)) %>%
      rename_str('Long.name', out_cn)
    df[[out_cn]] <- df[[out_cn]] %>% na.cover(df[[cn]])
  }

  if('Cause_long' %in% names(df)) df$Cause_long[grep('non-comm', df$Cause_long)] <- 'all'

  if('Pollutant' %in% names(df)) df$Pollutant <- df$Pollutant %>% recode(PM25 = "PM2.5")
  return(df)
}


hiapoll_species_corr <- function() {
  list(
    "PM25" = "pm25",
    "NO2" = "no2",
    "O3_8h" = "o3_8h",
    "SO2" = "so2",
    'PM10' = 'tpm10')
}


species_to_hiapoll <- function(species) {
  corr <- hiapoll_species_corr()
  names(corr)[which(corr %in% species)]
}


hiapoll_to_species <- function(hiapoll) {
  corr <- hiapoll_species_corr()
  corr[hiapoll] %>% unlist() %>% as.vector()
}


#' Key function to add a double_counted field to hia results
#'
#' @param hia
#' @param crfs
#' @param epi
#'
#' @return
#' @export
#'
#' @examples
add_double_counted <- function(hia, crfs, epi) {

  # Use CRFS double counted field first
  joined <- hia %>%
    left_join(crfs %>%
                # mutate(Cause = crf_incidence_to_cause(Incidence),
                #        Outcome = crf_effectname_to_outcome(effectname),
                #        Pollutant = Exposure) %>%
                select(cause, outcome, pollutant, double_counted),
              by = c('cause', 'outcome', 'pollutant'))

  # Except PM25, all of them should have been found in CRFs
  if(nrow(joined %>% filter(is.na(double_counted) & pollutant != 'PM25' & number > 0)) > 0) {
    stop('merged has failed in double counting detection')
  }

  # Manual for epi PM25
  joined[joined$pollutant == 'PM25' &
           any(joined$cause == CAUSE_NCDLRI) & # detect if NCD+LRI is being used (e.g. from GEMM or FUSION)
           joined$cause %in% CAUSE_NCDLRI_INCLUDED &
           joined$outcome %in% c(MEASURE_YLLS, MEASURE_DEATHS),
         'double_counted'] <- TRUE

  joined[joined$pollutant == 'PM25' &
           any(joined$cause == CAUSE_CV) & # detect if NCD+LRI is being used (e.g. from GEMM or FUSION)
           joined$cause %in% CAUSE_CV_INCLUDED &
           joined$outcome %in% c(MEASURE_YLLS, MEASURE_DEATHS),
         'double_counted'] <- TRUE

  joined <- joined %>%
    mutate(double_counted = tidyr::replace_na(double_counted, FALSE))

  return(joined)
}


add_age_group <- function(hia) {
  hia <- hia %>%
    mutate(age_group = case_when(grepl("LRI\\.child", cause) ~ "0-4",
                                 grepl("PTB|LBW", cause) ~ "Newborn",
                                 grepl("0to17|1to18", cause) ~ "0-18",
                                 T ~ "25+"))
  return(hia)
}


clean_cause_outcome <- function(hia) {

  # Clean asthma
  # hia$cause[grep('exac|sthma', hia$cause)] <- 'Asthma'

  # Valuation is now different between deaths and deaths.child
  hia$outcome[grepl("LRI\\.child", hia$cause) & (hia$outcome == 'Deaths')] <- 'Deaths.child'
  return(hia)
}

add_total_deaths_and_costs <- function(df) {
  df %<>% filter(!double_counted, !grepl("economic costs", Outcome)) %>%
    summarise(across(c(number=cost_mn_currentUSD), sum), .groups = 'keep') %>%
    mutate(Outcome = "economic costs", unit="million USD", Cause='AllCause', Pollutant='All', double_counted = F) %>%
    bind_rows(df %>% filter(!grepl("economic costs", Outcome)))

  df %<>% filter(!double_counted, grepl("Death", Outcome)) %>%
    summarise(across(c(number), sum), .groups = 'keep') %>%
    mutate(Outcome = "deaths, total", Cause='AllCause', Pollutant='All', unit="death", double_counted = T) %>%
    bind_rows(df %>% filter(!grepl("deaths, total", Outcome)))

  return(df)
}

#' Convert wide HIA data to long format
#'
#' @param hia Health impact assessment data in wide format
#' @return HIA data in long format with proper Outcome, Pollutant, and Cause columns
#' @export
to_long_hia <- function(hia) {
  # Get numeric columns to pivot (exclude character, factor, and pop columns)
  numeric_cols <- names(hia)[sapply(hia, is.numeric)]
  cols_to_pivot <- numeric_cols[numeric_cols != "pop"]

  # If no columns to pivot, return the data as-is with default columns
  # if(length(cols_to_pivot) == 0) {
  #   return(hia %>%
  #          mutate(Outcome = NA_character_,
  #                 Pollutant = NA_character_,
  #                 Cause = NA_character_,
  #                 number = NA_real_))
  # }

  hia %>%
    pivot_longer(all_of(cols_to_pivot),
                 names_to = 'cause_outcome',
                 values_to = 'number') %>%
    mutate(cause = stringr::word(cause_outcome, 1, sep = "_"),
           outcome = stringr::word(cause_outcome, 2, sep = "_")) %>%
    sel(-cause_outcome)
}

#' Safe World Bank data retrieval with retry logic and longer timeout
#'
#' Wraps \code{wbstats::wb_data()} with retry logic and configurable timeout
#' to handle network issues and API timeouts more robustly.
#'
#' @param ... Arguments passed to \code{wbstats::wb_data()}
#' @param max_retries Number of retry attempts (default: 3)
#' @param timeout_seconds Timeout for each request in seconds (default: 60)
#' @param retry_delay Delay between retries in seconds (default: 2)
#' @return Data frame returned by \code{wbstats::wb_data()}
#' @export
safe_wb_data <- function(..., max_retries = 3, timeout_seconds = 60, retry_delay = 2) {
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      # Set longer timeout for httr requests using httr::timeout()
      old_config <- httr::config()
      httr::set_config(httr::timeout(timeout_seconds))
      on.exit(httr::set_config(old_config), add = TRUE)

      result <- wbstats::wb_data(...)
      return(result)
    }, error = function(e) {
      if (attempt == max_retries) {
        stop("Failed to fetch World Bank data after ", max_retries, " attempts. Last error: ", e$message)
      }
      message("Attempt ", attempt, " failed, retrying in ", retry_delay, " seconds...")
      Sys.sleep(retry_delay)
    })
  }
}
