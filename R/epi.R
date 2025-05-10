# A series of functions to generate epi and ihme data from GDB and other sources


download_raw_epi <- function(version, dataset) {
  # This does not download data for now... but gives you the links to do so
  urls <- list(
    gbd2017 = list(
      raw = "https://gbd2017.healthdata.org/gbd-results?params=gbd-api-2017-permalink/d3401177db525d7f161784fe803019ef",
      asthma = "https://gbd2017.healthdata.org/gbd-results?params=gbd-api-2017-permalink/ad187e50445a73d6227f8dc8df6e8430"
    ),
    gbd2019 = list(
      raw = "https://gbd2019.healthdata.org/gbd-results?params=gbd-api-2019-permalink/ab58faccff223793b4ab74c53c1760d5",
      asthma = "https://gbd2019.healthdata.org/gbd-results?params=gbd-api-2019-permalink/81716ae90083a4c6e7f88ac16a0d0094"
    ),
    # For now, these are with pop from 2019 to ensure the scaling is alright. However, it would be much easier to
    # clean it, and download with rates instead
    gbd2021 = list(
      raw = "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/951bb3a80e3e0c542986a1be0f32efec",
      asthma = "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/428758edc6d75bebf16eb96beee09adc"
    )
  )

  url <- urls[[version]][[dataset]]

  message(glue("Download {dataset} csv files from {url}"))
  message("You will first need to create an account. Then paste the csv files into the data/epi_update folder.")
  stop("Missing data")
}


get_gbd_raw <- function(version) {
  filepaths <- list(
    gbd2017 = list.files("data/epi_update", pattern = "IHME-GBD_2017_DATA-\\d.csv", full.names = T),
    gbd2019 = list.files("data/epi_update", pattern = "IHME-GBD_2019_DATA-\\d.csv", full.names = T),
    gbd2021 = list.files("data/epi_update", pattern = "IHME-GBD_2021_DATA-\\d.csv", full.names = T)
  )[[version]]

  if(!all(file.exists(filepaths))){
    download_raw_epi(version, "raw")
  }

  filepaths %>%
  lapply(function(f) read_csv(f, col_types = cols())) %>%
    bind_rows()
}


get_gbd_asthma_raw <- function(version) {
  list(
    gbd2017 = c("data/epi_update/IHME-GBD_2017_DATA-asthma.csv"),
    gbd2019 = c("data/epi_update/IHME-GBD_2019_DATA-asthma.csv"),
    gbd2021 = c("data/epi_update/IHME-GBD_2021_DATA-asthma.csv")
  )[[version]] %>%
    lapply(read_csv) %>%
    bind_rows()
}


#' Convert a region id to the corresponding epi location_id
#' Could be national or subnational
#'
#'
#' @param region_id
#'
#' @return
#' @export
#'
#' @examples
get_epi_location_id <- function(region_id) {
  locations_w_gadm <- get_locations_with_gadm(use_cache = T)

  # We try first with region_id==gadm_id
  # If not match, it uses iso3
  location_id <- tibble(
    region_id,
    iso3 = substr(region_id, 1, 3) %>% country.recode(c(use_as_proxy, merge_into))
  ) %>%
    left_join(
      locations_w_gadm %>% sel(location_id, gadm_id),
      by = c("region_id" = "gadm_id")
    ) %>%
    left_join(
      locations_w_gadm %>% sel(location_id_iso3 = location_id, gadm_id),
      by = c("iso3" = "gadm_id")
    ) %>%
    mutate(location_id = coalesce(
      location_id, location_id_iso3
    )) %>%
    pull(location_id)

  if (length(location_id) != length(region_id)) stop(glue("Failed to get epi location_id for region: {region_id}"))

  return(location_id)
}


get_locations <- function() {
  raw <- read_xlsx(get_hia_path("IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2022M06D29.XLSX"), .name_repair = make.names) %>%
    sel(
      location_id = matches("location.id", ignore.case = T),
      level = matches("^level$", ignore.case = T),
      location_name = matches("location.nam", ignore.case = T),
      parent_id = matches("Parent.ID", ignore.case = T)
    )

  # Add country_id to each of these locations
  country_mapping <- list()
  for (level in seq(3, 6)) {
    raw_level <- raw %>% filter(level == !!level)
    if (level == 3) {
      country_mapping[[level]] <- raw_level %>%
        distinct(location_id, country_id = location_id, country_name = location_name)
    } else {
      country_mapping[[level]] <- raw_level %>%
        distinct(location_id, parent_id) %>%
        left_join(country_mapping[[level - 1]],
          by = c("parent_id" = "location_id")
        ) %>%
        sel(location_id, country_id, country_name)
    }
  }
  country_mapping %<>% bind_rows()


  raw %>%
    left_join(country_mapping) %>%
    sel(location_id, location_name, location_level = level, country_id, country_name) %>%
    mutate(iso3 = countrycode::countrycode(country_name, "country.name", "iso3c"))
}


attach_gadm_to_locations <- function(locations = get_locations()) {
  matching_files <- get_hia_paths(pattern = "*.csv", path = "location_matching")
  matching_subnational <- lapply(matching_files, read_csv, col_types = cols()) %>%
    bind_rows() %>%
    sel(iso3, ihme_level, ihme_location_name, gadm_level, gadm_id, gadm_name)

  # Create a country matching
  matching_countries <- creahelpers::get_adm(level = 0, res = "coarse") %>%
    as.data.frame() %>%
    sel(
      iso3 = GID_0,
      gadm_id = GID_0,
      gadm_name = COUNTRY
    ) %>%
    mutate(gadm_level = 0) %>%
    right_join(locations %>%
      filter(location_level == 3) %>%
      distinct(iso3,
        ihme_location_name = location_name,
        ihme_level = location_level
      ))

  matching <- bind_rows(
    matching_countries,
    matching_subnational
  )

  locations %>%
    left_join(matching,
      by = c(
        "iso3" = "iso3",
        "location_level" = "ihme_level",
        "location_name" = "ihme_location_name"
      )
    )
}

get_locations_with_gadm <- function(use_cache = T) {
  dir.create("cache", showWarnings = F)
  filepath <- "cache/locations_with_gadm.RDS"
  if (file.exists(filepath) & use_cache) {
    return(readRDS(filepath))
  }

  locations_w_gadm <- get_locations() %>%
    attach_gadm_to_locations()
  saveRDS(locations_w_gadm, filepath)
  return(locations_w_gadm)
}


get_epi_pop <- function(version="gbd2019", level = c(3, 4)) {

  locations <- get_locations()

  filepath <- list(
    "gbd2019" = "IHME_GBD_2019_POP_2019_Y2020M10D15.CSV"

  )

  # CHECK Should we use 2019 pop even with 2017 data?
  pop <- read_csv(get_hia_path("IHME_GBD_2019_POP_2019_Y2020M10D15.CSV")) %>% # read.csv('2017 data/IHME_GBD_2017_POP_2015_2017.CSV') %>%
    dplyr::filter(year_id == 2019, tolower(sex_name) == "both") %>%
    dplyr::rename(country = location_name)

  locations %>%
    right_join(pop) %>%
    filter(location_level %in% !!level) %>%
    distinct()
}


get_pop_total <- function(pop) {
  pop %>% filter(age_group_name == "All Ages")
}


get_wb_ind <- function() {
  read_csv("data/epi_update/World Bank indicators.csv")
}


get_asthma_new <- function() {
  read.csv("data/epi_update/new asthma cases.csv") %>%
    sel(country, starts_with("X2ppb")) %>%
    gather(estimate, val, -country) %>%
    mutate(estimate = estimate %>% gsub(".*_", "", .)) %>%
    mutate(
      var = "new.asthma_NO2",
      val = val * 1e3
    )
}


get_asthma_erv <- function(pop.total = NULL) {
  asthma.erv <- read_xlsx("data/epi_update/Anenberg EHP 2018 results.xlsx", sheet = "pm totconc") %>%
    filter(!is.na(ID)) %>%
    dplyr::rename(
      country = Country,
      exac.0to17 = exac_0_17_base,
      exac.18to99 = exac_18_99_base,
      exac.0to99 = exac_0_99_base
    )


  # add Serbia and Montenegro data to asthma ERV
  pop.total <- creahelpers::default_if_null(pop.total, get_pop_total())
  pop.total %<>% addiso()
  pop.total %>%
    filter(iso3 %in% c("SRB", "MNE")) %>%
    distinct() -> sm
  sm$val <- sm$val / sum(sm$val)
  asthma.erv %>%
    filter(country == "Serbia and Montenegro") %>%
    "["(c(1, 1), T) -> sm.erv
  sm.erv %<>% mutate_if(is.numeric, multiply_by, sm$val)
  sm.erv$country <- sm$country
  sm.erv$ID <- NA

  asthma.erv$country[asthma.erv$ID == 310] <- "Democratic Republic of the Congo"
  asthma.erv$country[asthma.erv$ID == 311] <- "Congo - Brazzaville"
  asthma.erv %<>% bind_rows(sm.erv) %>% sel(-ID)

  # Scale to population
  asthma.erv.scaled <- asthma.erv %>%
    mutate(iso3 = countrycode::countrycode(country, "country.name", "iso3c")) %>%
    sel(country, iso3, starts_with("exac")) %>%
    left_join(pop.total %>% filter(location_level == 3) %>% sel(iso3, pop = val))

  asthma.erv.scaled$exac.0to17 %<>% divide_by(asthma.erv.scaled$pop) %>% multiply_by(1e5)
  asthma.erv.scaled$exac.18to99 %<>% divide_by(asthma.erv.scaled$pop) %>% multiply_by(1e5)
  asthma.erv.scaled$exac.0to99 %<>% divide_by(asthma.erv.scaled$pop) %>% multiply_by(1e5)

  asthma.erv.scaled %>%
    sel(country, iso3, starts_with("exac")) %>%
    gather(var, val, -c(country, iso3))
}


get_ptb <- function() {
  PTB <- readxl::read_xlsx("data/epi_update/2014 National preterm birth rates.xlsx", skip = 3, col_names = F, n_max = 300)
  nms <- readxl::read_xlsx("data/epi_update/2014 National preterm birth rates.xlsx", skip = 1, col_names = F, n_max = 2)
  nms[2, ] <- ifelse(is.na(nms[2, ]), nms[1, ], nms[2, ])
  names(PTB) <- unlist(nms[2, ]) %>% make.names(unique = T)
  PTB %<>% filter(!is.na(PTB.rate)) %>% dplyr::rename(country = Country)
  PTB %<>% dplyr::rename(val = PTB.rate) %>% mutate(var = "PTB.rate")
  return(PTB)
}

get_death_all_cause <- function(pop.total, version = "gbd2019") {
  get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(
      (cause_name %in% c(
        "Non-communicable diseases",
        "Lower respiratory infections"
      )),
      metric_name == "Number"
    ) %>%
    mutate(age_low = get_age_low(age_name)) %>%
    filter(!is.na(age_low), age_low >= 30) %>%
    filter(location_level %in% c(3, 4)) %>%
    gather_ihme() %>%
    group_by(location_id, location_name, location_level, measure_name, metric_name, estimate) %>%
    summarise_at("val", sum) %>%
    ihme_getrate(pop.total = pop.total) %>%
    mutate(var = paste0("NCD.LRI_", measure_name))
}


get_death_crude <- function(version = "gbd2019") {
  get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(age_name == "All ages", cause_name == "All causes", metric_name == "Rate", measure_name == "Deaths") %>%
    gather_ihme() %>%
    mutate(var = "crude.death.rate")
}


get_death_child_lri <- function(pop.total, version = "gbd2019") {
  get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(
      cause_name == "Lower respiratory infections",
      metric_name == "Number",
      age_name %in% c("Under 5", "<5 years")
    ) %>%
    gather_ihme() %>%
    ihme_getrate(pop.total = pop.total) %>%
    filter(measure_name %in% c("Deaths", "YLLs")) %>%
    mutate(var = paste0("LRI.child_", measure_name))
}


recode_gbd_cause <- function(cause_name){

  new_cause_names <- case_when(

    # General
    cause_name == "All causes" ~ "AllCause",
    cause_name == "Lower respiratory infections" ~ CAUSE_LRI,
    cause_name == "Tracheal, bronchus, and lung cancer" ~ CAUSE_LUNGCANCER,
    cause_name == "Diabetes mellitus type 2" ~ CAUSE_DIABETES,
    cause_name == "Alzheimer's disease and other dementias" ~ CAUSE_DEMENTIA,

    # Cardiovascular diseases
    cause_name == "Ischemic heart disease" ~ CAUSE_IHD,
    cause_name == "Stroke" ~ CAUSE_STROKE,
    cause_name == "Cardiovascular diseases" ~ "TotCV",

    cause_name %in% c("Rheumatic heart disease",
                      "Hypertensive heart disease",
                      "Cardiomyopathy and myocarditis",
                      "Atrial fibrillation and flutter",
                      "Aortic aneurysm",
                      "Lower extremity peripheral arterial disease",
                      "Endocarditis",
                      "Non-rheumatic valvular heart disease",
                      "Peripheral artery disease",
                      "Other cardiovascular and circulatory diseases",
                      "Pulmonary Arterial Hypertension"
                      ) ~ "OthCV",

    # Respiratory diseases
    cause_name == "Chronic obstructive pulmonary disease" ~ CAUSE_COPD,
    cause_name == "Chronic respiratory diseases" ~ "TotResp",
    cause_name %in% c("Pneumoconiosis",
                      "Asthma",
                      "Interstitial lung disease and pulmonary sarcoidosis",
                      "Other chronic respiratory diseases") ~ "OthResp",

    TRUE ~ NA_character_
  )

  # Print those that weren't matched
  if(any(is.na(new_cause_names))) {
    warning(glue("Some causes were not matched:", paste0(unique(cause_name[is.na(new_cause_names)]), collapse=", ")))
  }

  return(new_cause_names)
}


get_yld <- function(pop.total, version = "gbd2019") {

  if(version == "gbd2017"){
    return(get_yld_gbd2017(pop.total=pop.total, version=version))
  }

  get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(
      metric_name == "Number",
      grepl("YLD|Death|YLL", measure_name)
    ) %>%
    filter(age_name=="25+ years") %>%
    gather_ihme() %>%
    ihme_getrate(pop.total = pop.total) %>%
    mutate(cause_name = recode_gbd_cause(cause_name)) %>%
    filter(!is.na(cause_name)) %>%
    filter(!is.na(val)) %>%
    group_by(location_id, location_name, location_level, iso3, cause_name, metric_name, measure_name, year) %>%
    group_modify(function(df, group){

      # If there's only one original cause (i.e. 3 estimates) return it
      if(nrow(df) == 3) return(df)

      df_sum <- df %>%
        spread(estimate, val) %>%
        group_by() %>%
        dplyr::summarise(
          central = sum(central, na.rm = TRUE),
          sigma = sqrt(sum(((high - low) / (2 * 1.96))^2))
        )

      low <- df_sum$central - 1.96 * df_sum$sigma
      high <- df_sum$central + 1.96 * df_sum$sigma

      # Ensure that the lower bound is not negative
      if(low < 0) {
        warning("Negative lower bound. Setting to 0.")
        low <- 0
      }

      # Create a new data frame with aggregated estimates
      data.frame(
        estimate = c("central", "low", "high"),
        val = c(df_sum$central, low, high)
      )
    }) %>%
    filter(!is.na(cause_name)) %>%
    mutate(var = paste0(cause_name, "_", measure_name))
}


#' Because we didn't download all respiratory and cardiovascular diseases,
#' we use another approach to determine OthCV and OthResp, by substracting
#' those we have from the total
#'
#' @return
#' @export
#'
#' @examples
get_yld_gbd2017 <- function(pop.total, version){

  yld <- get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(
      metric_name == "Number",
      grepl("YLD|Death|YLL", measure_name)
    ) %>%
    mutate(age_low = get_age_low(age_name)) %>%
    filter(!is.na(age_low), age_low >= 25) %>%
    gather_ihme() %>%
    group_by(location_id, location_name, location_level, iso3, cause_name, metric_name, measure_name, estimate, year) %>%
    summarise_at("val", sum) %>%
    ihme_getrate(pop.total = pop.total) %>%
    mutate(cause_name = recode(cause_name,
                               "Ischemic heart disease" = "IHD",
                               "Lower respiratory infections" = "LRI",
                               "Tracheal, bronchus, and lung cancer" = "LC",
                               "Chronic obstructive pulmonary disease" = "COPD",
                               "Diabetes mellitus type 2" = "Diabetes",
                               "All causes" = "AllCause",
                               "Stroke" = "Stroke",

                               # Add totals to deduct from
                               "Cardiovascular diseases" = "TotCV",
                               "Chronic respiratory diseases" = "TotResp",

                               .default = NA_character_
    )) %>%
    filter(!is.na(cause_name)) %>%
    mutate(var = paste0(cause_name, "_", measure_name))



  # Compute others
  other_cv <- compute_others(yld, "TotCV", "IHD|Stroke", "OthCV")
  other_resp <- compute_others(yld, "TotResp", "COPD", "OthResp")


  yld %>%
    # And add the others
    bind_rows(other_cv) %>%
    bind_rows(other_resp)
}

compute_others <- function(yld, total, grep, newname, metric_name="Number") {

  yld %>%
    filter(cause_name == total | grepl(grep, cause_name)) %>%
    filter(metric_name == !!metric_name) %>%
    group_by(measure_name, location_id, location_name, location_level, iso3, metric_name, year) %>%
    dplyr::group_modify(function(df, group){

      # Here we assume:
      # 1- independence between causes
      # 2- that the Total variance was computed by simply summing variances
      # Extract estimates for Total deaths

      total_estimates <- df %>%
        filter(cause_name == total) %>%
        select(estimate, val) %>%
        spread(estimate, val)

      # Extract estimates for causes of interest
      causes_estimates <- df %>%
        filter(cause_name != total) %>%  # Exclude total
        group_by(cause_name) %>%
        select(cause_name, estimate, val) %>%
        spread(estimate, val)

      # Ensure that estimates are available
      if (nrow(total_estimates) == 0 || nrow(causes_estimates) == 0) {
        stop("Estimates for Total or Causes of Interest are missing.")
      }

      # Calculate standard deviation for Total deaths
      total_sd <- (total_estimates$high - total_estimates$low) / (2 * 1.96)
      var_T <- total_sd^2

      # Initialize sums for causes of interest
      sum_values_C <- 0
      sum_vars_C <- 0

      # Loop through each cause of interest to calculate variances and sum values
      for (i in 1:nrow(causes_estimates)) {
        central <- causes_estimates$central[i]
        low <- causes_estimates$low[i]
        high <- causes_estimates$high[i]

        # Calculate standard deviation
        sd_Ci <- (high - low) / (2 * 1.96)
        var_Ci <- sd_Ci^2

        # Sum central values and variances
        sum_values_C <- sum_values_C + central
        sum_vars_C <- sum_vars_C + var_Ci
      }

      # Reverse engineer variance of Other Deaths
      var_O <- var_T - sum_vars_C

      if(is.na(var_O)) {
        warning(glue("NA sum for group {group$location_name} {group$measure_name} {group$metric_name}"))
        return(tibble())
      }

      # Check for negative variance
      if (var_O < 0) {
        warning("Calculated variance for Other Deaths is negative. This may indicate that the assumption does not hold.")
        var_O <- 0
      }

      # Standard deviation of Other Deaths
      sigma_O <- sqrt(var_O)

      # Central Value of Other Deaths
      value_O <- total_estimates$central - sum_values_C

      # Confidence Interval for Other Deaths
      lower_O <- value_O - 1.96 * sigma_O
      upper_O <- value_O + 1.96 * sigma_O

      # Ensure non-negative lower bound
      if (lower_O < 0){
        warning(glue("{group$location_name} {group$measure_name} {group$metric_name} is negative. Setting it to 0"))
        lower_O <- 0
      }

      # Create data frame with results for Other Deaths
      other_df <- data.frame(
        cause_name = newname,
        var = paste0(newname, "_", unique(group$measure_name)),
        estimate = c("central", "low", "high"),
        val = c(value_O, lower_O, upper_O)
      )

      return(other_df)
    })
}

get_asthma_prev <- function(pop.total, version = "gbd2019") {

  asthma_raw_data <- get_gbd_asthma_raw(version = version) %>%
    mutate(age_low = get_age_low(age_name))

  asthma.prev <- asthma_raw_data %>% # read.csv('2017 data/IHME-GBD_2017_Asthma.csv') %>%
    add_location_details() %>%
    filter(
      measure_name %in% c("Incidence", "Prevalence"),
      metric_name == "Rate"
    ) %>%
    gather_ihme() %>%
    filter(tolower(age_name) != "all ages") %>%
    filter(age_low %in% c(0, 1, 5, 10, 15)) %>% # Some GBD versions have 1-4, others <5 (<1 year is 0), hence the 0 and 1
    group_by(location_id, location_name, location_level, iso3, year, measure_name, estimate) %>%
    summarise_at("val", mean) %>%
    mutate(var = paste0("Asthma.", substr(measure_name, 1, 3), ".1to18nopopnorm")) %>%
    ungroup() %>%
    distinct()

  asthma.prev <- asthma_raw_data %>%
    add_location_details() %>%
    filter(
      measure_name %in% c("Incidence", "Prevalence"),
      metric_name == "Number"
    ) %>%
    gather_ihme() %>%
    filter(tolower(age_name) != "all ages") %>%
    filter(age_low %in% c(0, 1, 5, 10, 15)) %>% # Some GBD versions have 1-4, others <5 (<1 year is 0), hence the 0 and 1
    group_by(location_id, location_name, location_level, iso3, year, measure_name, estimate) %>%
    summarise_at("val", sum) %>%
    mutate(var = paste0("Asthma.", substr(measure_name, 1, 4), ".1to18")) %>%
    ihme_getrate(pop.total = pop.total) %>%
    bind_rows(asthma.prev)

  asthma_raw_data %>%
    add_location_details() %>%
    filter(
      tolower(age_name) == "all ages",
      measure_name == "Prevalence",
      metric_name == "Rate"
    ) %>%
    gather_ihme() %>%
    sel(location_id, location_name, location_level, iso3, year, measure_name, estimate, val) %>%
    mutate(var = paste0("Asthma.", substr(measure_name, 1, 4), ".0to99")) %>%
    bind_rows(asthma.prev) ->
  asthma.prev

  return(asthma.prev)
}


add_country_to_epi_wide <- function(epi_wide,
                                    base_iso3,
                                    iso3,
                                    pop,
                                    name,
                                    income_group = NULL) {
  epi_wide_new <- epi_wide %>%
    filter(
      iso3 == base_iso3,
      location_level == 3
    ) %>%
    dplyr::mutate(
      iso3 = !!iso3,
      pop = pop,
      location_id = NA,
      location_name = name,
      country = name
    )


  if (!is.null(income_group)) {
    epi_wide_new$income_group <- income_group
  }

  return(
    bind_rows(epi_wide, epi_wide_new)
  )
}


fill_and_add_missing_regions <- function(epi_wide) {
  # add missing admin regions
  epi_wide <- add_country_to_epi_wide(
    epi_wide,
    base_iso3 = "CHN",
    iso3 = "HKG",
    pop = 7.392e6,
    name = "Hong Kong",
    income_group = "High income"
  )

  epi_wide <- add_country_to_epi_wide(
    epi_wide,
    base_iso3 = "CHN",
    iso3 = "MAC",
    pop = 622567,
    name = "Macau",
    income_group = "High income"
  )

  epi_wide <- add_country_to_epi_wide(
    epi_wide,
    base_iso3 = "ALB",
    iso3 = "XKX",
    pop = 1831e3,
    name = "Kosovo"
  )


  # Fill Taiwan
  idx_taiwan <- !is.na(epi_wide$iso3) & epi_wide$iso3 == "TWN"
  idx_japan <- !is.na(epi_wide$iso3) & epi_wide$iso3 == "JPN" & epi_wide$location_level == 3
  epi_wide$country[idx_taiwan] <- "Taiwan"
  epi_wide$GDP.PPP.2011USD[idx_taiwan] <- epi_wide$GDP.PPP.2011USD[idx_japan] * 53023 / 44227
  epi_wide[idx_taiwan & epi_wide$estimate == "central", ] %>%
    unlist() %>%
    subset(is.na(.)) %>%
    names() -> fillcols
  epi_wide[idx_taiwan, fillcols] <- epi_wide[idx_japan, fillcols]


  # epi_wide[!is.na(epi_wide$iso3) & epi_wide$iso3=='XKX' & epi_wide$estimate=='central',] %>% unlist %>% subset(is.na(.)) %>%
  # names -> fillcols
  # epi_wide[!is.na(epi_wide$iso3) & epi_wide$iso3=='XKX', fillcols] <- epi_wide[epi_wide$iso3=='ALB', fillcols]

  # scale Kosovo asthma cases by population
  asthma.cols <- grep("new.asthma|exac\\.", names(epi_wide), value = T)
  epi_wide[epi_wide$iso3 == "XKX", asthma.cols] <-
    epi_wide[epi_wide$iso3 == "ALB", asthma.cols] *
      epi_wide$pop[epi_wide$iso3 == "XKX"][1] / epi_wide$pop[epi_wide$iso3 == "ALB"][1]

  return(epi_wide)
}


#' For subnational levels, we fill with national data if subnational one is not available
#'
#' @param epi
#'
#' @return
#' @export
#'
#' @examples
fill_subnational <- function(epi) {
  bind_rows(
    epi %>%
      filter(location_level != 4),
    epi %>%
      tidyr::complete(
        nesting(estimate, var),
        nesting(location_id, country, iso3, location_name, location_level, region, income_group)
      ) %>%
      filter(location_level == 4) %>%
      left_join(
        epi %>%
          filter(location_level == 3) %>%
          sel(iso3, var, estimate, val_country = val)
      ) %>%
      mutate(val = coalesce(val, val_country)) %>%
      sel(-c(val_country))
  )
}



fill_low_high <- function(indata) {
  ddply(
    indata, .(location_id),
    function(df) {
      for (col in names(df)) {
        df[[col]] <- df[[col]] %>%
          na.fill(df[[col]][df$estimate == "central"])
      }
      return(df)
    }
  )
}


add_location_details <- function(x, locations = get_locations()) {
  joiner <- locations %>%
    sel(location_id, location_level, iso3_filler = iso3, location_name_filler = location_name)

  if (length(intersect(names(x), names(joiner))) == 0) {
    if ("country" %in% names(x)) {
      x$iso3 <- countrycode::countrycode(x$country, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))
      joiner %<>% dplyr::mutate(iso3 = iso3_filler)
    } else {
      stop("Missing location details")
    }
  }

  # Ensure country-level if not explicitly stated otherwise
  if (!any(c("location_id", "location_level") %in% names(x))) {
    x$location_level <- 3
  }

  if (!"iso3" %in% names(x)) x$iso3 <- NA
  if (!"location_name" %in% names(x)) x$location_name <- NA

  y <- x %>%
    left_join(joiner) %>%
    mutate(
      iso3 = coalesce(iso3, iso3_filler),
      location_name = coalesce(location_name, location_name_filler)
    ) %>%
    sel(-c(iso3_filler, location_name_filler))

  if (nrow(x) != nrow(y)) {
    stop("Adding location details changed dataset")
  }

  y$location_id %<>% as.integer()
  y$location_level %<>% as.integer()

  return(y)
}


generate_epi <- function(version = "gbd2019") {


  library(readxl)
  library(magrittr)
  library(plyr)
  library(zoo)

  # Get data from various soruces
  wb_ind <- get_wb_ind()
  pop <- get_epi_pop(version)
  pop.total <- get_pop_total(pop)
  asthma.new <- get_asthma_new()
  asthma.erv <- get_asthma_erv(pop.total = pop.total)
  ptb <- get_ptb()
  locations <- get_locations()

  # Get data from World Bank
  lbw <- readWB_online("SH.STA.BRTW.ZS", valuename = "val", var = "LBW.rate")
  birth.rate <- readWB_online("SP.DYN.CBRT.IN", valuename = "val", var = "birth.rate")
  labor.partic <- readWB_online("SL.TLF.ACTI.ZS", valuename = "val", var = "labor.partic")
  labor.age.share <- readWB_online("SP.POP.1564.TO.ZS", valuename = "val", var = "working.age.share")
  GDP <- readWB_online("NY.GDP.PCAP.PP.KD", valuename = "val", var = "GDP.PPP.2011USD")

  # Get data from GBD
  death.all.cause <- get_death_all_cause(pop.total = pop.total, version = version)
  deaths.crude <- get_death_crude(version = version)
  death.child.lri <- get_death_child_lri(pop.total = pop.total, version = version)
  yld <- get_yld(pop.total = pop.total, version = version)
  # death.totcp <- get_death_totcp(yld = yld, pop.total = pop.total, version = version)
  asthma.prev <- get_asthma_prev(pop.total = pop.total, version = version)


  epi <- lapply(list(
    death.all.cause,
    deaths.crude,
    death.child.lri,
    yld,
    # death.totcp,
    pop.total %>% mutate(var = "pop"),
    birth.rate,
    ptb,
    lbw,
    asthma.prev,
    asthma.new,
    asthma.erv,
    labor.partic,
    labor.age.share,
    GDP
  ), function(x) {
    x %>%
      add_location_details(locations = locations)
  }) %>%
    bind_rows() %>%
    sel(location_id, location_level, iso3, var, val, estimate) %>%
    mutate(estimate = zoo::na.fill(estimate, "central")) %>%
    filter(!is.na(location_id), !is.na(val)) %>%
    # To have a single location_name per location_id (which is not necessarily the case otherwise)
    add_location_details()


  # Check that low <= central <= high
  bad <- epi %>%
    spread(estimate, val) %>%
    filter(low>central | low>high | central>high) %>%
    distinct(var)
  if(nrow(bad) > 0) {
    stop(glue("Bad values in {bad$var}"))
  }

  wbstats::wb_countries() %>%
    sel(iso3 = iso3c, region = region, income_group = income_level, country) %>%
    filter(income_group != "Aggregates") -> wb_countries

  # Ensure no duplicated row
  duplicated <- any(duplicated(epi[c('location_id', 'var', 'estimate')]))
  if(duplicated) {
    stop("Duplicate rows in epi data")
  }


  epi %<>% left_join(wb_countries %>% sel(iso3, country, region, income_group))
  epi %<>% fill_subnational()
  epi %>%
    distinct() %>%
    pivot_wider(names_from = var, values_from = val) -> epi_wide

  epi_wide <- fill_and_add_missing_regions(epi_wide)

  # add new variables for health impact calculations
  epi_wide$Absences.per <- epi_wide$working.age.share / 100 *
    epi_wide$labor.partic / 100 * 9.4 * 1e5
  epi_wide$PTB.per <- epi_wide$PTB.rate / 100 * epi_wide$birth.rate / 1e3 * 1e5
  epi_wide$LBW.per <- epi_wide$LBW.rate / 100 * epi_wide$birth.rate / 1e3 * 1e5
  epi_wide$Asthma.Prev.0to17_no2 <- (epi_wide$Asthma.Prev.1to18 / epi_wide$Asthma.Inci.1to18) *
    epi_wide$new.asthma_NO2


  list.files(path = "inst/extdata/", pattern = "CRF", full.names = T) %>%
    lapply(read_csv) %>%
    bind_rows() %>%
    mutate(Incidence = crf_recode_incidence(Incidence, Exposure)) %>%
    distinct(Incidence, Exposure) -> CRFs

  c("pop", CRFs$Incidence, names(epi_wide) %>% grep("Deaths|YLLs|YLDs|birth|labor", ., value = T)) -> inci.out

  # CRFs$Incidence[CRFs$Exposure %in% c('SO2', 'NO2') & grepl('Deaths|YLLs', CRFs$Incidence)] %<>%
  #   gsub('NCD\\.LRI', 'AllCause', .)


  epi_wide %>%
    set_names(names(.) %>%
      gsub("\\.per|_base", "", .) %>%
      gsub("_0_17", ".0to17", .) %>%
      gsub("_1_18|_1to18", ".1to18", .) %>%
      gsub("_18_99", ".18to99", .)) %>%
    fill_low_high() -> epi_hia


  bind_cols(
    select_at(epi_hia, c("location_id", "location_level")),
    select_if(epi_hia, is.character),
    select_at(epi_hia, intersect(inci.out, names(epi_hia)))
  ) %>%
    filter(!is.na(pop)) %>%
    arrange(country) ->
  epi_hia

  epi_hia %>%
    filter(!is.na(location_id)) %>%
    write_csv(glue("inst/extdata/epi_for_hia_{version}.csv"))
}


get_age_low <- function(age_name) {
  age_low <- stringr::str_extract(age_name, "^\\d+") %>% as.numeric()
  age_low[age_name == "Under 5"] <- 0 # In 2017 version
  age_low[age_name == "<5 years"] <- 0 # In 2019 version
  age_low[age_name == "All Ages"] <- -1
  age_low
}

fill_young_lungcancer <- function(ihme){
  # Lung Cancer death and YLLs missing for young age in gbd2017
  # If YLD is 0, assuming Deaths and YLL are as well.

  missing <- ihme %>%
    group_by(location_id, location_name, cause_short, cause_name, age) %>%
    # Filter groups that have Deaths but not YLD
    filter(!is.na(val)) %>%
    filter(
      cause_short %in% c("LC"),
      all(unique(measure_name) == "YLDs"),
      any(measure_name == "YLDs" & val == 0),
      age_low <=10
    ) %>%
    filter(measure_name == "YLDs")

  bind_rows(
    missing %>% mutate(measure_name = "Deaths"),
    missing %>% mutate(measure_name = "YLLs"),
    ihme)
}


generate_ihme <- function(version = "gbd2019") {

  # read IHME mortality and morbidity data to enable country calculations
  ihme <- get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(location_level %in% c(3, 4)) %>%
    dplyr::filter(metric_name == "Number") %>%
    # 25+ is redundant with all the various age groups
    dplyr::filter(!grepl("95\\+|25\\+", age_name)) %>%
    gather_ihme()

  homogenise_age_name <- function(age_name) {
    age_name %>%
      gsub(" years", "", .) %>%
      gsub(" to ", "-", .) %>%
      gsub(" plus", "+", .)
  }

  ihme$age_low <- get_age_low(ihme$age_name)
  ihme$age <- homogenise_age_name(ihme$age_name)

  if (ihme %>% group_by(age_low) %>% dplyr::summarise(count = n_distinct(age_name)) %>% pull(count) %>% max() > 1) {
    stop("Two many age categories")
  }

  ihme <- ihme %>%
    dplyr::filter(age_low >= 25) %>%
    group_by_at(vars(-val, -starts_with("age"))) %>%
    summarise_at("val", sum) %>%
    mutate(age = "25+") %>%
    bind_rows(ihme) %>%
    ungroup()

  # ihme2 <- ihme %>% add_location_details()

  ihme <- ihme %>%
    mutate(cause_short = case_when(
      grepl("Diab", cause_name) ~ CAUSE_DIABETES,
      grepl("Stroke", cause_name) ~ CAUSE_STROKE,
      grepl("Lower resp", cause_name) ~ CAUSE_LRI,
      grepl("Non-comm", cause_name) ~ "NCD",
      grepl("Isch", cause_name) ~ CAUSE_IHD,
      grepl("obstr", cause_name) ~ CAUSE_COPD,
      grepl("lung canc", cause_name) ~ CAUSE_LUNGCANCER,
      grepl("dementia", cause_name) ~ CAUSE_DEMENTIA,
      T ~ NA_character_
    )) %>%
    filter(!is.na(cause_short))


  # Check that we have all these
  if(length(setdiff(c(CAUSE_DIABETES, CAUSE_STROKE, CAUSE_LRI, "NCD", CAUSE_IHD, CAUSE_COPD, CAUSE_LUNGCANCER, CAUSE_DEMENTIA),
          unique(ihme$cause_short)))>0) stop("Missing data in IHME")


  ihme <- ihme %>%
    dplyr::filter(cause_short %in% c("NCD", CAUSE_LRI)) %>%
    group_by_at(vars(-val, -starts_with("cause"))) %>%
    dplyr::summarise(val=sum(val),
              n=n()) %>%
    {
      stopifnot(all(.$n == 2))
      .
    } %>%
    mutate(cause_name = "NCD+LRI", cause_short = CAUSE_NCDLRI) %>%
    bind_rows(ihme) %>%
    ungroup() %>%
    select(-n)


  ihme <- fill_young_lungcancer(ihme)


  ihme <- ihme %>%
    dplyr::filter(iso3 == "ALB", location_level == 3) %>%
    mutate(iso3 = "XKX", location_name = "Kosovo", location_id = NA) %>%
    bind_rows(ihme)

  # Generate a lighter version
  ihme %>%
    sel(location_id, location_name, iso3, location_level, age, measure_name, age_low, age_name, cause_short, cause_name, sex_name, metric_name, estimate, val) %>%
    filter(estimate == "central") %>%
    write_csv(glue("inst/extdata/ihme_{version}.csv"))
}
