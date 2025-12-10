# A series of functions to generate epi and ihme data from GDB and other sources



#' This is the main function to generate the necessary epidemiological files used in creahia
#'
#' @param version
#'
#' @returns
#' @export
#'
#' @examples
generate_epi <- function(version = "gbd2023") {
  generate_epi_rate_wide(version = version)
  generate_epi_count_long(version = version)
}


#' This is the main function to generate epi_rate_wide csv files
#'
#' @param version
#'
#' @returns
#' @export
#'
#' @examples
generate_epi_rate_wide <- function(version = "gbd2023") {

  library(creahia)

  # Get data from World Bank
  lbw_rate_pct <- readWB_online("SH.STA.BRTW.ZS", valuename = "val", var = "lbw_rate_pct")
  birth_rate_p1k <- readWB_online("SP.DYN.CBRT.IN", valuename = "val", var = "birth_rate_p1k")
  labor_partic_pct <- readWB_online("SL.TLF.ACTI.ZS", valuename = "val", var = "labor_partic_pct")
  labor_age_share_pct <- readWB_online("SP.POP.1564.TO.ZS", valuename = "val", var = "labor_age_share_pct")

  # Get data from other sources or compute them from World Bank data
  wb_ind <- get_wb_ind()
  pop <- get_epi_pop(version)
  asthma.new <- get_asthma_new()
  asthma.erv <- get_asthma_erv(pop = pop)
  ptb <- get_ptb(birth_rate_p1k)
  lbw <- get_lbw(birth_rate_p1k, lbw_rate_pct)
  absences <- get_absences(labor_age_share_pct, labor_partic_pct)
  locations <- get_locations()

  # Get data from GBD
  death.all.cause <- get_death_all_cause(pop = pop, version = version)
  deaths.crude <- get_death_crude(version = version)
  death.child.lri <- get_death_child_lri(pop = pop, version = version)
  yld <- get_yld(pop = pop, version = version)
  asthma.prev_inc <- get_asthma_prev_and_inc(pop = pop, version = version)

  epi <- lapply(list(
    pop %>% mutate(metric_key = "pop"),
    death.all.cause,
    death.child.lri,
    yld,
    ptb,
    lbw,
    absences,
    asthma.prev_inc,
    asthma.erv
    # Not used, and not in gbd2019 and gbd2021 versions
    # deaths.crude,
    # asthma.new,
  ), function(x) {
    x %>%
      add_location_details(locations = locations)
  }) %>%
    bind_rows() %>%
    select(location_id, location_level, iso3, metric_key, val, estimate) %>%
    mutate(estimate = zoo::na.fill(estimate, "central")) %>%
    filter(!is.na(location_id), !is.na(val)) %>%
    # To ensure a single location_name per location_id (which is not necessarily the case otherwise)
    add_location_details()

  # Some checks
  check_low_high(epi)
  check_duplicated(epi)

  # Add information
  epi <- add_region_and_income_group(epi)
  epi <- fill_subnational(epi)

  # Move to wide format
  epi_wide <- epi %>%
    distinct() %>%
    pivot_wider(names_from = metric_key, values_from = val)

  # Add missing regions (e.g. Hong Kong, Macau, Kosovo)
  epi_wide <- fill_and_add_missing_regions(epi_wide)

  # Add new variables for health impact calculations
  # epi_wide$Asthma.Prev.0to17_no2 <- (epi_wide$Asthma.Prev.1to18 / epi_wide$Asthma.Inci.1to18) *
  #   epi_wide$new.asthma_NO2

  # Last transformations
  epi_wide <- fill_low_high(epi_wide)
  epi_wide <- rearrange_epi_wide(epi_wide)

  epi_wide %>%
    filter(!is.na(location_id)) %>%
    write_csv(glue::glue("inst/extdata/epi/processed/epi_rate_wide_{version}.csv"))
}


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
    ),
    # IHME has new constraints for GBD 2023 volumes of requests. Need to split it.
    gbd2023 = list(
      raw = c(
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/a07331966aa79e306604e0410e901038", #Deaths - Rate
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/9a19665b246918a54cb43ac64579079f", #YLD - Rate
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/dbf992902689b33eb12222c8255c88e7", #YLL - Rate
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/314d2e57edd83ee2bf1fd7b2bc48c4a0", #Deaths - Number
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/22254738d5c3f9b72a920ad8ff2a89d3", #YLD - Number
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/42d57710c259ffe1aff7f1c29e3e5a2b",  #YLL - Number

        # Now sub-national data, with all of the measure/metrics above
        "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/80a38755da4a1d0b2f02a6af8575793a" # Indonesia
      ),
      asthma = "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/229016516ad62c1e6a953aaf07e8af73"
    )
  )

  url <- urls[[version]][[dataset]]

  message(glue::glue("Download {dataset} csv files from {url}"))
  message("You will first need to create an account. Then paste the csv files into the data/epi_update folder.")
  stop("Missing data")
}


get_gbd_raw <- function(version) {
  filepaths <- list(
    gbd2017 = list.files("data/epi_update", pattern = "IHME-GBD_2017_DATA-\\d.csv", full.names = T),
    gbd2019 = list.files("data/epi_update", pattern = "IHME-GBD_2019_DATA-\\d.csv", full.names = T),
    gbd2021 = list.files("data/epi_update", pattern = "IHME-GBD_2021_DATA-\\d.csv", full.names = T),
    gbd2023 = list.files("data/epi_update/gbd2023", pattern = "IHME-GBD_2023_DATA.*.csv", full.names = T)
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
    gbd2021 = c("data/epi_update/IHME-GBD_2021_DATA-asthma.csv"),
    gbd2023 = list.files("data/epi_update/gbd2023/asthma", pattern = "IHME-GBD_2023_DATA.*.csv", full.names = T)
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
      locations_w_gadm %>% select(location_id, gadm_id),
      by = c("region_id" = "gadm_id")
    ) %>%
    left_join(
      locations_w_gadm %>% select(location_id_iso3 = location_id, gadm_id),
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
  raw <- readxl::read_xlsx(get_hia_path("location_matching/IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2022M06D29.XLSX"), .name_repair = make.names) %>%
    select(
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
        select(location_id, country_id, country_name)
    }
  }
  country_mapping %<>% bind_rows()


  raw %>%
    left_join(country_mapping, by = "location_id") %>%
    select(location_id, location_name, location_level = level, country_id, country_name) %>%
    mutate(iso3 = countrycode::countrycode(country_name, "country.name", "iso3c"))
}



attach_gadm_to_locations <- function(locations = get_locations()) {
  matching_files <- get_hia_paths(pattern = "*.csv", path = "location_matching")
  matching_subnational <- lapply(matching_files, read_csv, col_types = cols()) %>%
    bind_rows() %>%
    select(iso3, ihme_level, ihme_location_name, gadm_level, gadm_id, gadm_name)

  # Create a country matching
  # Rather than relying on creahelpers::get_adm, we embed a gadm0.csv in the package.
  # It was generated like this:
  #  creahelpers::get_adm(level = 0, res = "low") %>%
  #   as.data.frame() %>%
  #   write.csv("inst/extdata/location_matching/gadm0.csv", row.names = F)

  matching_countries <- read_csv(get_hia_path("location_matching/gadm0.csv"), col_types = cols()) %>%
    select(
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
    "gbd2019" = "IHME_GBD_2019_POP_2019_Y2020M10D15.CSV",
    "gbd2021" = "IHME-GBD_2021_POP.csv",
    "gbd2023" = "IHME-GBD_2023_POP.csv"
  )[[version]]

  # We use latest year available
  pop <- read_csv(get_hia_path(glue("population/{filepath}")))

  # Recode columns if need be
  recode_cols <- list(
    year_id = "year",
    age_group_name = "age_name"
  )
  colnames(pop) <- recode(colnames(pop), !!!recode_cols)

  pop <- pop %>%
    dplyr::filter(year == max(year),
                  tolower(sex_name) == "both",
                  tolower(age_name) == "all ages") %>%
    dplyr::rename(country = location_name)
    # select(location_id, sex_id, sex_name, age_id, age_name, year, val, upper, lower)


  locations %>%
    right_join(pop, by = "location_id") %>%
    filter(location_level %in% !!level) %>%
    distinct()
}


get_wb_ind <- function() {
  read_csv("data/epi_update/World Bank indicators.csv")
}


#' Get asthma new cases (from unknown source?)
#'
#' @returns
get_asthma_new <- function() {
  read.csv("data/epi_update/new asthma cases.csv") %>%
    select(country, starts_with("X2ppb")) %>%
    gather(estimate, val, -country) %>%
    mutate(estimate = estimate %>% gsub(".*_", "", .)) %>%
    mutate(
      metric_key = "new.asthma_NO2",
      val = val * 1e3
    )
}


#' Get Asthma emergency room visits from Anenberg et al. 2018
#' https://doi.org/10.1289/EHP3766
#' https://ehp.niehs.nih.gov/doi/10.1289/ehp3766
#'
#' and scale it to population
#'
#' @param pop
#'
#' @returns
get_asthma_erv <- function(pop = NULL) {
  asthma.erv <- readxl::read_xlsx("data/epi_update/Anenberg EHP 2018 results.xlsx", sheet = "pm totconc") %>%
    filter(!is.na(ID)) %>%
    dplyr::rename(
      country = Country,
      exac.0to17 = exac_0_17_base,
      exac.18to99 = exac_18_99_base,
      exac.0to99 = exac_0_99_base
    )

  # add Serbia and Montenegro data to asthma ERV
  pop %<>% addiso()
  pop %>%
    filter(iso3 %in% c("SRB", "MNE")) %>%
    distinct() -> sm
  sm$val <- sm$val / sum(sm$val)
  asthma.erv %>%
    filter(country == "Serbia and Montenegro") %>%
    "["(c(1, 1), T) -> sm.erv
  sm.erv %<>% mutate_if(is.numeric, magrittr::multiply_by, sm$val)
  sm.erv$country <- sm$country
  sm.erv$ID <- NA

  asthma.erv$country[asthma.erv$ID == 310] <- "Democratic Republic of the Congo"
  asthma.erv$country[asthma.erv$ID == 311] <- "Congo - Brazzaville"
  asthma.erv %<>% bind_rows(sm.erv) %>% select(-ID)

  # Scale to population
  asthma.erv.scaled <- asthma.erv %>%
    mutate(iso3 = countrycode::countrycode(country, "country.name", "iso3c")) %>%
    select(country, iso3, starts_with("exac")) %>%
    left_join(pop %>% filter(location_level == 3) %>% select(iso3, pop = val))

  asthma.erv.scaled$exac.0to17 %<>% magrittr::divide_by(asthma.erv.scaled$pop) %>% magrittr::multiply_by(1e5)
  asthma.erv.scaled$exac.18to99 %<>% magrittr::divide_by(asthma.erv.scaled$pop) %>% magrittr::multiply_by(1e5)
  asthma.erv.scaled$exac.0to99 %<>% magrittr::divide_by(asthma.erv.scaled$pop) %>% magrittr::multiply_by(1e5)

  asthma.erv.scaled %>%
    select(country, iso3, starts_with("exac")) %>%
    gather(metric_key, val, -c(country, iso3))
}


get_ptb <- function(birth_rate_p1k) {
  PTB <- readxl::read_xlsx("data/epi_update/2014 National preterm birth rates.xlsx", skip = 3, col_names = F, n_max = 300)
  nms <- readxl::read_xlsx("data/epi_update/2014 National preterm birth rates.xlsx", skip = 1, col_names = F, n_max = 2)
  nms[2, ] <- ifelse(is.na(nms[2, ]), nms[1, ], nms[2, ])
  names(PTB) <- unlist(nms[2, ]) %>% make.names(unique = T)
  PTB %<>% filter(!is.na(PTB.rate)) %>% dplyr::rename(country = Country)
  # PTB %<>% dplyr::rename(val = PTB.rate) %>% mutate(var = "PTB.rate")

  # Using birth rate, we convert PTB rate (of births) to PTB rate (of population)
  PTB %>%
    add_location_details() %>%
    left_join(birth_rate_p1k %>% select(iso3, birth_rate_p1k = val), by = "iso3") %>%
    mutate(val = PTB.rate / 100 * birth_rate_p1k / 1e3 * 1e5,
           metric_key = "PTB") %>%
    filter(!is.na(val)) %>%
    select(iso3, country, metric_key, val)
}

get_lbw <- function(birth_rate_p1k, lbw_rate_pct){
  lbw_rate_pct %>%
    dplyr::rename(lbw_rate_pct = val) %>%
    add_location_details() %>%
    left_join(
      birth_rate_p1k %>% select(iso3, birth_rate_p1k = val), by = "iso3"
    ) %>%
    mutate(
      val = lbw_rate_pct / 100 * birth_rate_p1k / 1e3 * 1e5,
      metric_key = "LBW"
    )  %>%
    filter(!is.na(val)) %>%
    select(iso3, country, metric_key, val)
}

get_absences <- function(labor_age_share_pct, labor_partic_pct) {
  labor_age_share_pct %>%
    dplyr::rename(labor_age_share_pct = val) %>%
    left_join(
      labor_partic_pct %>% dplyr::rename(labor_partic_pct = val) %>% select(iso3, labor_partic_pct),
      by = "iso3"
    ) %>%
    mutate(metric_key = "Absences",
           val = labor_age_share_pct / 100 *
             labor_partic_pct / 100 * 9.4 * 1e5) %>%
    filter(!is.na(val)) %>%
    select(iso3, country, metric_key, val)

}

get_death_all_cause <- function(pop, version = "gbd2019") {
  get_gbd_raw(version) %>%
    add_location_details() %>%
    recode_gbd() %>%
    filter(cause_name %in% c(CAUSE_NCD, CAUSE_LRI),
      metric_name == "Number"
    ) %>%
    mutate(age_low = get_age_low(age_name)) %>%
    filter(!is.na(age_low), age_low >= 30) %>%
    filter(location_level %in% c(3, 4)) %>%
    gather_epi() %>%
    group_by(location_id, location_name, location_level, measure_name, metric_name, estimate) %>%
    summarise_at("val", sum) %>%
    ihme_getrate(pop = pop) %>%
    mutate(metric_key = build_metric_key(CAUSE_NCDLRI, measure_name))
}


get_death_crude <- function(version = "gbd2019") {
  get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(age_name == "All ages", cause_name == "All causes", metric_name == "Rate", measure_name == "Deaths") %>%
    gather_epi() %>%
    mutate(metric_key = "crude.death.rate")
}


get_death_child_lri <- function(pop, version = "gbd2019") {
  get_gbd_raw(version) %>%
    add_location_details() %>%
    recode_gbd() %>%
    filter(
      cause_name == CAUSE_LRI,
      metric_name == "Number",
      age_name %in% c("Under 5", "<5 years")
    ) %>%
    mutate(age_name = AGE_CHILDREN) %>%
    gather_epi() %>%
    ihme_getrate(pop = pop) %>%
    filter(measure_name %in% c(MEASURE_DEATHS, MEASURE_YLLS)) %>%
    mutate(metric_key = build_metric_key(CAUSE_LRICHILD, measure_name))
}


get_yld <- function(pop, version = "gbd2019") {

  if(version == "gbd2017"){
    return(get_yld_gbd2017(pop=pop, version=version))
  }

  get_gbd_raw(version) %>%
    recode_gbd() %>%
    add_location_details() %>%
    filter(
      metric_name == "Number",
      measure_name %in% c(MEASURE_YLLS, MEASURE_DEATHS, MEASURE_YLDS)
    ) %>%
    filter(age_name=="25+ years") %>%
    gather_epi() %>%
    ihme_getrate(pop = pop) %>%
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
    ungroup() %>%
    filter(!is.na(cause_name)) %>%
    mutate(metric_key = build_metric_key(cause_name, measure_name))

}


#' Because we didn't download all respiratory and cardiovascular diseases,
#' we use another approach to determine OthCV and OthResp, by substracting
#' those we have from the total
#'
#' @return
#' @export
#'
#' @examples
get_yld_gbd2017 <- function(pop, version){

  yld <- get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(
      metric_name == "Number",
      measure_name %in% c(MEASURE_YLLS, MEASURE_DEATHS, MEASURE_YLDS)
    ) %>%
    mutate(age_low = get_age_low(age_name)) %>%
    filter(!is.na(age_low), age_low >= 25) %>%
    gather_epi() %>%
    group_by(location_id, location_name, location_level, iso3, cause_name, metric_name, measure_name, estimate, year) %>%
    summarise_at("val", sum) %>%
    ihme_getrate(pop = pop) %>%
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
    mutate(metric_key = build_metric_key(cause_name, measure_name))

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
        metric_key = paste0(newname, "_", unique(group$measure_name)),
        estimate = c("central", "low", "high"),
        val = c(value_O, lower_O, upper_O)
      )

      return(other_df)
    })
}


#' Get asthma prevalence and incidence from GBD
#'
#' @param pop
#' @param version
#'
#' @returns
#' @export
#'
#' @examples
get_asthma_prev_and_inc <- function(pop, version = "gbd2019") {

  asthma_raw_data <- get_gbd_asthma_raw(version = version) %>%
    mutate(age_low = get_age_low(age_name))

  # asthma.prev <- asthma_raw_data %>%
  #   add_location_details() %>%
  #   filter(
  #     measure_name %in% c("Incidence", "Prevalence"),
  #     metric_name == "Rate"
  #   ) %>%
  #   gather_epi() %>%
  #   filter(tolower(age_name) != "all ages") %>%
  # # Some of the asthma data we downloaded from GBD have 1-4, others <5 (<1 year is 0), hence the 0 and 1
  #   filter(age_low %in% c(0, 1, 5, 10, 15)) %>%
  #   group_by(location_id, location_name, location_level, iso3, year, measure_name, estimate) %>%
  #   summarise_at("val", mean) %>%
  #   mutate(metric_key = paste0("Asthma.", substr(measure_name, 1, 4), ".1to18nopopnorm")) %>%
  #   ungroup() %>%
  #   distinct()

  asthma.prev <- asthma_raw_data %>%
    add_location_details() %>%
    filter(
      measure_name %in% c("Incidence", "Prevalence"),
      metric_name == "Number"
    ) %>%
    gather_epi() %>%
    filter(tolower(age_name) != "all ages") %>%
    # Some of the asthma data we downloaded from GBD have 1-4, others <5 (<1 year is 0), hence the 0 and 1
    filter(age_low %in% c(0, 1, 5, 10, 15)) %>%
    group_by(location_id, location_name, location_level, iso3, year, measure_name, estimate) %>%
    summarise_at("val", sum) %>%
    mutate(metric_key = paste0("Asthma.", substr(measure_name, 1, 4), ".1to18")) %>%
    ihme_getrate(pop = pop)
  # %>%
  # bind_rows(asthma.prev)

  asthma_raw_data %>%
    add_location_details() %>%
    filter(
      tolower(age_name) == "all ages",
      measure_name == "Prevalence",
      metric_name == "Rate"
    ) %>%
    gather_epi() %>%
    select(location_id, location_name, location_level, iso3, year, measure_name, estimate, val) %>%
    mutate(metric_key = paste0("Asthma.", substr(measure_name, 1, 4), ".0to99")) %>%
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

  # Fill Taiwan using Japan data
  idx_taiwan <- !is.na(epi_wide$iso3) & epi_wide$iso3 == "TWN"
  idx_japan <- !is.na(epi_wide$iso3) & epi_wide$iso3 == "JPN" & epi_wide$location_level == 3
  epi_wide$country[idx_taiwan] <- "Taiwan"
  epi_wide[idx_taiwan & epi_wide$estimate == "central", ] %>%
    unlist() %>%
    subset(is.na(.)) %>%
    names() -> fillcols
  epi_wide[idx_taiwan, fillcols] <- epi_wide[idx_japan, fillcols]

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
        nesting(estimate, metric_key),
        nesting(location_id, country, iso3, location_name, location_level, region, income_group)
      ) %>%
      filter(location_level == 4) %>%
      left_join(
        epi %>%
          filter(location_level == 3) %>%
          select(iso3, metric_key, estimate, val_country = val),
        by = c("iso3", "metric_key", "estimate")
      ) %>%
      mutate(val = coalesce(val, val_country)) %>%
      select(-c(val_country))
  )
}


fill_low_high <- function(indata) {
  # For each location_id and numeric metric column, fill missing values
  # in non-central estimates with the central estimate value

  numeric_cols <- names(indata)[vapply(indata, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("location_id", "location_level"))

  indata %>%
    dplyr::group_by(location_id) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(numeric_cols),
      ~ dplyr::coalesce(.x, .[estimate == "central"][1])
    )) %>%
    dplyr::ungroup()
}


add_location_details <- function(x, locations = get_locations()) {
  joiner <- locations %>%
    select(location_id, location_level, iso3_filler = iso3, location_name_filler = location_name)

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

  # Determine which columns to join on based on what's available
  join_cols <- intersect(names(x), names(joiner))
  if (length(join_cols) == 0) {
    # If no common columns, we can't join
    stop("No common columns found for joining location details")
  }

  y <- x %>%
    left_join(joiner, by = join_cols) %>%
    mutate(
      iso3 = coalesce(iso3, iso3_filler),
      location_name = coalesce(location_name, location_name_filler)
    ) %>%
    select(-c(iso3_filler, location_name_filler))

  if (nrow(x) != nrow(y)) {
    stop("Adding location details changed dataset")
  }

  y$location_id %<>% as.integer()
  y$location_level %<>% as.integer()

  return(y)
}


add_region_and_income_group <- function(epi) {
  wb_countries <- wbstats::wb_countries() %>%
    select(iso3 = iso3c, region = region, income_group = income_level, country) %>%
    filter(income_group != "Aggregates")

  epi %>%
    left_join(wb_countries %>% select(iso3, country, region, income_group), by = "iso3")
}

check_low_high <- function(epi){
  # Check that low <= central <= high
  bad <- epi %>%
    spread(estimate, val) %>%
    filter(low>central | low>high | central>high) %>%
    distinct(metric_key)
  if(nrow(bad) > 0) {
    stop(glue("Bad values in {bad$metric_key}"))
  }
}

check_duplicated <- function(epi){
  duplicated <- any(duplicated(epi[c('location_id', 'metric_key', 'estimate')]))
  if(duplicated) {
    stop("Duplicate rows in epi data")
  }
}

rearrange_epi_wide <- function(epi_wide){
  bind_cols(
    select_at(epi_wide, c("location_id", "location_level")),
    select_if(epi_wide, is.character),
    # All remaining columns
    select_if(epi_wide, is.numeric) %>%
      select(order(colnames(.))) %>%
      # Without location_id and lovation_level
      select(-c(location_id, location_level))
  ) %>%
    filter(!is.na(pop)) %>%
    arrange(country)
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
    group_by(location_id, location_name, cause_name, age) %>%
    # Filter groups that have Deaths but not YLD
    filter(!is.na(val)) %>%
    filter(
      cause_name %in% c("LC"),
      all(unique(measure_name) == MEASURE_YLDS),
      any(measure_name == MEASURE_YLDS & val == 0),
      age_low <=10
    ) %>%
    filter(measure_name == MEASURE_YLDS)

  bind_rows(
    missing %>% mutate(measure_name = MEASURE_DEATHS),
    missing %>% mutate(measure_name = MEASURE_YLLS),
    ihme) %>%
    ungroup()
}


#' This is the main function to generate epi with count in long format
#'
#' @param version
#'
#' @returns
#' @export
#'
#' @examples
generate_epi_count_long <- function(version = "gbd2023") {

  # read IHME mortality and morbidity data to enable country calculations
  epi_long <- get_gbd_raw(version) %>%
    add_location_details() %>%
    filter(location_level %in% c(3, 4)) %>%
    dplyr::filter(metric_name == "Number") %>%
    # 25+ is redundant with all the various age groups
    dplyr::filter(!grepl("95\\+|25\\+", age_name)) %>%
    gather_epi()

  homogenise_age_name <- function(age_name) {
    age_name %>%
      gsub(" years", "", .) %>%
      gsub(" to ", "-", .) %>%
      gsub(" plus", "+", .) %>%
      gsub("^<5$", AGE_CHILDREN, .)
  }

  epi_long$age_low <- get_age_low(epi_long$age_name)
  epi_long$age <- homogenise_age_name(epi_long$age_name)

  if (epi_long %>% group_by(age_low) %>% dplyr::summarise(count = n_distinct(age_name)) %>% pull(count) %>% max() > 1) {
    stop("Two many age categories")
  }

  epi_long <- epi_long %>%
    dplyr::filter(age_low >= 25) %>%
    group_by_at(vars(-val, -starts_with("age"))) %>%
    summarise_at("val", sum) %>%
    mutate(age = "25+") %>%
    bind_rows(epi_long) %>%
    ungroup()

  epi_long <- epi_long %>%
    recode_gbd() %>%
    filter(!is.na(cause_name)) %>%
    # Because multiple causes go into OthCV and OthResp, we aggregate
    group_by(location_id, location_name, iso3, location_level, age, measure_name, age_low, age_name, cause_name, sex_name, metric_name, estimate) %>%
    summarise(val = sum(val), .groups = "drop")

  # Check that we have all these
  if(length(setdiff(c(CAUSE_DIABETES, CAUSE_STROKE, CAUSE_LRI, CAUSE_NCD, CAUSE_IHD, CAUSE_COPD, CAUSE_LUNGCANCER, CAUSE_DEMENTIA),
          unique(epi_long$cause_name)))>0) stop("Missing data in epi_long")

  epi_long <- epi_long %>%
    dplyr::filter(cause_name %in% c(CAUSE_NCD, CAUSE_LRI)) %>%
    group_by_at(vars(-val, -starts_with("cause"))) %>%
    dplyr::summarise(val=sum(val),
              n=n()) %>%
    {
      stopifnot(all(.$n == 2))
      .
    } %>%
    mutate(cause_name = CAUSE_NCDLRI) %>%
    bind_rows(epi_long) %>%
    ungroup() %>%
    select(-n)


  epi_long <- fill_young_lungcancer(epi_long)

  # Add LRI.CHILD
  epi_long <- epi_long %>%
    mutate(cause_name = case_when(cause_name==CAUSE_LRI & age==AGE_CHILDREN ~ CAUSE_LRICHILD,
                                 T ~ cause_name))

  # Add Kosovo
  epi_long <- epi_long %>%
    dplyr::filter(iso3 == "ALB", location_level == 3) %>%
    mutate(iso3 = "XKX", location_name = "Kosovo", location_id = NA) %>%
    bind_rows(epi_long)

  # Check age completeness (allows both aggregate and split ages to coexist)
  check_age_completeness(unique(epi_long$age), data_name = glue::glue("epi_long {version}"))

  # Generate a lighter version
  epi_long %>%
    select(location_id, location_name, iso3, location_level, age, measure_name, age_low, age_name, cause=cause_name, sex_name, metric_name, estimate, val) %>%
    filter(estimate == "central") %>%
    write_csv(glue::glue("inst/extdata/epi/processed/epi_count_long_{version}.csv"))
}
