


generate_rrs <- function(){


  versions <- c(
    RR_ORIGINAL,
    RR_GBD2019,
    RR_GBD2021
  )


  rrs <- lapply(versions, function(version){

    raw <- if(version == RR_ORIGINAL){
      generate_gbd_rr_original()
    } else if(version == RR_GBD2019){
      generate_gbd_rr_gbd2019()
    } else if(version == RR_GBD2021){
      generate_gbd_rr_gbd2021()
    }

    raw %>%
      check_rr() %>%
      mutate(version=version)

  }) %>%
    bind_rows()


  ggplot(rrs %>%
           pivot_longer(cols=c(low, central, high), names_to='rr_type', values_to='rr') %>%
           filter(rr_type=='central')
           ,
         aes(exposure, rr, col=version,
             # linetype = rr_type
             )) +
    geom_line() +
    facet_grid(cause_short~age, scales='free_y')




}

#' Check format and data
#'
#' @returns
#' @export
#'
#' @examples
check_rr <- function(rr){

  # No NA age
  stopifnot(all(!is.na(rr$age)))

  # Cause short is known
  stopifnot(all(rr$cause_short %in% c('LRI', 'COPD', 'LBW', 'IHD', 'Stroke', 'LC', 'PTB', 'GA', 'Diabetes', 'LRI.child')))

  # Colnames
  stopifnot(setequal(colnames(rr), c('cause_short', 'age', 'exposure', 'low', 'central', 'high')))

  # Exposure 0-300
  stopifnot(min(rr$exposure) == 0)
  stopifnot(max(rr$exposure) == 300)

  rr
}

recode_gbd_causes <- function(cause){
  recode(cause,
         lri = 'LRI',
         lower_respiratory_infections = 'LRI',
         t2_dm = 'Diabetes',
         diabetes = 'Diabetes',
         cvd_ihd = 'IHD',
         ischaemic_heart_disease = 'IHD',
         cvd_stroke = 'Stroke',
         stroke = 'Stroke',
         neo_lung = 'LC',
         lung_cancer = 'LC',
         resp_copd = 'COPD',
         copd = 'COPD',
         bw = 'LBW',
         birth_weight = 'LBW',
         ptb = 'PTB',
         ga = 'GA',
         gestational_age_shift = 'GA'
  )
}


recode_age <- function(age){
  case_when(age == 99 ~ '25+',
            age == 80 ~ '80+',
            age < 80 ~ paste0(age, '-', age + 4))
}


add_lri_child <- function(rrs){
  rrs %>% filter(cause_short == 'LRI') %>%
    mutate(cause_short = 'LRI.child', age = 'Under 5') %>%
    bind_rows(rrs)
}

generate_gbd_rr_original <- function(){

  # Sent to Lauri by email
  read_csv(get_hia_path('ier_computed_table.csv'), col_types = cols()) %>%
    # there's a weird one where rr_upper < rr_mean
    mutate(
      low = rr_lower,
      central = pmin(rr_mean, rr_upper),
      high = pmax(rr_mean, rr_upper)
    ) %>%
    select(-c(rr_lower, rr_mean, rr_upper)) %>%
    mutate(cause_short = recode_gbd_causes(cause)) %>%
    mutate(age = recode_age(age)) %>%
    dplyr::filter(exposure <= 300, !is.na(age)) %>%
    select(cause_short, age, exposure, low, central, high) %>%
    # add the LRI risk function to be used for children
    add_lri_child()
}

generate_gbd_rr_gbd2019 <- function(){

  # Summary/Mean Estimates [CSV] should be downloaded from there and unzipped
  # https://ghdx.healthdata.org/record/ihme-data/global-burden-disease-study-2019-gbd-2019-particulate-matter-risk-curves
  folder <- '~/Downloads/IHME_GBD_2019_PM_RISK_SUMM/'


  tibble(file = list.files(folder)) %>%
    mutate(
      cause = tolower(str_extract(file, 'LRI|RESP_COPD|BW|CVD_STROKE|CVD_IHD|GA|NEO_LUNG|PTB|COPD|T2_DM')),
      age = as.numeric(str_extract(file, '(?<=_)[0-9]{2}(?=_)')),
      full_path = file.path(folder, file)
    ) %>%
    # Replace NA age_group with 99 (or another value) for files without age in name
    mutate(age = ifelse(is.na(age), 99, age)) %>%
    mutate(age = recode_age(age)) %>%
    mutate(data = map(full_path, read_csv)) %>%
    select(cause, age, data) %>%
    unnest(data) %>%
    filter(exposure_spline <= 300,
           !is.na(age)) %>%
    mutate(cause_short = recode_gbd_causes(cause)) %>%
    select(cause_short,
           exposure=exposure_spline,
           age,
           exposure=exposure_spline,
           central=mean,
           low=lower,
           high=upper) %>%
  # Add child
  add_lri_child()
}

generate_gbd_rr_gbd2021 <- function(){

  # Download RR_CURVES from GBD2021: https://cloud.ihme.washington.edu/s/WXGaoWbd5ksS6jm
  folder <- '~/Downloads/IHME_GBD_2021_AIR_POLLUTION_1990_2021_RR_CURVES/'


  tibble(file = list.files(folder)) %>%
    filter(grepl("_PM_", file),
           grepl("_MEAN_", file)
           ) %>%
    mutate(
      # cause = tolower(str_extract(file, 'BIRTH_WEIGHT|COPD|DIABETES|GESTATIONAL_AGE_SHIFT|ISCHEMIC_HEART_DISEASE|LOWER_RESPIRATORY_INFECTIONS|LUNG_CANCER|STROKE')),
      age = as.numeric(str_extract(file, '(?<=_)[0-9]{2}(?=_)')),
      full_path = file.path(folder, file)
    ) %>%
    # Replace NA age_group with 99 (or another value) for files without age in name
    mutate(age = ifelse(is.na(age), 99, age)) %>%
    mutate(age = recode_age(age)) %>%
    mutate(data = map(full_path, read_csv)) %>%
    select(age, data) %>%
    unnest(data) %>%
    filter(exposure <= 300,
           !is.na(age)) %>%
    mutate(cause_short = recode_gbd_causes(cause)) %>%
    select(cause_short,
           exposure,
           age,
           central=mean,
           low=lower,
           high=upper) %>%
    # Add child
    add_lri_child()
}
