generate_rr_gbd2019 <- function(){

  # Summary/Mean Estimates [CSV] should be downloaded from there and unzipped
  # https://ghdx.healthdata.org/record/ihme-data/global-burden-disease-study-2019-gbd-2019-particulate-matter-risk-curves
  folder <- '~/Downloads/IHME_GBD_2019_PM_RISK_SUMM/'

  tibble(file = list.files(folder)) %>%
    mutate(
      cause = tolower(str_extract(file, 'LRI|RESP_COPD|LBW|BW|CVD_STROKE|CVD_IHD|GA|NEO_LUNG|PTB|COPD|T2_DM')),
      age = as.numeric(str_extract(file, '(?<=_)[0-9]{2}(?=_)')),
      full_path = file.path(folder, file)
    ) %>%
    # Replace NA age_group with 99 (or another value) for files without age in name
    mutate(age = ifelse(is.na(age), 9999, age)) %>%
    filter(cause != 'lbw',
           cause != 'bw',
           cause != 'ga'
    ) %>%
    mutate(age = recode_age(age)) %>%
    mutate(data = map(full_path, read_csv, col_types=cols())) %>%
    select(cause, age, data) %>%
    unnest(data) %>%
    filter(exposure_spline <= 300,
           !is.na(age)) %>%
    mutate(cause = recode_rr_causes(cause)) %>%
    select(cause,
           exposure=exposure_spline,
           age,
           exposure=exposure_spline,
           central=mean,
           low=lower,
           high=upper) %>%
    # Add child
    add_lri_child()
}
