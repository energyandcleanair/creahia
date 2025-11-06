generate_rr_gbd2021 <- function(){

  # stop("Not validated yet! There is no age info")
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
    # Replace NA age_group with 9999 (or another value) for files without age in name
    mutate(age = ifelse(is.na(age), 9999, age)) %>%
    mutate(age = recode_age(age)) %>%
    mutate(data = map(full_path, read_csv, col_types=cols())) %>%
    select(age, data) %>%
    unnest(data) %>%
    filter(exposure <= 300,
           cause != 'bw',
           cause != 'ga',
           !is.na(age)) %>%
    mutate(cause = recode_rr_causes(cause)) %>%
    select(cause,
           exposure,
           age,
           central=mean,
           low=lower,
           high=upper) %>%
    # Add child
    add_lri_child()
}
