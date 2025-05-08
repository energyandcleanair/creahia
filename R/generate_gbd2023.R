generate_rr_gbd2023 <- function(){

  # stop("Not validated yet! There is no age info")
  # Download RR_CURVES from GBD2023: https://cloud.ihme.washington.edu/s/dDPNXqCdSLqjSzW?dir=/IHME_GBD_2023_AIR_POLLUTION_1990_2023_RR_CURVES
  folder <- '~/Downloads/IHME_GBD_2023_AIR_POLLUTION_1990_2023_RR_CURVES/'


  tibble(file = list.files(folder)) %>%
    filter(grepl("_PM_", file),
           grepl("_MEAN_", file)
    ) %>%
    # 1 IHME_GBD_2021_AIR_POLLUTION_1990_2021_PM_RR_BIRTH_WEIGHT_MEAN_Y2022M01D31.CSV
  #   2 IHME_GBD_2021_AIR_POLLUTION_1990_2021_PM_RR_COPD_MEAN_Y2022M01D31.CSV
  # 3 IHME_GBD_2021_AIR_POLLUTION_1990_2021_PM_RR_DIABETES_MEAN_Y2022M01D31.CSV
  # 4 IHME_GBD_2021_AIR_POLLUTION_1990_2021_PM_RR_GESTATIONAL_AGE_SHIFT_MEAN_Y2022M01D31.CSV
  # 5 IHME_GBD_2021_AIR_POLLUTION_1990_2021_PM_RR_LUNG_CANCER_MEAN_Y2022M01D31.CSV
  # 6 IHME_GBD_2023_AIR_POLLUTION_1990_2022_AIR_PM_RR_DEMENTIA_MEAN_Y2024M02D07.csv      # Ignored for now
  # 7 IHME_GBD_2023_AIR_POLLUTION_1990_2022_AIR_PM_RR_ISCHEMIC_HEART_DISEASE_MEAN_Y2024M02D07.csv
  # 8 IHME_GBD_2023_AIR_POLLUTION_1990_2022_AIR_PM_RR_LOWER_RESPIRATORY_INFECTIONS_MEAN_Y2024M02D07.csv
  # 9 IHME_GBD_2023_AIR_POLLUTION_1990_2022_AIR_PM_RR_STROKE_MEAN_Y2024M02D07.csv
    mutate(
      cause_from_file = tolower(str_extract(file, 'BIRTH_WEIGHT|COPD|DIABETES|GESTATIONAL_AGE_SHIFT|ISCHEMIC_HEART_DISEASE|LOWER_RESPIRATORY_INFECTIONS|LUNG_CANCER|STROKE')),
      age = as.numeric(str_extract(file, '(?<=_)[0-9]{2}(?=_)')),
      full_path = file.path(folder, file)
    ) %>%
    # Replace NA age_group with 9999 (or another value) for files without age in name
    mutate(age = ifelse(is.na(age), 9999, age)) %>%
    mutate(age = recode_age(age)) %>%
    mutate(data = map(full_path, read_csv, col_types=cols())) %>%
    select(age, cause_from_file, data) %>%
    unnest(data) %>%
    mutate(cause=coalesce(cause, cause_from_file)) %>%
    filter(exposure <= 300,
           cause != 'bw',
           cause != 'ga',
           !is.na(age)) %>%
    mutate(cause = recode_gbd_causes(cause)) %>%
    filter(!is.na(cause)) %>%
    group_by(cause) %>%
    # New version seem to be log(RR)
    mutate(apply_exp=min(mean) == 0) %>%
    mutate(
      mean = ifelse(apply_exp, exp(mean), mean),
      lower = ifelse(apply_exp, exp(lower), lower),
      upper = ifelse(apply_exp, exp(upper), upper)
    ) %>%
    ungroup() %>%
    select(cause,
           exposure,
           age,
           central=mean,
           low=lower,
           high=upper) %>%
    # Add child
    add_lri_child()
}
