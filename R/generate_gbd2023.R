generate_rr_gbd2023 <- function(folder = '~/Downloads/IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES/'){

  # Official release: Global Burden of Disease Study 2023 (GBD 2023) Air Pollution Exposure
  # Estimates and Risk Curves 1990-2023, https://doi.org/10.6069/K4TW-Q814
  # Download "Relative Risk Estimates" from
  # https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023
  # and unzip it in `folder`. PM mean files in that release:
  # IHME_GBD_2023_AIR_POLLUTION_1990_2021_PM_RR_BIRTH_WEIGHT_MEAN_Y2022M01D31.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2021_PM_RR_COPD_MEAN_Y2022M01D31.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2021_PM_RR_DIABETES_MEAN_Y2022M01D31.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2021_PM_RR_GESTATIONAL_AGE_SHIFT_MEAN_Y2022M01D31.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2021_PM_RR_LUNG_CANCER_MEAN_Y2022M01D31.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2022_PM_RR_DEMENTIA_MEAN_Y2025M11D18.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2022_PM_RR_ISCHEMIC_HEART_DISEASE_MEAN_Y2025M11D18.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2022_PM_RR_LOWER_RESPIRATORY_INFECTIONS_MEAN_Y2025M11D18.CSV
  # IHME_GBD_2023_AIR_POLLUTION_1990_2022_PM_RR_STROKE_MEAN_Y2025M11D18.CSV
  # COPD, Diabetes and LC are the GBD 2021 curves, re-released as is. The 2025 files
  # have no cause column, use `risk` for exposure, and are in log(RR).
  # The packaged rr_gbd2023.csv only takes Dementia from this output. IHD, LRI and
  # Stroke are kept from the earlier 2024 files (another exposure grid, within 1e-4 RR)
  # so that their results do not change.
  read_rr_file <- function(path){
    read_csv(path, col_types=cols()) %>%
      rename(any_of(c(exposure = "risk")))
  }

  tibble(file = list.files(folder)) %>%
    filter(grepl("_PM_", file),
           grepl("_MEAN_", file)
    ) %>%
    mutate(
      cause_from_file = tolower(str_extract(file, 'BIRTH_WEIGHT|COPD|DIABETES|GESTATIONAL_AGE_SHIFT|ISCHEMIC_HEART_DISEASE|LOWER_RESPIRATORY_INFECTIONS|LUNG_CANCER|STROKE|DEMENTIA')),
      age = as.numeric(str_extract(file, '(?<=_)[0-9]{2}(?=_)')),
      full_path = file.path(folder, file)
    ) %>%
    # Replace NA age_group with 9999 (or another value) for files without age in name
    mutate(age = ifelse(is.na(age), 9999, age)) %>%
    mutate(age = recode_age(age)) %>%
    mutate(data = map(full_path, read_rr_file)) %>%
    select(age, cause_from_file, data) %>%
    unnest(data) %>%
    mutate(cause=coalesce(cause, cause_from_file)) %>%
    filter(exposure <= 300,
           cause != 'bw',
           cause != 'ga',
           !is.na(age)) %>%
    mutate(cause = recode_rr_causes(cause)) %>%
    filter(!is.na(cause)) %>%
    dplyr::group_by(cause) %>%
    # New version seem to be log(RR)
    dplyr::mutate(apply_exp=mean[exposure==0]==0) %>%
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
