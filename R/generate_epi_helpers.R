gather_epi <- function(df) {
  df %>%
    dplyr::rename(central = val, low = lower, high = upper) %>%
    tidyr::gather(estimate, val, central, low, high) %>%
    dplyr::mutate(measure_name = measure_name %>% gsub(' .*', '', .))
}


ihme_getrate <- function(df, pop) {
  df %>%
    left_join(pop %>% sel(location_id, pop = val), by = "location_id") %>%
    mutate(val = val / pop * 1e5) %>% sel(-pop) %>%
    ungroup %>%
    distinct
}

#' Recode GBD cause name to CREAHIA cause name
#'
#' @param cause_name
#'
#' @returns
#' @export
#'
#' @examples
recode_gbd_cause <- function(cause_name){

  new_cause_names <- case_when(

    # General
    cause_name == "All causes" ~ "AllCause",
    cause_name == "Lower respiratory infections" ~ CAUSE_LRI,
    cause_name == "Tracheal, bronchus, and lung cancer" ~ CAUSE_LUNGCANCER,
    cause_name == "Diabetes mellitus type 2" ~ CAUSE_DIABETES,
    cause_name == "Alzheimer's disease and other dementias" ~ CAUSE_DEMENTIA,

    # NCD
    cause_name == "Non-communicable diseases" ~ CAUSE_NCD,

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

#' Recode GBD measure name to CREAHIA measure name
#'
#' @param measure_name
#'
#' @returns
#' @export
#'
#' @examples
recode_gbd_measure <- function(measure_name){

  new_measure_names <- case_when(
    grepl("yll", tolower(measure_name)) ~ MEASURE_YLLS,
    grepl("yld", tolower(measure_name)) ~ MEASURE_YLDS,
    grepl("death", tolower(measure_name)) ~ MEASURE_DEATHS,
    TRUE ~ NA_character_
  )

  # Print those that weren't matched
  if(any(is.na(new_measure_names))) {
    warning(glue("Some measures were not matched:", paste0(unique(measure_name[is.na(new_measure_names)]), collapse=", ")))
  }

  return(new_measure_names)
}

#' Recode GBD cause and measure names to CREAHIA names
#'
#' @param gbd_raw
#'
#' @returns
#' @export
#'
#' @examples
recode_gbd <- function(gbd_raw){
  gbd_raw %>%
    mutate(
      cause_name = recode_gbd_cause(cause_name),
      measure_name = recode_gbd_measure(measure_name)
    )
}


