get_cause_measure <- function(){
  # Define causes for each measure type
  # TODO: it's unclear why some e.g. Diabetes should not have YLDs
  causes_ylls <- c(CAUSE_NCDLRI, CAUSE_LRICHILD, CAUSE_IHD, CAUSE_STROKE,
                   CAUSE_COPD, CAUSE_LUNGCANCER, CAUSE_LRI, CAUSE_DEMENTIA)

  causes_ylds <- c(CAUSE_STROKE, CAUSE_DIABETES, CAUSE_COPD, CAUSE_DEMENTIA)

  causes_deaths <- c(CAUSE_NCDLRI, CAUSE_IHD, CAUSE_STROKE, CAUSE_COPD,
                     CAUSE_LUNGCANCER, CAUSE_LRI, CAUSE_LRICHILD, CAUSE_DIABETES,
                     CAUSE_DEMENTIA)

  # Create the tribble more clearly
  bind_rows(
    tibble(cause = causes_ylls, measure = MEASURE_YLLS),
    tibble(cause = causes_ylds, measure = MEASURE_YLDS),
    tibble(cause = causes_deaths, measure = MEASURE_DEATHS)
  ) %>% distinct()
}
