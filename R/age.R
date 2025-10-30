#' Check age group completeness
#'
#' Validates that if any AGE_ADULTS_SPLIT age groups are present, all must be present.
#' This ensures we have complete age coverage for health impact calculations.
#'
#' @param ages Vector of age groups present in the data
#' @param data_name Optional name of dataset for error messages
#'
#' @returns TRUE if checks pass, otherwise throws an error
#' @export
#'
#' @examples
#' \dontrun{
#' # This should pass - all split ages present
#' check_age_completeness(c("25-29", "30-34", "35-39", "40-44", 
#'                          "45-49", "50-54", "55-59", "60-64",
#'                          "65-69", "70-74", "75-79", "80+"))
#'
#' # This should pass - split ages plus aggregate (both can coexist, will be deduplicated later)
#' check_age_completeness(c("25+", "25-29", "30-34", "35-39", "40-44", 
#'                          "45-49", "50-54", "55-59", "60-64",
#'                          "65-69", "70-74", "75-79", "80+"))
#'
#' # This should fail - incomplete split ages
#' check_age_completeness(c("25-29", "30-34", "35-39"))
#' }
check_age_completeness <- function(ages, data_name = "data") {
  
  # Remove NA values
  ages <- ages[!is.na(ages)]
  
  # Check completeness: if any AGE_ADULTS_SPLIT are present, all must be present
  split_ages_present <- intersect(ages, AGE_ADULTS_SPLIT)
  
  if(length(split_ages_present) > 0) {
    missing_split_ages <- setdiff(AGE_ADULTS_SPLIT, ages)
    
    if(length(missing_split_ages) > 0) {
      stop(glue::glue(
        "Incomplete age coverage in {data_name}: ",
        "Found {length(split_ages_present)} of {length(AGE_ADULTS_SPLIT)} split age groups. ",
        "Missing: {paste(missing_split_ages, collapse = ', ')}"
      ))
    }
  }
  
  return(TRUE)
}


#' Check age group coverage and uniqueness
#'
#' Validates that age groups satisfy both completeness and non-overlap requirements:
#' 1. If AGE_ADULTS_SPLIT age groups are used, all must be present (completeness)
#' 2. No overlap between aggregate (25+) and split age groups (uniqueness)
#'
#' Use this for data that will be used directly without deduplication.
#' Use check_age_completeness() for data that allows both aggregate and split (e.g., IHME data).
#'
#' @param ages Vector of age groups present in the data
#' @param data_name Optional name of dataset for error messages
#'
#' @returns TRUE if checks pass, otherwise throws an error
#' @export
#'
#' @examples
#' \dontrun{
#' # This should pass
#' check_age_coverage_and_uniqueness(c("25-29", "30-34", "35-39", "40-44", 
#'                                      "45-49", "50-54", "55-59", "60-64",
#'                                      "65-69", "70-74", "75-79", "80+"))
#'
#' # This should fail (incomplete)
#' check_age_coverage_and_uniqueness(c("25-29", "30-34", "35-39"))
#'
#' # This should fail (overlap)
#' check_age_coverage_and_uniqueness(c("25+", "25-29", "30-34"))
#' }
check_age_coverage_and_uniqueness <- function(ages, data_name = "data") {
  
  # Remove NA values
  ages <- ages[!is.na(ages)]
  
  # Check 1: Completeness - if any AGE_ADULTS_SPLIT are present, all must be present
  split_ages_present <- intersect(ages, AGE_ADULTS_SPLIT)
  
  if(length(split_ages_present) > 0) {
    missing_split_ages <- setdiff(AGE_ADULTS_SPLIT, ages)
    
    if(length(missing_split_ages) > 0) {
      stop(glue::glue(
        "Incomplete age coverage in {data_name}: ",
        "Found {length(split_ages_present)} of {length(AGE_ADULTS_SPLIT)} split age groups. ",
        "Missing: {paste(missing_split_ages, collapse = ', ')}"
      ))
    }
  }
  
  # Check 2: Uniqueness - no overlap between aggregate (25+) and split ages
  has_aggregate <- AGE_ADULTS %in% ages
  has_split <- length(split_ages_present) > 0
  
  if(has_aggregate && has_split) {
    stop(glue::glue(
      "Age group overlap in {data_name}: ",
      "Both aggregate age group '{AGE_ADULTS}' and split age groups are present. ",
      "This creates overlapping age ranges."
    ))
  }
  
  return(TRUE)
}


#' Deduplicate adult ages: ensure no overlap i.e. no adult age (25+) AND age intervals e.g. 40-44
#'
#'
#' @param ages Vector of age groups
#'
#' @returns Vector of deduplicated age groups
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove aggregate if all split ages present
#' deduplicate_adult_ages(c("25+", "25-29", "30-34", "35-39", "40-44", 
#'                          "45-49", "50-54", "55-59", "60-64",
#'                          "65-69", "70-74", "75-79", "80+"))
#' }
deduplicate_adult_ages <- function(ages){

  # If all detailed from 25 to 80+ available, use them
  if(all(AGE_ADULTS_SPLIT %in% ages)) {
    ages <- ages[ages != AGE_ADULTS]
  }
  # Otherwise, use AGE_ADULTS
  if(any(ages == AGE_ADULTS)) {
    ages <- ages[!ages %in% AGE_ADULTS_SPLIT]
  }

  ages
}
