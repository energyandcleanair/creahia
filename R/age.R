#' Deduplicate adult ages: ensure no overlap i.e. no adult age (25+) AND age intervals e.g. 40-44
#'
#'
#' @param ages
#'
#' @returns
#' @export
#'
#' @examples
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
