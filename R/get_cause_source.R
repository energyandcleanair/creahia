#' Return all causes, measure and associated rr_sources
#' using rr_sources as an ordered preference indicator
#'
#' @param versions
#'
#' @returns
#' @export
#'
#' @examples get_cause_measure_version(rr_sources=c(RR_GEMM, RR_ORIGINAL))
get_cause_source <- function(rr_sources, add_measure=F) {

  rr_sources_tbl <- parse_rr_sources(rr_sources)

  existing <- lapply(unique(rr_sources_tbl$source), function(x) get_rr(x)) %>%
    bind_rows() %>%
    distinct(cause, source)

  cause_source <- inner_join(
    existing,
    rr_sources_tbl
  )

  if(add_measure){
    cause_measure <- get_cause_measure()
    cause_source <- left_join(cause_source, cause_measure, by = "cause")
  }

  return(cause_source)
}



#' Parse relative risk sources into a standardized tibble format
#'
#' @param rr_sources Input in one of three formats:
#'   1. Character vector of source names (preference order)
#'   2. Named list mapping sources to causes: list(source1=c(cause1, cause2), ...)
#'   3. Tibble with cause and source columns
#'
#' @return A tibble with cause and source columns, with no duplicate causes.
#'   When multiple sources exist for a cause, the first source in the preference
#'   order is kept.
#'
#' @examples
#' # Vector of sources in preference order
#' parse_rr_sources(c("RR_GEMM", "RR_ORIGINAL"))
#'
#' # Named list mapping sources to causes
#' parse_rr_sources(list(RR_GEMM = c("cause1", "cause2"), RR_ORIGINAL = c("cause3")))
#'
#' # Direct tibble input
#' parse_rr_sources(tibble(cause = c("cause1", "cause2"), source = c("RR_GEMM", "RR_ORIGINAL")))
parse_rr_sources <- function(rr_sources) {
  # Case 3: If already a tibble with cause and source, return directly
  if (is.data.frame(rr_sources) && all(c("cause", "source") %in% names(rr_sources))) {
    return(rr_sources)
  }

  # Case 1: Character vector of sources
  if (is.character(rr_sources) || is.factor(rr_sources)) {
    available <- lapply(rr_sources, function(x) get_rr(x)) %>%
      bind_rows() %>%
      distinct(cause, source)

    # Keep only the first occurrence of each cause (based on source preference)
    result <- available %>%
      mutate(source_priority = match(source, rr_sources)) %>%
      arrange(source_priority) %>%
      distinct(cause, .keep_all = TRUE) %>%
      select(-source_priority)

    return(result)
  }

  # Case 2: Named list mapping sources to causes
  # e.g. list(RR_GEMM = c(CAUSE_LRI, CAUSE_LUNGCANCER, CAUSE_DIABETES), RR_ORIGINAL = c(CAUSE_IHD, CAUSE_STROKE, CAUSE_COPD))
  if (is.list(rr_sources)) {
    result <- tibble(cause = character(), source = character())

    # Process each source in the list
    for (source_name in names(rr_sources)) {
      causes <- rr_sources[[source_name]]
      if (length(causes) > 0) {
        source_data <- tibble(
          cause = causes,
          source = rep(source_name, length(causes))
        )
        result <- bind_rows(result, source_data)
      }
    }

    return(result)
  }

  # If we get here, the input format wasn't recognized
  stop("Invalid format for rr_sources. Must be a character vector, named list, or tibble.")
}


#' This function converts old way to define calculated causes
#' to the new approach.
#'
#' This is to make new version compatible with old parameter definition.
#' The logic old parameters can be found in get_calc_causes_old.R
#'
#' One can see results using the combinations:
#'
#'  get_calc_causes_old(gbd_causes = "all", calc_causes = "GEMM and GBD")
#'
#'  get_calc_causes_old(gbd_causes = "default", calc_causes = "GEMM and GBD")
#'
#'  get_calc_causes_old(gbd_causes = "default", calc_causes = "GBD only")
#'
#'
#' @param calc_causes
#' @param gbd_causes
#'
#' @returns
#' @export
#'
#' @examples
convert_old_parameters_to_rr_sources <- function(calc_causes, gbd_causes){

  rr_sources <- list()
  gbd_causes <- gbd_causes %||% "default"

  if(gbd_causes == "all" && calc_causes == "GEMM and GBD"){
    rr_sources[[RR_GEMM]] <- c(CAUSE_NCDLRI)
    rr_sources[[RR_ORIGINAL]] <- c(CAUSE_LRICHILD, CAUSE_LRI, CAUSE_COPD, CAUSE_DIABETES, CAUSE_IHD, CAUSE_LUNGCANCER, CAUSE_STROKE)
  }

  if(gbd_causes == "default" && calc_causes == "GEMM and GBD"){
    rr_sources[[RR_GEMM]] <- c(CAUSE_NCDLRI, CAUSE_LRI, CAUSE_COPD, CAUSE_IHD, CAUSE_LUNGCANCER, CAUSE_STROKE)
    rr_sources[[RR_ORIGINAL]] <- c(CAUSE_LRICHILD, CAUSE_DIABETES)
  }

  if(gbd_causes == "default" && calc_causes == "GBD only"){
    rr_sources[[RR_ORIGINAL]] <- c(CAUSE_LRICHILD, CAUSE_LRI, CAUSE_COPD, CAUSE_DIABETES, CAUSE_IHD, CAUSE_LUNGCANCER, CAUSE_STROKE)
  }

  if(length(rr_sources) == 0) stop("Invalid combination of gbd_causes and calc_causes. Please check the input values.")

  return(rr_sources)
}
