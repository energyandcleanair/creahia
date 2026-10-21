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
    rr_sources_tbl,
    by = c("cause", "source")
  )

  if(add_measure){
    cause_measure <- get_cause_measure()
    if ("outcome" %in% names(cause_source)) {
      cause_source <- cause_source %>%
        dplyr::mutate(measure = as.character(outcome)) %>%
        dplyr::semi_join(cause_measure, by = c("cause", "measure"))
    } else {
      cause_source <- left_join(cause_source, cause_measure, by = "cause")
    }
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
#' parse_rr_sources(tibble(cause = c("cause1", "cause2"), source = c(RR_GEMM, RR_ORIGINAL)))
parse_rr_sources <- function(rr_sources) {
  
  # Case 1: If already a tibble with cause and source, return directly
  if (is.data.frame(rr_sources) && all(c("cause", "source") %in% names(rr_sources))) {
    return(rr_sources)
  }

  # Case 2: Character vector of sources
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

  # Case 3: Named list mapping sources to causes
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

  if (identical(calc_causes, "none")) {
    return(tibble::tibble(
      cause = character(),
      source = character(),
      outcome = character()
    ))
  }

  if (length(calc_causes) == 0) {
    stop(
      paste(
        "calc_causes must be provided as a preset name,",
        "a '<cause>_<outcome>' vector, or \"none\";",
        "NULL and empty character vectors are not valid conversion inputs."
      ),
      call. = FALSE
    )
  }

  gbd_causes <- gbd_causes %||% "default"

  preset_labels <- c("GEMM and GBD", "GBD only", "GEMM only")
  explicit_selection <- length(calc_causes) != 1 ||
    !(calc_causes %in% preset_labels)

  if (explicit_selection) {
    if (!is.character(calc_causes) || any(!grepl("_", calc_causes))) {
      stop(
        paste(
          "Explicit legacy calc_causes values must use",
          "'<cause>_<outcome>' names."
        ),
        call. = FALSE
      )
    }
    selected <- tibble::tibble(
      cause = sub("_.*$", "", calc_causes),
      outcome = sub("^[^_]+_", "", calc_causes)
    )
    legacy_gemm_causes <- setdiff(
      unique(get_rr(RR_GEMM)$cause),
      CAUSE_LRICHILD
    )
    original_causes <- unique(get_rr(RR_ORIGINAL)$cause)
    selected_original_causes <- if (identical(gbd_causes, "default")) {
      setdiff(selected$cause, legacy_gemm_causes)
    } else if (identical(gbd_causes, "all")) {
      intersect(selected$cause, original_causes)
    } else {
      intersect(selected$cause, as.character(gbd_causes))
    }
    selected <- selected %>%
      dplyr::mutate(source = dplyr::if_else(
        cause %in% selected_original_causes,
        RR_ORIGINAL,
        RR_GEMM
      ))

    unavailable <- selected %>%
      dplyr::filter(
        (source == RR_GEMM & !(cause %in% legacy_gemm_causes)) |
          (source == RR_ORIGINAL & !(cause %in% original_causes))
      )

    if (nrow(unavailable) > 0) {
      stop(
        "Legacy calc_causes contains causes without an RR source: ",
        paste(unique(unavailable$cause), collapse = ", "),
        call. = FALSE
      )
    }

    supported <- get_cause_measure() %>%
      dplyr::transmute(cause, outcome = measure)
    unsupported <- dplyr::anti_join(
      selected,
      supported,
      by = c("cause", "outcome")
    )
    if (nrow(unsupported) > 0) {
      stop(
        "Legacy calc_causes contains unsupported cause/outcome values: ",
        paste(
          paste0(unsupported$cause, "_", unsupported$outcome),
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    return(selected %>% dplyr::distinct(cause, source, outcome))
  }

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

  if(gbd_causes == "default" && calc_causes == "GEMM only"){
    rr_sources[[RR_GEMM]] <- c(CAUSE_NCDLRI, CAUSE_LRI, CAUSE_COPD, CAUSE_IHD, CAUSE_LUNGCANCER, CAUSE_STROKE)
  }

  if(length(rr_sources) == 0) stop("Invalid combination of gbd_causes and calc_causes. Please check the input values.")

  return(rr_sources)
}
