.hia_specification_component_columns <- function() {
  c(
    "component_id",
    "component_type",
    "selection_id",
    "source",
    "cause",
    "outcome",
    "age_contract",
    "ready",
    "blocking_reason",
    "reproduction_status",
    "scientific_review_status"
  )
}


.capture_population_year_intent <- function(
  pop_year,
  scale_base_year,
  scale_target_year,
  resolved_pop_year
) {
  list(
    supplied = list(
      pop_year = pop_year,
      scale_base_year = scale_base_year,
      scale_target_year = scale_target_year
    ),
    resolved_pop_year = resolved_pop_year,
    scaling_requested = !is.null(resolved_pop_year) &&
      length(resolved_pop_year) > 0
  )
}


.new_hia_specification <- function(
  readiness,
  population_year_intent,
  input_contract,
  epi_version
) {
  required_readiness_fields <- c(
    "ready",
    "components",
    "blocking_components",
    "ignored_rr_pairs"
  )
  missing_readiness_fields <- setdiff(
    required_readiness_fields,
    names(readiness)
  )
  if (length(missing_readiness_fields) > 0) {
    stop(
      "HIA readiness result is missing required fields: ",
      paste(missing_readiness_fields, collapse = ", "),
      call. = FALSE
    )
  }

  component_columns <- .hia_specification_component_columns()
  missing_component_columns <- setdiff(
    component_columns,
    names(readiness$components)
  )
  if (length(missing_component_columns) > 0) {
    stop(
      "HIA readiness components are missing required fields: ",
      paste(missing_component_columns, collapse = ", "),
      call. = FALSE
    )
  }

  components <- readiness$components %>%
    dplyr::select(dplyr::all_of(component_columns))

  specification <- list(
    schema_version = 1L,
    input_contract = input_contract,
    components = list(
      crf = components %>% dplyr::filter(component_type == "crf"),
      rr = components %>% dplyr::filter(component_type == "rr"),
      epi = components %>% dplyr::filter(component_type == "epi")
    ),
    epi_version = epi_version,
    population_year = population_year_intent,
    readiness = list(
      ready = readiness$ready,
      blocking_component_ids = readiness$blocking_components$component_id,
      ignored_rr_pairs = readiness$ignored_rr_pairs
    )
  )

  structure(
    specification,
    class = c("creahia_hia_specification", "list")
  )
}
