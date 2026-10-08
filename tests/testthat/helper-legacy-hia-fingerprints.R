legacy_hia_fingerprint_regions <- function() {
  tibble::tibble(
    region_id = "fingerprint_region",
    region_name = "Fingerprint region",
    country_id = "FPR"
  )
}

legacy_hia_empty_rr_sources <- function() {
  tibble::tibble(
    cause = character(),
    source = character()
  )
}

legacy_hia_resolved_rr_components <- function(rr_sources) {
  if (!has_rr_sources(rr_sources)) {
    return(tibble::tibble(
      cause = character(),
      source = character(),
      outcome = character()
    ))
  }

  get_cause_source(rr_sources, add_measure = TRUE) %>%
    dplyr::transmute(
      cause = .data$cause,
      source = .data$source,
      outcome = .data$measure
    ) %>%
    dplyr::arrange(.data$source, .data$cause, .data$outcome)
}

legacy_hia_metric_components <- function(crfs, rr_sources) {
  crf_components <- crfs %>%
    dplyr::distinct(.data$cause, .data$outcome)

  rr_components <- legacy_hia_resolved_rr_components(rr_sources) %>%
    dplyr::select("cause", "outcome")

  dplyr::bind_rows(crf_components, rr_components) %>%
    dplyr::distinct(.data$cause, .data$outcome) %>%
    dplyr::arrange(.data$cause, .data$outcome)
}

legacy_hia_epi_fixture <- function(crfs, rr_sources) {
  metrics <- legacy_hia_metric_components(crfs, rr_sources)
  estimates <- c("low", "central", "high")

  epi <- tibble::tibble(
    location_id = 1,
    estimate = estimates,
    pop = 100000,
    country = "Fingerprint country",
    iso3 = "FPR"
  )

  for (metric_i in seq_len(nrow(metrics))) {
    metric_name <- build_metric_key(
      metrics$cause[[metric_i]],
      metrics$outcome[[metric_i]]
    )
    central_value <- 1000 + metric_i * 100
    epi[[metric_name]] <- central_value * c(0.9, 1, 1.1)
  }

  epi
}

legacy_hia_epi_long_fixture <- function(rr_sources) {
  rr_components <- legacy_hia_resolved_rr_components(rr_sources)

  if (nrow(rr_components) == 0) {
    return(tibble::tibble(
      location_id = integer(),
      cause = character(),
      measure_name = character(),
      age = character(),
      estimate = character(),
      val = numeric()
    ))
  }

  purrr::map2_dfr(
    seq_len(nrow(rr_components)),
    split(rr_components, seq_len(nrow(rr_components))),
    function(component_i, component) {
      ages <- get_rr(component$source[[1]]) %>%
        dplyr::filter(.data$cause == component$cause[[1]]) %>%
        dplyr::distinct(.data$age) %>%
        dplyr::pull(.data$age) %>%
        deduplicate_adult_ages()

      tibble::tibble(
        location_id = 1L,
        cause = component$cause[[1]],
        measure_name = component$outcome[[1]],
        age = ages,
        estimate = "central",
        val = component_i * 100 + seq_along(ages)
      )
    }
  )
}

legacy_hia_fingerprint <- function(result, configuration_id) {
  result %>%
    dplyr::filter(.data$estimate == "central") %>%
    dplyr::transmute(
      configuration_id = configuration_id,
      scenario = .data$scenario,
      region_id = .data$region_id,
      pollutant = .data$pollutant,
      cause = .data$cause,
      outcome = .data$outcome,
      estimate = .data$estimate,
      age_group = .data$age_group,
      double_counted = .data$double_counted,
      number = .data$number
    ) %>%
    dplyr::arrange(
      .data$configuration_id,
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome
    )
}

read_legacy_actual_default_hia_fingerprint <- function() {
  readr::read_csv(
    testthat::test_path(
      "fixtures",
      "legacy_actual_default_hia_fingerprint.csv"
    ),
    show_col_types = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(
      .data$configuration_id,
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome
    )
}

read_legacy_crf_profiles_hia_fingerprint <- function() {
  readr::read_csv(
    testthat::test_path(
      "fixtures",
      "legacy_crf_profiles_hia_fingerprint.csv"
    ),
    show_col_types = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(
      .data$configuration_id,
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome
    )
}
