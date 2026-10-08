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

legacy_hia_epi_version_multiplier <- function(epi_version) {
  multipliers <- c(
    default = 1,
    gbd2019 = 1,
    gbd2021 = 1.2,
    gbd2023 = 1.4
  )

  multiplier <- unname(multipliers[[epi_version]])
  if (is.null(multiplier)) {
    stop("Unknown fingerprint EPI version: ", epi_version, call. = FALSE)
  }

  multiplier
}

legacy_hia_epi_fixture <- function(crfs, rr_sources, epi_version = "default") {
  metrics <- legacy_hia_metric_components(crfs, rr_sources)
  estimates <- c("low", "central", "high")
  epi_multiplier <- legacy_hia_epi_version_multiplier(epi_version)

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
    central_value <- (1000 + metric_i * 100) * epi_multiplier
    epi[[metric_name]] <- central_value * c(0.9, 1, 1.1)
  }

  epi
}

legacy_hia_epi_long_fixture <- function(rr_sources, epi_version = "default") {
  rr_components <- legacy_hia_resolved_rr_components(rr_sources)
  epi_multiplier <- legacy_hia_epi_version_multiplier(epi_version)

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
        val = (component_i * 100 + seq_along(ages)) * epi_multiplier
      )
    }
  )
}

legacy_hia_configuration_value <- function(configuration, column) {
  configuration[[column]][[1]]
}

legacy_hia_configuration_rr_sources <- function(configuration) {
  calc_causes <- legacy_hia_configuration_value(
    configuration,
    "legacy_calc_causes"
  )

  if (!is.na(calc_causes)) {
    return(convert_old_parameters_to_rr_sources(
      calc_causes = calc_causes,
      gbd_causes = legacy_hia_configuration_value(
        configuration,
        "legacy_gbd_causes"
      )
    ))
  }

  legacy_hia_configuration_value(configuration, "legacy_rr_sources")
}

legacy_hia_configuration_compute_args <- function(configuration) {
  args <- list(
    conc_map = legacy_default_fingerprint_conc_map(),
    species = c("pm25", "no2", "o3_8h", "so2"),
    regions = legacy_hia_fingerprint_regions(),
    epi_version = legacy_hia_configuration_value(
      configuration,
      "epi_version"
    ),
    crfs_version = legacy_hia_configuration_value(
      configuration,
      "legacy_crfs_version"
    ),
    pop_year = legacy_hia_configuration_value(configuration, "pop_year"),
    diagnostic_folder = NULL,
    crf_compute = "legacy"
  )

  calc_causes <- legacy_hia_configuration_value(
    configuration,
    "legacy_calc_causes"
  )

  if (!is.na(calc_causes)) {
    args$calc_causes <- calc_causes
    args$gbd_causes <- legacy_hia_configuration_value(
      configuration,
      "legacy_gbd_causes"
    )
  } else {
    args$rr_sources <- legacy_hia_configuration_value(
      configuration,
      "legacy_rr_sources"
    )
  }

  args
}

legacy_hia_population_projection_fixture <- function() {
  tibble::tibble(
    iso3 = "FPR",
    year = c(2020L, 2021L, 2023L),
    pop = c(100, 110, 130)
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

read_legacy_historical_configurations_hia_fingerprint <- function() {
  readr::read_csv(
    testthat::test_path(
      "fixtures",
      "legacy_historical_configurations_hia_fingerprint.csv"
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
