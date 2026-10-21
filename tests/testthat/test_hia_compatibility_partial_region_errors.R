partial_region_fixture <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    legacy_crfs <- get_crfs("default")
    legacy_rr_sources <- parse_rr_sources("gemm")
    epi <- get_epi("gbd2021")
    observation <- .observe_hia_compatibility_boundary(
      crfs = legacy_crfs,
      rr_sources = legacy_rr_sources,
      epi = epi,
      context = list(
        rr_sources_supplied = TRUE,
        epi_supplied = TRUE,
        crfs_supplied = FALSE,
        epi_version = "gbd2021",
        crfs_version = "default",
        crf_compute = "legacy",
        rr_selection_mode = "rr_sources",
        calc_causes = NULL,
        gbd_causes = NULL
      ),
      population_year_intent = .capture_population_year_intent(
        pop_year = NULL,
        scale_base_year = NULL,
        scale_target_year = NULL,
        resolved_pop_year = NULL
      )
    )
    compute_inputs <- .activate_hia_compatibility_route(observation)

    cache <<- list(
      epi = epi,
      legacy_crfs = legacy_crfs,
      legacy_rr_sources = legacy_rr_sources,
      registry_crfs = compute_inputs$crfs,
      empty_rr_sources = compute_inputs$rr_sources,
      route_plan = observation$route_plan
    )
    cache
  }
})


partial_region_location_id <- function(region_id) {
  unname(c(IDN = 11, THA = 18, ZZZ = NA_real_)[region_id])
}


partial_region_concentrations <- function(
  tha_baseline = c(20, 30),
  tha_scenario = c(25, 35),
  scenario = "scen",
  second_region = "THA"
) {
  make_region <- function(baseline, scenario_values, pop) {
    data.frame(
      conc_baseline_pm25 = baseline,
      conc_scenario_pm25 = scenario_values,
      pop = pop
    )
  }

  stats::setNames(
    list(stats::setNames(
      list(
        make_region(c(20, 30), c(25, 35), c(1000, 2000)),
        make_region(tha_baseline, tha_scenario, c(3000, 4000))
      ),
      c("IDN", second_region)
    )),
    scenario
  )
}


capture_partial_region_warnings <- function(code) {
  observed_warnings <- character()
  value <- withCallingHandlers(
    force(code),
    warning = function(condition) {
      observed_warnings <<- c(
        observed_warnings,
        conditionMessage(condition)
      )
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = observed_warnings)
}


run_partial_region_paf <- function(route, conc_map) {
  fixture <- partial_region_fixture()
  route <- match.arg(route, c("registry", "legacy"))

  if (identical(route, "registry")) {
    crfs <- fixture$registry_crfs
    rr_sources <- fixture$empty_rr_sources
    crf_compute <- "registry"
  } else {
    crfs <- fixture$legacy_crfs
    rr_sources <- fixture$legacy_rr_sources
    crf_compute <- "legacy"
  }

  capture_partial_region_warnings(compute_hia_paf(
    conc_map = conc_map,
    species = "pm25",
    regions = NULL,
    scenarios = names(conc_map),
    epi_version = "gbd2021",
    rr_sources = rr_sources,
    crfs = crfs,
    diagnostic_folder = NULL,
    crf_compute = crf_compute
  ))
}


run_routed_partial_region_paf <- function(
  routing,
  conc_map,
  calc_causes = NULL,
  gbd_causes = NULL
) {
  fixture <- partial_region_fixture()
  original_compute_hia_paf <- compute_hia_paf
  captured <- NULL
  unexpected_error <- NULL
  old_options <- options(
    creahia.hia_compatibility_routing = routing,
    creahia.hia_dual_run_observer = NULL,
    creahia.hia_readiness_shadow_observer = NULL
  )
  on.exit(options(old_options), add = TRUE)

  result <- capture_partial_region_warnings(
    tryCatch(
      with_mocked_bindings(
        compute_hia_paf = function(...) {
          captured <<- original_compute_hia_paf(...)
          stop("PARTIAL_REGION_PAF_CAPTURED", call. = FALSE)
        },
        get_epi_location_id = partial_region_location_id,
        {
          compute_hia(
            conc_map = conc_map,
            species = "pm25",
            regions = NULL,
            scenarios = names(conc_map),
            epi_version = "gbd2021",
            epi = fixture$epi,
            rr_sources = "gemm",
            calc_causes = calc_causes,
            gbd_causes = gbd_causes,
            diagnostic_folder = NULL
          )
        },
        .package = "creahia"
      ),
      error = function(error) {
        if (!identical(conditionMessage(error), "PARTIAL_REGION_PAF_CAPTURED")) {
          unexpected_error <<- error
        }
        NULL
      }
    )
  )

  if (!is.null(unexpected_error)) {
    stop(unexpected_error)
  }

  list(value = captured, warnings = result$warnings)
}


expect_partial_region_parity <- function(registry, legacy) {
  expect_equal(
    arrange_historical_first_contract_paf(registry),
    arrange_historical_first_contract_paf(legacy),
    tolerance = 1e-12
  )
}


test_that("compatibility routing without RR components skips provenance checks", {
  fixture <- partial_region_fixture()
  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = "gemm",
    calc_causes = "none",
    gbd_causes = "default"
  )
  observation <- .observe_hia_compatibility_boundary(
    crfs = fixture$legacy_crfs,
    rr_sources = rr_selection$rr_sources,
    epi = fixture$epi,
    context = list(
      rr_sources_supplied = TRUE,
      epi_supplied = TRUE,
      crfs_supplied = FALSE,
      epi_version = "gbd2021",
      crfs_version = "default",
      crf_compute = "legacy",
      rr_selection_mode = rr_selection$mode,
      calc_causes = "none",
      gbd_causes = "default"
    ),
    population_year_intent = .capture_population_year_intent(
      pop_year = NULL,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = NULL
    )
  )
  compute_inputs <- .activate_hia_compatibility_route(observation)
  conc_map <- partial_region_concentrations()

  expect_false(observation$route_plan$rr_provenance_required)
  expect_silent(.apply_hia_compatibility_route_plan(
    route_plan = observation$route_plan,
    crfs = fixture$legacy_crfs,
    rr_sources = rr_selection$rr_sources,
    crf_compute = "legacy"
  ))
  expect_length(.hia_compatibility_rr_crf_ids(compute_inputs$crfs), 0L)

  routed <- run_routed_partial_region_paf(
    TRUE,
    conc_map,
    calc_causes = "none",
    gbd_causes = "default"
  )
  legacy <- run_routed_partial_region_paf(
    FALSE,
    conc_map,
    calc_causes = "none",
    gbd_causes = "default"
  )

  expect_partial_region_parity(routed$value, legacy$value)
  expect_true(nrow(routed$value) > 0)
})


test_that("compatibility routing isolates an out-of-range historical RR region", {
  conc_map <- partial_region_concentrations(
    tha_baseline = c(20, 3000),
    tha_scenario = c(25, 3005)
  )

  routed <- run_routed_partial_region_paf(TRUE, conc_map)
  legacy <- run_routed_partial_region_paf(FALSE, conc_map)

  expect_partial_region_parity(routed$value, legacy$value)
  expect_identical(names(routed$value), names(legacy$value))
  expect_identical(vapply(routed$value, typeof, character(1)),
                   vapply(legacy$value, typeof, character(1)))
  expect_true(any(grepl("^Failed for region .*THA", routed$warnings)))
  expect_true(any(grepl("^Failed for region .*THA", legacy$warnings)))

  fixture <- partial_region_fixture()
  rr_crf_ids <- .hia_compatibility_rr_crf_ids(fixture$registry_crfs)
  rr_slots <- fixture$registry_crfs %>%
    dplyr::filter(.data$crf_id %in% rr_crf_ids) %>%
    dplyr::distinct(.data$pollutant, .data$cause, .data$outcome)
  log_linear_slots <- fixture$registry_crfs %>%
    dplyr::filter(
      .data$pollutant == "PM25",
      !(.data$crf_id %in% rr_crf_ids)
    ) %>%
    dplyr::distinct(.data$pollutant, .data$cause, .data$outcome)
  tha_rows <- routed$value %>%
    dplyr::filter(.data$region_id == "THA")

  expect_gt(length(rr_crf_ids), 0L)
  expect_equal(nrow(dplyr::semi_join(tha_rows, rr_slots,
                                    by = c("pollutant", "cause", "outcome"))), 0L)
  expect_equal(nrow(dplyr::semi_join(tha_rows, log_linear_slots,
                                    by = c("pollutant", "cause", "outcome"))), 5L)
  expect_equal(nrow(tha_rows), 5L)
})


test_that("compatibility RR provenance is internal and explicit Sets remain unmarked", {
  fixture <- partial_region_fixture()
  rr_crf_ids <- .hia_compatibility_rr_crf_ids(fixture$registry_crfs)
  explicit_set <- crfs_set("gemm_adult_deaths_components")

  expect_setequal(
    rr_crf_ids,
    fixture$registry_crfs$crf_id[
      fixture$registry_crfs$form == CRF_FORM_TABULAR
    ]
  )
  expect_length(.hia_compatibility_rr_crf_ids(explicit_set), 0L)
  expect_null(attr(
    explicit_set,
    "creahia.hia_compatibility_rr_crf_ids",
    exact = TRUE
  ))
})


test_that("Registry route application rejects lost compatibility RR provenance", {
  fixture <- partial_region_fixture()
  route_plan <- fixture$route_plan
  attr(
    route_plan$registry_set,
    "creahia.hia_compatibility_rr_crf_ids"
  ) <- NULL

  expect_true(route_plan$rr_provenance_required)
  expect_error(
    .apply_hia_compatibility_route_plan(
      route_plan = route_plan,
      crfs = fixture$legacy_crfs,
      rr_sources = fixture$legacy_rr_sources,
      crf_compute = "legacy"
    ),
    "Call inspect_hia_configuration() again",
    fixed = TRUE
  )
})


test_that("compatibility routing isolates unmatching age weights by region", {
  conc_map <- partial_region_concentrations()
  original_registry_weights <- get_crf_age_weights
  original_legacy_weights <- get_age_weights

  with_mocked_bindings(
    get_crf_age_weights = function(region_id, cause, outcome, ages, epi_version) {
      if (identical(region_id, "THA")) {
        stop("Unmatching age weights.", call. = FALSE)
      }
      original_registry_weights(region_id, cause, outcome, ages, epi_version)
    },
    get_age_weights = function(region_id, cause, measure, rr_source, version) {
      if (identical(region_id, "THA")) {
        stop("Unmatching age weights.", call. = FALSE)
      }
      original_legacy_weights(region_id, cause, measure, rr_source, version)
    },
    get_epi_location_id = partial_region_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_true(any(grepl("^Failed for region .*THA.*Unmatching age weights",
                        registry$warnings)))
  expect_true(any(grepl("^Failed for region .*THA.*Unmatching age weights",
                        legacy$warnings)))
})


test_that("a later compatibility RR failure removes earlier rows for that region", {
  fixture <- partial_region_fixture()
  conc_map <- partial_region_concentrations()
  original_registry_weights <- get_crf_age_weights
  original_legacy_weights <- get_age_weights
  rr_crf_ids <- .hia_compatibility_rr_crf_ids(fixture$registry_crfs)
  rr_crfs <- fixture$registry_crfs %>%
    dplyr::filter(.data$crf_id %in% rr_crf_ids)
  rr_slots <- rr_crfs %>%
    dplyr::distinct(.data$pollutant, .data$cause, .data$outcome)
  log_linear_slots <- fixture$registry_crfs %>%
    dplyr::filter(
      .data$pollutant == "PM25",
      !(.data$crf_id %in% rr_crf_ids)
    ) %>%
    dplyr::distinct(.data$pollutant, .data$cause, .data$outcome)

  expect_gt(match("LC", rr_crfs$cause), 1L)

  with_mocked_bindings(
    get_crf_age_weights = function(region_id, cause, outcome, ages, epi_version) {
      if (identical(region_id, "THA") && identical(cause, "LC")) {
        stop("later RR-derived CRF failure", call. = FALSE)
      }
      original_registry_weights(region_id, cause, outcome, ages, epi_version)
    },
    get_age_weights = function(region_id, cause, measure, rr_source, version) {
      if (identical(region_id, "THA") && identical(cause, "LC")) {
        stop("later RR-derived CRF failure", call. = FALSE)
      }
      original_legacy_weights(region_id, cause, measure, rr_source, version)
    },
    get_epi_location_id = partial_region_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_equal(
    sum(grepl("^Failed for region .*THA", registry$warnings)),
    1L
  )
  expect_equal(
    sum(grepl("^Failed for region .*THA", legacy$warnings)),
    1L
  )

  tha_rows <- registry$value %>%
    dplyr::filter(.data$region_id == "THA")
  idn_rows <- registry$value %>%
    dplyr::filter(.data$region_id == "IDN")
  expect_equal(
    nrow(dplyr::semi_join(
      tha_rows,
      rr_slots,
      by = c("pollutant", "cause", "outcome")
    )),
    0L
  )
  expect_equal(
    nrow(dplyr::semi_join(
      tha_rows,
      log_linear_slots,
      by = c("pollutant", "cause", "outcome")
    )),
    nrow(log_linear_slots)
  )
  expect_equal(
    nrow(dplyr::semi_join(
      idn_rows,
      rr_slots,
      by = c("pollutant", "cause", "outcome")
    )),
    nrow(rr_slots)
  )
})


test_that("all regions may fail compatibility RR calculation without aborting", {
  conc_map <- partial_region_concentrations()

  with_mocked_bindings(
    get_crf_age_weights = function(...) {
      stop("all regions fail RR-derived CRFs", call. = FALSE)
    },
    get_age_weights = function(...) {
      stop("all regions fail RR-derived CRFs", call. = FALSE)
    },
    get_epi_location_id = partial_region_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_setequal(unique(registry$value$region_id), c("IDN", "THA"))
  expect_equal(
    sum(grepl("^Failed for region ", registry$warnings)),
    2L
  )
  expect_equal(
    sum(grepl("^Failed for region ", legacy$warnings)),
    2L
  )
})


test_that("compatibility routing isolates EPI lookup errors by region", {
  conc_map <- partial_region_concentrations()
  failing_location_id <- function(region_id) {
    if (identical(region_id, "THA")) {
      stop("injected EPI lookup error", call. = FALSE)
    }
    partial_region_location_id(region_id)
  }

  with_mocked_bindings(
    get_epi_location_id = failing_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_true(any(grepl("^Failed for region .*THA", registry$warnings)))
  expect_true(any(grepl("injected EPI lookup error", registry$warnings)))
  expect_true(any(grepl("^Failed for region .*THA", legacy$warnings)))
})


test_that("compatibility routing isolates PAF helper errors by region", {
  conc_map <- partial_region_concentrations()
  original_paf_helper <- get_paf_from_rr_lauri

  with_mocked_bindings(
    get_paf_from_rr_lauri = function(...) {
      args <- list(...)
      if (identical(unname(args$pop), c(3000, 4000))) {
        stop("injected PAF helper error", call. = FALSE)
      }
      do.call(original_paf_helper, args)
    },
    get_epi_location_id = partial_region_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_true(any(grepl("^Failed for region .*THA", registry$warnings)))
  expect_true(any(grepl("injected PAF helper error", registry$warnings)))
  expect_true(any(grepl("^Failed for region .*THA", legacy$warnings)))
})


test_that("an unknown EPI region warns without aborting either route", {
  conc_map <- partial_region_concentrations(second_region = "ZZZ")

  with_mocked_bindings(
    get_epi_location_id = partial_region_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_true(any(grepl("No age weights found for ZZZ", registry$warnings)))
  expect_true(any(grepl("No age weights found for ZZZ", legacy$warnings)))
  expect_false(any(grepl("^Failed for region .*ZZZ", registry$warnings)))
  expect_false(any(grepl("^Failed for region .*ZZZ", legacy$warnings)))
})


test_that("compatibility RR failures are isolated within each scenario", {
  good <- partial_region_concentrations(scenario = "good")
  bad <- partial_region_concentrations(
    tha_baseline = c(20, 3000),
    tha_scenario = c(25, 3005),
    scenario = "bad"
  )
  conc_map <- c(bad, good)

  with_mocked_bindings(
    get_epi_location_id = partial_region_location_id,
    {
      registry <- run_partial_region_paf("registry", conc_map)
      legacy <- run_partial_region_paf("legacy", conc_map)
    },
    .package = "creahia"
  )

  expect_partial_region_parity(registry$value, legacy$value)
  expect_setequal(unique(registry$value$scenario), c("bad", "good"))
  expect_gt(
    nrow(dplyr::filter(registry$value,
                       .data$scenario == "good", .data$region_id == "THA")),
    nrow(dplyr::filter(registry$value,
                       .data$scenario == "bad", .data$region_id == "THA"))
  )
  expect_equal(sum(grepl("^Failed for region .*THA", registry$warnings)), 1L)
})


test_that("explicit crfs_set remains fail-fast pending maintainer policy decision", {
  # This intentionally preserves current explicit Registry behavior until the
  # maintainer decides whether explicit Sets should also isolate region errors.
  explicit_set <- crfs_set("gemm_adult_deaths_components")
  conc_map <- partial_region_concentrations(
    tha_baseline = c(20, 3000),
    tha_scenario = c(25, 3005)
  )

  with_mocked_bindings(
    get_epi_location_id = partial_region_location_id,
    {
      expect_error(
        compute_hia_paf(
          conc_map = conc_map,
          species = "pm25",
          regions = NULL,
          scenarios = names(conc_map),
          epi_version = "gbd2021",
          rr_sources = tibble::tibble(cause = character(), source = character()),
          crfs = explicit_set,
          diagnostic_folder = NULL
        ),
        "Exposure out of CRF range",
        fixed = TRUE
      )
    },
    .package = "creahia"
  )
})


test_that("compatibility log-linear CRF errors remain fail-fast", {
  fixture <- partial_region_fixture()
  original_apply_crf <- apply_crf
  conc_map <- partial_region_concentrations()

  with_mocked_bindings(
    apply_crf = function(crf, ..., region_id) {
      if (
        identical(crf$form, CRF_FORM_LOG_LINEAR) &&
        identical(region_id, "THA")
      ) {
        stop("injected log-linear error", call. = FALSE)
      }
      original_apply_crf(crf = crf, ..., region_id = region_id)
    },
    get_epi_location_id = partial_region_location_id,
    {
      expect_error(
        compute_hia_paf(
          conc_map = conc_map,
          species = "pm25",
          regions = NULL,
          scenarios = names(conc_map),
          epi_version = "gbd2021",
          rr_sources = fixture$empty_rr_sources,
          crfs = fixture$registry_crfs,
          diagnostic_folder = NULL
        ),
        "injected log-linear error",
        fixed = TRUE
      )
    },
    .package = "creahia"
  )
})
