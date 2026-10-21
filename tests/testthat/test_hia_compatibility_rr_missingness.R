rr_missingness_fixture <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- list(
        epi = get_epi("default"),
        epi_long = get_epi_count_long("default"),
        regions = tibble::tibble(
          region_id = c("IDN", "THA"),
          region_name = c("Indonesia", "Thailand"),
          country_id = c("IDN", "THA")
        )
      )
    }
    cache
  }
})


rr_missingness_conc_map <- function(case) {
  case <- match.arg(case, c(
    "other_pollutant_missing",
    "complete_case_reference",
    "pm25_missing",
    "pm25_all_missing"
  ))

  values <- switch(
    case,
    other_pollutant_missing = data.frame(
      conc_baseline_pm25 = c(20, 30),
      conc_scenario_pm25 = c(25, 35),
      conc_baseline_no2 = c(NA, 5),
      conc_scenario_no2 = c(NA, 6),
      pop = c(1000, 2000)
    ),
    complete_case_reference = data.frame(
      conc_baseline_pm25 = 30,
      conc_scenario_pm25 = 35,
      conc_baseline_no2 = 5,
      conc_scenario_no2 = 6,
      pop = 2000
    ),
    pm25_missing = data.frame(
      conc_baseline_pm25 = c(NA, 30),
      conc_scenario_pm25 = c(NA, 35),
      conc_baseline_no2 = c(4, 5),
      conc_scenario_no2 = c(5, 6),
      pop = c(1000, 2000)
    ),
    pm25_all_missing = NULL
  )

  if (identical(case, "pm25_all_missing")) {
    return(list(scen = list(
      IDN = data.frame(
        conc_baseline_pm25 = c(NA, NA),
        conc_scenario_pm25 = c(NA, NA),
        conc_baseline_no2 = c(4, 5),
        conc_scenario_no2 = c(5, 6),
        pop = c(1000, 2000)
      ),
      THA = data.frame(
        conc_baseline_pm25 = c(20, 30),
        conc_scenario_pm25 = c(25, 35),
        conc_baseline_no2 = c(4, 5),
        conc_scenario_no2 = c(5, 6),
        pop = c(1000, 2000)
      )
    )))
  }

  list(scen = list(IDN = values))
}


capture_rr_missingness_warnings <- function(code) {
  observed_warnings <- character()
  value <- withCallingHandlers(
    force(code),
    warning = function(condition) {
      observed_warnings <<- c(observed_warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = observed_warnings)
}


run_rr_missingness_route <- function(case, routing, capture_paf = FALSE) {
  fixture <- rr_missingness_fixture()
  conc_map <- rr_missingness_conc_map(case)
  original_compute_hia_paf <- compute_hia_paf
  captured_paf <- NULL
  captured_inputs <- NULL
  unexpected_error <- NULL
  sentinel <- "RR_MISSINGNESS_PAF_CAPTURED"
  old_options <- options(
    creahia.hia_compatibility_routing = routing,
    creahia.hia_dual_run_observer = NULL,
    creahia.hia_readiness_shadow_observer = NULL
  )
  on.exit(options(old_options), add = TRUE)

  execute_hia <- function() {
    compute_hia(
      conc_map = conc_map,
      species = c("pm25", "no2"),
      regions = fixture$regions,
      diagnostic_folder = NULL
    )
  }

  result <- capture_rr_missingness_warnings(
    tryCatch(
      if (capture_paf) {
        testthat::with_mocked_bindings(
          get_epi = function(...) fixture$epi,
          get_epi_count_long = function(...) fixture$epi_long,
          compute_hia_paf = function(...) {
            captured_inputs <<- list(...)
            captured_paf <<- do.call(original_compute_hia_paf, captured_inputs)
            stop(sentinel, call. = FALSE)
          },
          execute_hia(),
          .package = "creahia"
        )
      } else {
        testthat::with_mocked_bindings(
          get_epi = function(...) fixture$epi,
          get_epi_count_long = function(...) fixture$epi_long,
          execute_hia(),
          .package = "creahia"
        )
      },
      error = function(error) {
        if (!capture_paf || !identical(conditionMessage(error), sentinel)) {
          unexpected_error <<- error
        }
        NULL
      }
    )
  )

  if (!is.null(unexpected_error)) {
    stop(unexpected_error)
  }
  if (capture_paf) {
    result$value <- captured_paf
    result$compute_inputs <- captured_inputs
  }
  result
}


arrange_rr_missingness_paf <- function(paf) {
  paf %>%
    dplyr::arrange(
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome
    )
}


arrange_rr_missingness_impacts <- function(impacts) {
  impacts %>%
    dplyr::arrange(
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome,
      .data$estimate
    )
}


missing_concentration_warnings <- function(warnings) {
  warnings[grepl(
    "missing values in concentration or population data",
    warnings,
    fixed = TRUE
  )]
}


compute_rr_missingness_diagnostic_set <- function(crfs, case) {
  fixture <- rr_missingness_fixture()

  testthat::with_mocked_bindings(
    get_epi_count_long = function(...) fixture$epi_long,
    compute_hia_paf(
      conc_map = rr_missingness_conc_map(case),
      species = c("pm25", "no2"),
      regions = fixture$regions,
      rr_sources = character(),
      crfs = crfs,
      diagnostic_folder = NULL,
      crf_compute = "registry"
    ),
    .package = "creahia"
  )
}


test_that("compatibility RR missingness matches the legacy RR route", {
  registry <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )
  legacy <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = FALSE,
    capture_paf = TRUE
  )

  expect_equal(
    arrange_rr_missingness_paf(registry$value),
    arrange_rr_missingness_paf(legacy$value),
    tolerance = 1e-12
  )
  expect_identical(names(registry$value), names(legacy$value))
  expect_identical(
    vapply(registry$value, typeof, character(1)),
    vapply(legacy$value, typeof, character(1))
  )
  registry_missing_warnings <- missing_concentration_warnings(registry$warnings)
  legacy_missing_warnings <- missing_concentration_warnings(legacy$warnings)
  expect_identical(registry_missing_warnings, legacy_missing_warnings)
  expect_length(registry_missing_warnings, 1L)

  expect_identical(registry$compute_inputs$crf_compute, "registry")
  expect_false(has_rr_sources(registry$compute_inputs$rr_sources))
  registry_rr_ids <- .hia_compatibility_rr_crf_ids(
    registry$compute_inputs$crfs
  )
  expect_gt(length(registry_rr_ids), 0L)
  registry_ihd_rr <- registry$compute_inputs$crfs %>%
    dplyr::filter(
      .data$crf_id %in% registry_rr_ids,
      .data$pollutant == "PM25",
      .data$cause == "IHD",
      .data$outcome %in% c("Deaths", "YLLs")
    )
  expect_setequal(registry_ihd_rr$outcome, c("Deaths", "YLLs"))

  expect_identical(legacy$compute_inputs$crf_compute, "legacy")
  expect_true(has_rr_sources(legacy$compute_inputs$rr_sources))

  ihd <- registry$value %>%
    dplyr::filter(
      .data$pollutant == "PM25",
      .data$cause == "IHD",
      .data$outcome %in% c("Deaths", "YLLs")
    )
  expect_setequal(ihd$outcome, c("Deaths", "YLLs"))
  expect_true(all(!is.na(ihd$central)))
})


test_that("full compute routing preserves compatibility RR provenance", {
  registry <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )
  routed_crfs <- registry$compute_inputs$crfs
  rr_crf_ids <- attr(
    routed_crfs,
    "creahia.hia_compatibility_rr_crf_ids",
    exact = TRUE
  )

  expect_s3_class(routed_crfs, "creahia_hia_diagnostic_set")
  expect_type(rr_crf_ids, "character")
  expect_gt(length(rr_crf_ids), 0L)
  expect_identical(rr_crf_ids, .hia_compatibility_rr_crf_ids(routed_crfs))
  expect_true(all(rr_crf_ids %in% routed_crfs$crf_id))
})


test_that("dplyr-edited diagnostic Sets retain usable RR provenance", {
  registry_crfs <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )$compute_inputs$crfs
  baseline <- suppressWarnings(compute_rr_missingness_diagnostic_set(
    registry_crfs,
    "other_pollutant_missing"
  ))

  filtered_crfs <- registry_crfs %>%
    dplyr::filter(!(.data$pollutant == "PM25" & .data$cause == "IHD"))
  filtered <- suppressWarnings(compute_rr_missingness_diagnostic_set(
    filtered_crfs,
    "other_pollutant_missing"
  ))
  expected_filtered <- baseline %>%
    dplyr::filter(!(.data$pollutant == "PM25" & .data$cause == "IHD"))

  expect_equal(
    arrange_rr_missingness_paf(filtered),
    arrange_rr_missingness_paf(expected_filtered),
    tolerance = 1e-12
  )

  mutated_crfs <- dplyr::mutate(registry_crfs, inspection_note = "retained")
  mutated <- suppressWarnings(compute_rr_missingness_diagnostic_set(
    mutated_crfs,
    "other_pollutant_missing"
  ))
  expect_equal(
    arrange_rr_missingness_paf(mutated),
    arrange_rr_missingness_paf(baseline),
    tolerance = 1e-12
  )

  no_rr_crfs <- dplyr::filter(registry_crfs, .data$pollutant == "NO2")
  no_rr <- suppressWarnings(compute_rr_missingness_diagnostic_set(
    no_rr_crfs,
    "other_pollutant_missing"
  ))
  expected_no_rr <- baseline %>% dplyr::filter(.data$pollutant == "NO2")
  expect_equal(
    arrange_rr_missingness_paf(no_rr),
    arrange_rr_missingness_paf(expected_no_rr),
    tolerance = 1e-12
  )
})


test_that("missing compatibility RR provenance fails instead of changing rules", {
  routed_crfs <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )$compute_inputs$crfs
  attr(routed_crfs, "creahia.hia_compatibility_rr_crf_ids") <- NULL

  expect_error(
    .hia_compatibility_rr_crf_ids(routed_crfs),
    "Call inspect_hia_configuration() again",
    fixed = TRUE
  )
})


test_that("malformed compatibility RR provenance fails clearly", {
  routed_crfs <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )$compute_inputs$crfs
  rr_crf_ids <- .hia_compatibility_rr_crf_ids(routed_crfs)

  attr(routed_crfs, "creahia.hia_compatibility_rr_crf_ids") <- c(
    rr_crf_ids,
    NA_character_
  )
  expect_error(
    .hia_compatibility_rr_crf_ids(routed_crfs),
    "Call inspect_hia_configuration() again",
    fixed = TRUE
  )

  attr(routed_crfs, "creahia.hia_compatibility_rr_crf_ids") <- c(
    rr_crf_ids,
    rr_crf_ids[[1]]
  )
  expect_error(
    .hia_compatibility_rr_crf_ids(routed_crfs),
    "Call inspect_hia_configuration() again",
    fixed = TRUE
  )
})


test_that("NO2 log-linear CRFs retain the whole-table complete-case rule", {
  mixed_registry <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )$value
  mixed_legacy <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = FALSE,
    capture_paf = TRUE
  )$value
  reference_registry <- run_rr_missingness_route(
    "complete_case_reference",
    routing = TRUE,
    capture_paf = TRUE
  )$value
  reference_legacy <- run_rr_missingness_route(
    "complete_case_reference",
    routing = FALSE,
    capture_paf = TRUE
  )$value

  select_no2 <- function(paf) {
    paf %>%
      dplyr::filter(.data$pollutant == "NO2") %>%
      arrange_rr_missingness_paf()
  }

  expect_equal(
    select_no2(mixed_registry),
    select_no2(reference_registry),
    tolerance = 1e-12
  )
  expect_equal(
    select_no2(mixed_legacy),
    select_no2(reference_legacy),
    tolerance = 1e-12
  )
})


compute_explicit_set_missingness_paf <- function(case, explicit_set) {
  fixture <- rr_missingness_fixture()

  suppressWarnings(testthat::with_mocked_bindings(
    get_epi_count_long = function(...) fixture$epi_long,
    compute_hia_paf(
      conc_map = rr_missingness_conc_map(case),
      species = c("pm25", "no2"),
      regions = fixture$regions,
      rr_sources = character(),
      crfs = explicit_set,
      diagnostic_folder = NULL,
      crf_compute = "registry"
    ),
    .package = "creahia"
  ))
}


test_that("explicit Set tabular CRFs use only their pollutant's columns", {
  explicit_set <- crfs_set(presets = "gemm_adult_deaths_components")
  select_pm25 <- function(paf) {
    paf %>%
      dplyr::filter(.data$pollutant == "PM25") %>%
      arrange_rr_missingness_paf()
  }

  explicit <- compute_explicit_set_missingness_paf(
    "other_pollutant_missing",
    explicit_set
  )
  routed <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )$value
  whole_table <- compute_explicit_set_missingness_paf(
    "complete_case_reference",
    explicit_set
  )

  # The same GEMM curves give the same PAF through an explicit Set and through
  # compatibility routing, rather than dropping cells where only NO2 is missing.
  explicit_rows <- select_pm25(explicit)
  routed_rows <- select_pm25(routed) %>%
    dplyr::semi_join(
      explicit_rows,
      by = c("pollutant", "cause", "outcome", "region_id")
    ) %>%
    dplyr::select(dplyr::all_of(names(explicit_rows)))
  expect_equal(nrow(explicit_rows), 5L)
  expect_equal(explicit_rows, routed_rows, tolerance = 1e-12)

  ihd_deaths <- function(paf) {
    paf %>%
      dplyr::filter(.data$cause == "IHD", .data$outcome == "Deaths") %>%
      dplyr::pull("central")
  }
  expect_false(isTRUE(all.equal(
    ihd_deaths(explicit_rows),
    ihd_deaths(select_pm25(whole_table))
  )))
})


test_that("explicit Set log-linear CRFs keep the whole-table complete-case rule", {
  explicit_set <- crfs_set(
    presets = "gemm_adult_deaths_components",
    add = list(list(crf_id = "legacy_no2_asthma_1to18_incidence_v1"))
  )
  select_no2 <- function(paf) {
    paf %>%
      dplyr::filter(.data$pollutant == "NO2") %>%
      arrange_rr_missingness_paf()
  }

  # pm25_missing drops its first row from the whole table; its second row is
  # complete_case_reference.
  mixed <- compute_explicit_set_missingness_paf("pm25_missing", explicit_set)
  reference <- compute_explicit_set_missingness_paf(
    "complete_case_reference",
    explicit_set
  )

  expect_equal(nrow(select_no2(mixed)), 1L)
  expect_true(select_no2(mixed)$central != 0)
  expect_equal(select_no2(mixed), select_no2(reference), tolerance = 1e-12)
})


test_that("mixed missingness PM2.5 PAF matches fixed values", {
  routed <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE,
    capture_paf = TRUE
  )$value

  # Fixed values from the established legacy RR route (routing = FALSE) with
  # the packaged default EPI. Live parity cannot detect both routes drifting
  # together; do not update these values without explaining the change.
  observed <- routed %>%
    dplyr::filter(
      .data$pollutant == "PM25",
      .data$cause == "IHD",
      .data$outcome %in% c("Deaths", "YLLs")
    ) %>%
    dplyr::arrange(.data$outcome) %>%
    dplyr::select(outcome, low, central, high)

  expect_equal(
    as.data.frame(observed),
    data.frame(
      outcome = c("Deaths", "YLLs"),
      low = c(0.0462333536229005, 0.0533990821100956),
      central = c(0.0513365714490304, 0.0593136453905775),
      high = c(0.0564673488156938, 0.0652641776583348)
    ),
    tolerance = 1e-10
  )
})


test_that("PM2.5 missingness preserves full PAF route parity", {
  registry <- run_rr_missingness_route(
    "pm25_missing",
    routing = TRUE,
    capture_paf = TRUE
  )
  legacy <- run_rr_missingness_route(
    "pm25_missing",
    routing = FALSE,
    capture_paf = TRUE
  )

  expect_equal(
    arrange_rr_missingness_paf(registry$value),
    arrange_rr_missingness_paf(legacy$value),
    tolerance = 1e-12
  )
  registry_missing_warnings <- missing_concentration_warnings(registry$warnings)
  legacy_missing_warnings <- missing_concentration_warnings(legacy$warnings)
  expect_identical(registry_missing_warnings, legacy_missing_warnings)
  expect_length(registry_missing_warnings, 1L)
})


test_that("an all-missing PM2.5 region is omitted on both routes", {
  registry <- run_rr_missingness_route(
    "pm25_all_missing",
    routing = TRUE,
    capture_paf = TRUE
  )
  legacy <- run_rr_missingness_route(
    "pm25_all_missing",
    routing = FALSE,
    capture_paf = TRUE
  )

  expect_equal(
    arrange_rr_missingness_paf(registry$value),
    arrange_rr_missingness_paf(legacy$value),
    tolerance = 1e-12
  )
  registry_pm25 <- registry$value %>%
    dplyr::filter(.data$pollutant == "PM25")
  legacy_pm25 <- legacy$value %>%
    dplyr::filter(.data$pollutant == "PM25")
  expect_true(any(registry_pm25$region_id == "THA"))
  expect_true(any(legacy_pm25$region_id == "THA"))
  expect_false(any(registry_pm25$region_id == "IDN"))
  expect_false(any(legacy_pm25$region_id == "IDN"))
  expect_false(any(grepl("Failed for region", registry$warnings, fixed = TRUE)))
  expect_false(any(grepl("Failed for region", legacy$warnings, fixed = TRUE)))
  registry_missing_warnings <- missing_concentration_warnings(registry$warnings)
  legacy_missing_warnings <- missing_concentration_warnings(legacy$warnings)
  expect_identical(registry_missing_warnings, legacy_missing_warnings)
  expect_length(registry_missing_warnings, 1L)
})


test_that("mixed missingness preserves end-to-end impact parity", {
  registry <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = TRUE
  )
  legacy <- run_rr_missingness_route(
    "other_pollutant_missing",
    routing = FALSE
  )

  expect_equal(
    arrange_rr_missingness_impacts(registry$value),
    arrange_rr_missingness_impacts(legacy$value),
    tolerance = 1e-12
  )
  expect_identical(names(registry$value), names(legacy$value))
  expect_identical(
    vapply(registry$value, typeof, character(1)),
    vapply(legacy$value, typeof, character(1))
  )
  registry_missing_warnings <- missing_concentration_warnings(registry$warnings)
  legacy_missing_warnings <- missing_concentration_warnings(legacy$warnings)
  expect_identical(registry_missing_warnings, legacy_missing_warnings)
  expect_length(registry_missing_warnings, 2L)
})
