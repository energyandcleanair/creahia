make_dual_run_test_crfs <- function() {
  tibble::tibble(
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    double_counted = FALSE
  )
}


make_dual_run_test_epi <- function() {
  tibble::tibble(
    estimate = c("low", "central", "high"),
    NCD.LRI_Deaths = c(1, 2, 3)
  )
}


make_dual_run_test_impacts <- function() {
  tibble::tibble(
    scenario = "scenario1",
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    region_id = "BGD",
    number = 1
  )
}


run_dual_run_test_hia <- function(...) {
  compute_hia(
    conc_map = list(scenario1 = list(BGD = data.frame(pop = 100))),
    species = "no2",
    regions = data.frame(region_id = "BGD"),
    diagnostic_folder = NULL,
    ...
  )
}


make_dual_run_observation <- function(route = "registry") {
  compute_inputs <- list(
    crfs = structure(list(), class = c("creahia_crf_set", "list")),
    rr_sources = .empty_hia_rr_sources(),
    crf_compute = "registry"
  )

  list(
    status = "resolved",
    input_contract = "component_resolved|legacy_profile_c40|rr_sources|default",
    route_plan = .new_hia_compatibility_route_plan(
      status = "planned",
      route = route,
      reason = if (route == "registry") {
        "all_components_ready"
      } else {
        "components_not_ready"
      }
    ),
    route_application = structure(
      list(
        status = "applied",
        compute_inputs = compute_inputs,
        error = NULL
      ),
      class = c("creahia_hia_route_application_observation", "list")
    )
  )
}


test_that("dual run is opt-in and compares complete final impacts", {
  seen <- new.env(parent = emptyenv())
  seen$paf_routes <- character()
  seen$scale_calls <- 0L
  legacy_crfs <- make_dual_run_test_crfs()
  epi <- make_dual_run_test_epi()
  unscaled <- make_dual_run_test_impacts()
  scaled <- dplyr::mutate(unscaled, number = number * 2)
  old_options <- options(
    creahia.hia_readiness_shadow_observer = NULL,
    creahia.hia_dual_run_observer = function(result) {
      seen$result <- result
    }
  )
  on.exit(options(old_options), add = TRUE)

  expect_false(.hia_readiness_shadow_enabled())
  expect_true(.hia_dual_run_enabled())
  expect_true(.hia_compatibility_diagnostics_enabled())

  with_mocked_bindings(
    get_crfs = function(version = "default") legacy_crfs,
    get_epi = function(version = "default") epi,
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .observe_hia_compatibility_boundary = function(...) {
      make_dual_run_observation()
    },
    compute_hia_paf = function(..., crf_compute) {
      seen$paf_routes <- c(seen$paf_routes, crf_compute)
      tibble::tibble()
    },
    compute_hia_impacts = function(...) unscaled,
    get_pop_year = function(year_desired) 2020,
    scale_hia_pop = function(hia, base_year, target_year) {
      seen$scale_calls <- seen$scale_calls + 1L
      expect_equal(base_year, 2020)
      expect_equal(target_year, 2021)
      scaled
    },
    {
      result <- run_dual_run_test_hia(pop_year = 2021)
    }
  )

  expect_identical(result, scaled)
  expect_identical(seen$paf_routes, c("legacy", "registry"))
  expect_equal(seen$scale_calls, 2L)
  expect_s3_class(seen$result, "creahia_hia_dual_run_result")
  expect_equal(seen$result$status, "matched")
  expect_equal(seen$result$reason, "outputs_match")
  expect_equal(seen$result$legacy_rows, 1L)
  expect_equal(seen$result$registry_rows, 1L)
  expect_length(seen$result$differences, 0L)
})


test_that("dual run reports differences without changing legacy output", {
  seen <- new.env(parent = emptyenv())
  legacy <- make_dual_run_test_impacts()
  registry <- dplyr::mutate(legacy, number = number + 1)
  impact_calls <- 0L
  old_options <- options(
    creahia.hia_readiness_shadow_observer = NULL,
    creahia.hia_dual_run_observer = function(result) {
      seen$result <- result
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") make_dual_run_test_crfs(),
    get_epi = function(version = "default") make_dual_run_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .observe_hia_compatibility_boundary = function(...) {
      make_dual_run_observation()
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) {
      impact_calls <<- impact_calls + 1L
      if (impact_calls == 1L) legacy else registry
    },
    {
      result <- run_dual_run_test_hia()
    }
  )

  expect_identical(result, legacy)
  expect_equal(seen$result$status, "different")
  expect_equal(seen$result$reason, "outputs_differ")
  expect_true(length(seen$result$differences) > 0L)

  structural_difference <- .compare_hia_dual_run_results(
    legacy_impacts = legacy,
    registry_impacts = dplyr::select(legacy, number, dplyr::everything())
  )
  expect_equal(structural_difference$status, "different")
  expect_true(length(structural_difference$differences) > 0L)
})


test_that("dual run reports candidate errors and unavailable routes", {
  seen <- new.env(parent = emptyenv())
  old_options <- options(
    creahia.hia_dual_run_observer = function(result) {
      seen$result <- result
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    compute_hia_paf = function(...) stop("candidate failed"),
    {
      result <- .execute_hia_compatibility_dual_run(
        observation = make_dual_run_observation(),
        legacy_impacts = make_dual_run_test_impacts(),
        conc_map = list(),
        species = "no2",
        regions = data.frame(),
        scenarios = character(),
        epi_version = "default",
        epi = make_dual_run_test_epi()
      )
    }
  )

  expect_equal(result$status, "error")
  expect_equal(result$reason, "registry_candidate_error")
  expect_match(result$error, "candidate failed")

  unavailable <- .execute_hia_compatibility_dual_run(
    observation = make_dual_run_observation(route = "legacy"),
    legacy_impacts = make_dual_run_test_impacts(),
    conc_map = list(),
    species = "no2",
    regions = data.frame(),
    scenarios = character(),
    epi_version = "default",
    epi = make_dual_run_test_epi()
  )
  expect_equal(unavailable$status, "not_run")
  expect_equal(unavailable$reason, "components_not_ready")
})


test_that("dual run observer failures do not interrupt comparison", {
  old_options <- options(
    creahia.hia_dual_run_observer = function(result) stop("observer failed")
  )
  on.exit(options(old_options), add = TRUE)

  result <- .notify_hia_dual_run_observer(.new_hia_dual_run_result(
    status = "matched",
    reason = "outputs_match"
  ))

  expect_equal(result$status, "matched")
})
