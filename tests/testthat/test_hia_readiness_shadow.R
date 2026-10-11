make_shadow_test_crfs <- function() {
  tibble::tibble(
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    double_counted = FALSE
  )
}


make_shadow_test_epi <- function() {
  tibble::tibble(
    estimate = c("low", "central", "high"),
    NCD.LRI_Deaths = c(1, 2, 3)
  )
}


make_shadow_test_impacts <- function() {
  tibble::tibble(
    scenario = "scenario1",
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    region_id = "BGD",
    number = 1
  )
}


run_shadow_test_hia <- function(...) {
  compute_hia(
    conc_map = list(scenario1 = list(BGD = data.frame(pop = 100))),
    species = "no2",
    regions = data.frame(region_id = "BGD"),
    diagnostic_folder = NULL,
    ...
  )
}


make_shadow_test_boundary_context <- function(...) {
  defaults <- list(
    rr_sources_supplied = FALSE,
    epi_supplied = FALSE,
    crfs_supplied = FALSE,
    epi_version = "default",
    crfs_version = "default",
    crf_compute = "legacy",
    rr_selection_mode = "rr_sources",
    calc_causes = NULL,
    gbd_causes = NULL
  )
  utils::modifyList(defaults, list(...))
}


test_that("actual default HIA call observes readiness without changing output", {
  seen <- new.env(parent = emptyenv())
  expected <- make_shadow_test_impacts()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      seen$observation <- observation
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") make_shadow_test_crfs(),
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(
      crfs,
      rr_sources,
      epi,
      crf_input_mode,
      crf_selection_id,
      epi_version
    ) {
      seen$crfs <- crfs
      seen$rr_sources <- rr_sources
      seen$epi <- epi
      seen$crf_input_mode <- crf_input_mode
      seen$crf_selection_id <- crf_selection_id
      seen$epi_version <- epi_version
      readiness <- make_specification_readiness()
      readiness$components <- readiness$components[0, ]
      readiness$blocking_components <- readiness$blocking_components[0, ]
      readiness
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia()
    }
  )

  expect_identical(result, expected)
  expect_equal(seen$observation$status, "resolved")
  expect_false(seen$observation$result$ready)
  expect_s3_class(
    seen$observation$specification,
    "creahia_hia_specification"
  )
  expect_null(
    seen$observation$specification$population_year$resolved_pop_year
  )
  expect_identical(seen$crfs, make_shadow_test_crfs())
  expect_identical(seen$epi, make_shadow_test_epi())
  expect_equal(seen$rr_sources$source, RR_GEMM)
  expect_equal(seen$crf_input_mode, "legacy_profile")
  expect_equal(seen$crf_selection_id, "legacy_profile_default")
  expect_equal(seen$epi_version, "default")
})


test_that("shadow specification records resolved population-year intent", {
  seen <- new.env(parent = emptyenv())
  expected <- make_shadow_test_impacts()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      seen$observation <- observation
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") make_shadow_test_crfs(),
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(...) {
      readiness <- make_specification_readiness()
      readiness$components <- readiness$components[0, ]
      readiness$blocking_components <- readiness$blocking_components[0, ]
      readiness
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    get_pop_year = function(year_desired) year_desired,
    {
      result <- run_shadow_test_hia(pop_year = 2030)
    }
  )

  population_year <- seen$observation$specification$population_year
  expect_identical(result, expected)
  expect_equal(population_year$supplied$pop_year, 2030)
  expect_equal(population_year$resolved_pop_year, 2030)
  expect_true(population_year$scaling_requested)
})


test_that("GBD 2019 historical call emits its canonical specification", {
  seen <- new.env(parent = emptyenv())
  expected <- make_shadow_test_impacts()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      seen$observation <- observation
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") make_shadow_test_crfs(),
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      seen$translated_rr_sources <- rr_sources
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(..., epi_version) {
      seen$readiness_epi_version <- epi_version
      readiness <- make_specification_readiness()
      readiness$components <- readiness$components[0, ]
      readiness$blocking_components <- readiness$blocking_components[0, ]
      readiness
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    get_pop_year = function(year_desired) year_desired,
    {
      result <- run_shadow_test_hia(
        calc_causes = "GEMM and GBD",
        epi_version = "gbd2019",
        pop_year = 2020
      )
    }
  )

  specification <- seen$observation$specification
  expect_identical(result, expected)
  expect_equal(seen$observation$status, "resolved")
  expect_equal(
    specification$input_contract,
    "legacy_default_gemm_gbd2019_2020"
  )
  expect_equal(specification$epi_version, "gbd2019")
  expect_equal(specification$population_year$supplied$pop_year, 2020)
  expect_equal(specification$population_year$resolved_pop_year, 2020)
  expect_equal(seen$readiness_epi_version, "gbd2019")
  expect_identical(
    seen$translated_rr_sources,
    convert_old_parameters_to_rr_sources("GEMM and GBD", NULL)
  )
})


test_that("shadow readiness failures do not interrupt the legacy calculation", {
  seen <- new.env(parent = emptyenv())
  expected <- make_shadow_test_impacts()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      seen$observation <- observation
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") make_shadow_test_crfs(),
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(...) {
      stop("shadow resolver failed")
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia()
    }
  )

  expect_identical(result, expected)
  expect_equal(seen$observation$status, "error")
  expect_match(seen$observation$error, "shadow resolver failed")
})


test_that("shadow observer failures stay internal", {
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      stop("observer failed")
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) {
      readiness <- make_specification_readiness()
      readiness$components <- readiness$components[0, ]
      readiness$blocking_components <- readiness$blocking_components[0, ]
      readiness
    },
    {
      observation <- .observe_hia_compatibility_boundary(
        crfs = make_shadow_test_crfs(),
        rr_sources = tibble::tibble(
          cause = "NCD.LRI",
          source = RR_GEMM
        ),
        epi = make_shadow_test_epi(),
        context = make_shadow_test_boundary_context(),
        population_year_intent = .capture_population_year_intent(
          pop_year = NULL,
          scale_base_year = NULL,
          scale_target_year = NULL,
          resolved_pop_year = NULL
        )
      )
    }
  )

  expect_equal(observation$status, "resolved")
  expect_false(observation$result$ready)
  expect_s3_class(
    observation$specification,
    "creahia_hia_specification"
  )
})


test_that("shadow mode resolves the current package default contract", {
  observation <- .observe_hia_compatibility_boundary(
    crfs = get_crfs("default"),
    rr_sources = parse_rr_sources(c(RR_GEMM, RR_ORIGINAL)),
    epi = get_epi("default"),
    context = make_shadow_test_boundary_context(),
    population_year_intent = .capture_population_year_intent(
      pop_year = NULL,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = NULL
    )
  )
  component_counts <- observation$result$components %>%
    dplyr::count(component_type)

  expect_equal(observation$status, "resolved")
  expect_false(observation$result$ready)
  expect_equal(
    component_counts$n[component_counts$component_type == "crf"],
    15
  )
  expect_equal(
    component_counts$n[component_counts$component_type == "rr"],
    18
  )
  expect_equal(
    component_counts$n[component_counts$component_type == "epi"],
    1
  )
  expect_equal(nrow(observation$result$blocking_components), 18)
  expect_equal(nrow(observation$result$ignored_rr_pairs), 0)
  expect_equal(
    nrow(observation$specification$components$crf),
    15
  )
  expect_equal(
    nrow(observation$specification$components$rr),
    18
  )
  expect_false(
    observation$specification$population_year$scaling_requested
  )
})


test_that("GBD 2019 historical contract exposes its complete readiness gap", {
  rr_selection <- .resolve_hia_rr_selection_boundary(
    rr_sources = c(RR_GEMM, RR_ORIGINAL),
    calc_causes = "GEMM and GBD",
    gbd_causes = NULL
  )
  observation <- .observe_hia_compatibility_boundary(
    crfs = get_crfs("default"),
    rr_sources = parse_rr_sources(rr_selection$rr_sources),
    epi = get_epi("gbd2019"),
    context = make_shadow_test_boundary_context(
      epi_version = "gbd2019",
      rr_selection_mode = rr_selection$mode,
      calc_causes = rr_selection$calc_causes,
      gbd_causes = rr_selection$gbd_causes
    ),
    population_year_intent = .capture_population_year_intent(
      pop_year = 2020,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = 2020
    )
  )
  components <- observation$result$components
  blocking_components <- observation$result$blocking_components
  blocking_summary <- blocking_components %>%
    dplyr::count(component_type, source, name = "n") %>%
    dplyr::arrange(component_type, source)

  expect_equal(observation$status, "resolved")
  expect_equal(
    observation$input_contract,
    "legacy_default_gemm_gbd2019_2020"
  )
  expect_false(observation$result$ready)
  expect_equal(
    components %>% dplyr::count(component_type, name = "n"),
    tibble::tribble(
      ~component_type, ~n,
      "crf", 15L,
      "epi", 1L,
      "rr", 18L
    )
  )
  expect_true(components$ready[components$component_type == "epi"])
  expect_true(all(
    components$reproduction_status[components$component_type == "crf"] ==
      "parity_verified"
  ))
  expect_true(all(components$ready[components$component_type == "crf"]))
  expect_true(all(
    components$reproduction_status[components$component_type == "rr"] ==
      "parity_verified"
  ))
  expect_true(all(
    components$routing_status[components$component_type == "rr"] ==
      "not_ready"
  ))
  expect_equal(nrow(blocking_components), 18)
  expect_equal(
    blocking_summary,
    tibble::tribble(
      ~component_type, ~source, ~n,
      "rr", "gemm", 14L,
      "rr", "original", 4L
    )
  )
  expect_setequal(
    observation$specification$readiness$blocking_component_ids,
    blocking_components$component_id
  )
  expect_equal(nrow(observation$result$ignored_rr_pairs), 0)

  route_plan <- .plan_hia_compatibility_route(
    observation,
    registry_set_resolver = function(...) {
      stop("Unready historical RR must not resolve a Registry Set.")
    }
  )
  expect_equal(route_plan$route, "legacy")
  expect_equal(route_plan$reason, "components_not_ready")
  expect_setequal(
    route_plan$blocking_component_ids,
    blocking_components$component_id
  )
})


test_that("custom HIA inputs stay outside the first shadow slice", {
  observed <- FALSE
  expected <- make_shadow_test_impacts()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      observed <<- TRUE
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(...) {
      stop("custom inputs must not reach shadow readiness")
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia(
        rr_sources = tibble::tibble(
          cause = "NCD.LRI",
          source = RR_GEMM
        ),
        epi = make_shadow_test_epi(),
        crfs = make_shadow_test_crfs()
      )
    }
  )

  expect_identical(result, expected)
  expect_false(observed)
})


test_that("non-default profile versions stay outside the first shadow slice", {
  observed <- FALSE
  expected <- make_shadow_test_impacts()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      observed <<- TRUE
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") make_shadow_test_crfs(),
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(...) {
      stop("non-default profiles must not reach shadow readiness")
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia(crfs_version = "C40")
    }
  )

  expect_identical(result, expected)
  expect_false(observed)
})
