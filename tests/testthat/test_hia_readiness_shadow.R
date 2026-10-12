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


test_that("ordinary HIA calls skip shadow resolution unless opted in", {
  expected <- make_shadow_test_impacts()
  old_options <- options(creahia.hia_readiness_shadow_observer = NULL)
  on.exit(options(old_options), add = TRUE)

  expect_false(.hia_readiness_shadow_enabled())

  with_mocked_bindings(
    get_crfs = function(version = "default") make_shadow_test_crfs(),
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .capture_population_year_intent = function(...) {
      stop("disabled shadow mode must not build a diagnostic specification")
    },
    .observe_hia_compatibility_boundary = function(...) {
      stop("disabled shadow mode must not resolve compatibility")
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia()
    }
  )

  expect_identical(result, expected)
})


test_that("shadow mode requires an observer function", {
  old_options <- options(creahia.hia_readiness_shadow_observer = TRUE)
  on.exit(options(old_options), add = TRUE)

  expect_false(.hia_readiness_shadow_enabled())

  options(creahia.hia_readiness_shadow_observer = function(...) NULL)
  expect_true(.hia_readiness_shadow_enabled())
})


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
      make_specification_readiness()
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
  expect_equal(seen$observation$route_plan$route, "legacy")
  expect_equal(
    seen$observation$route_plan$reason,
    "components_not_ready"
  )
  expect_s3_class(
    seen$observation$route_application,
    "creahia_hia_route_application_observation"
  )
  expect_equal(seen$observation$route_application$status, "applied")
  expect_identical(
    seen$observation$route_application$compute_inputs$crfs,
    make_shadow_test_crfs()
  )
  expect_identical(
    seen$observation$route_application$compute_inputs$rr_sources,
    seen$rr_sources
  )
  expect_identical(
    seen$observation$route_application$compute_inputs$crf_compute,
    "legacy"
  )
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
      make_specification_readiness()
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
      make_specification_readiness()
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
    paste(
      "component_resolved",
      "legacy_profile_default",
      "legacy_cause_selectors",
      "gbd2019",
      sep = "|"
    )
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
  expect_equal(seen$observation$route_plan$status, "error")
  expect_equal(
    seen$observation$route_plan$reason,
    "compatibility_boundary_error"
  )
  expect_equal(seen$observation$route_application$status, "error")
  expect_match(
    seen$observation$route_application$error,
    "Cannot apply an errored HIA compatibility route plan"
  )
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
      make_specification_readiness()
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


test_that("shadow route-planning failures stay internal", {
  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) {
      make_specification_readiness()
    },
    .plan_hia_compatibility_route = function(...) {
      stop("route planning failed")
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
  expect_equal(observation$route_plan$status, "error")
  expect_equal(observation$route_plan$reason, "route_planning_error")
  expect_match(observation$route_plan$error, "route planning failed")
  expect_equal(observation$route_application$status, "error")
  expect_null(observation$route_application$compute_inputs)
  expect_match(
    observation$route_application$error,
    "Cannot apply an errored HIA compatibility route plan"
  )
})


test_that("shadow route application prepares complete Registry compute inputs", {
  registry_set <- crfs_set("gbd2019_deaths")

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) {
      make_specification_readiness()
    },
    .plan_hia_compatibility_route = function(...) {
      .new_hia_compatibility_route_plan(
        status = "planned",
        route = "registry",
        reason = "all_components_ready",
        registry_set = registry_set
      )
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

  application <- observation$route_application
  expect_equal(application$status, "applied")
  expect_identical(application$compute_inputs$crfs, registry_set)
  expect_false(has_rr_sources(application$compute_inputs$rr_sources))
  expect_identical(application$compute_inputs$crf_compute, "registry")
})


test_that("shadow Registry candidates do not change the actual compute route", {
  seen <- new.env(parent = emptyenv())
  expected <- make_shadow_test_impacts()
  legacy_crfs <- make_shadow_test_crfs()
  registry_set <- crfs_set("gbd2019_deaths")
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      seen$observation <- observation
    }
  )
  on.exit(options(old_options), add = TRUE)

  with_mocked_bindings(
    get_crfs = function(version = "default") legacy_crfs,
    get_epi = function(version = "default") make_shadow_test_epi(),
    parse_rr_sources = function(rr_sources) {
      tibble::tibble(cause = "NCD.LRI", source = RR_GEMM)
    },
    validate_crfs_against_epi = function(...) invisible(TRUE),
    .resolve_hia_component_readiness = function(...) {
      make_specification_readiness()
    },
    .plan_hia_compatibility_route = function(...) {
      .new_hia_compatibility_route_plan(
        status = "planned",
        route = "registry",
        reason = "all_components_ready",
        registry_set = registry_set
      )
    },
    compute_hia_paf = function(..., crfs, rr_sources, crf_compute) {
      seen$paf_crfs <- crfs
      seen$paf_rr_sources <- rr_sources
      seen$paf_crf_compute <- crf_compute
      tibble::tibble()
    },
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia()
    }
  )

  candidate_inputs <- seen$observation$route_application$compute_inputs
  expect_identical(result, expected)
  expect_identical(candidate_inputs$crfs, registry_set)
  expect_false(has_rr_sources(candidate_inputs$rr_sources))
  expect_identical(candidate_inputs$crf_compute, "registry")
  expect_identical(seen$paf_crfs, legacy_crfs)
  expect_true(has_rr_sources(seen$paf_rr_sources))
  expect_identical(seen$paf_crf_compute, "legacy")
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
  expect_true(observation$result$ready)
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
  expect_equal(nrow(observation$result$blocking_components), 0)
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
  expect_equal(observation$route_plan$route, "registry")
  expect_equal(observation$route_plan$reason, "all_components_ready")
})


test_that("GBD 2019 historical contract satisfies component routing", {
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
  expect_equal(observation$status, "resolved")
  expect_equal(
    observation$input_contract,
    paste(
      "component_resolved",
      "legacy_profile_default",
      "legacy_cause_selectors",
      "gbd2019",
      sep = "|"
    )
  )
  expect_true(observation$result$ready)
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
      "ready"
  ))
  expect_equal(nrow(blocking_components), 0)
  expect_length(
    observation$specification$readiness$blocking_component_ids,
    0
  )
  expect_equal(nrow(observation$result$ignored_rr_pairs), 0)

  route_plan <- observation$route_plan
  expect_equal(route_plan$route, "registry")
  expect_equal(route_plan$reason, "all_components_ready")
  expect_s3_class(route_plan$registry_set, "creahia_crf_set")
  expect_equal(observation$route_application$status, "applied")
  expect_identical(
    observation$route_application$compute_inputs$crf_compute,
    "registry"
  )

  another_population_year <- .observe_hia_compatibility_boundary(
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
      pop_year = 2021,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = 2021
    )
  )
  expect_equal(another_population_year$status, "resolved")
  expect_equal(another_population_year$route_plan$route, "registry")
  expect_equal(
    another_population_year$input_contract,
    observation$input_contract
  )
  expect_setequal(
    another_population_year$result$components$component_id,
    observation$result$components$component_id
  )
})


test_that("C40 is assessed as a component profile instead of excluded", {
  observation <- .observe_hia_compatibility_boundary(
    crfs = get_crfs("C40"),
    rr_sources = parse_rr_sources(c(RR_GEMM, RR_ORIGINAL)),
    epi = get_epi("default"),
    context = make_shadow_test_boundary_context(crfs_version = "C40"),
    population_year_intent = .capture_population_year_intent(
      pop_year = NULL,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = NULL
    )
  )
  crf_components <- observation$result$components %>%
    dplyr::filter(component_type == "crf")

  expect_equal(observation$status, "resolved")
  expect_equal(nrow(crf_components), 19L)
  expect_true(all(
    crf_components$selection_id == "legacy_profile_c40"
  ))
  expect_true(observation$result$ready)
  expect_equal(observation$route_plan$route, "registry")
  expect_equal(nrow(observation$result$blocking_components), 0L)
})


test_that("adapter-inapplicable observations preserve their existing route", {
  observed <- FALSE
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      observed <<- TRUE
    }
  )
  on.exit(options(old_options), add = TRUE)

  observation <- .observe_hia_compatibility_boundary(
    crfs = make_shadow_test_crfs(),
    rr_sources = tibble::tibble(
      cause = "NCD.LRI",
      source = RR_GEMM
    ),
    epi = make_shadow_test_epi(),
    context = make_shadow_test_boundary_context(crf_compute = "registry"),
    population_year_intent = .capture_population_year_intent(
      pop_year = NULL,
      scale_base_year = NULL,
      scale_target_year = NULL,
      resolved_pop_year = NULL
    )
  )

  expect_equal(observation$status, "not_applicable")
  expect_equal(observation$route_plan$route, "unchanged")
  expect_equal(observation$route_plan$reason, "registry_route")
  expect_equal(observation$route_application$status, "applied")
  expect_identical(
    observation$route_application$compute_inputs$crfs,
    make_shadow_test_crfs()
  )
  expect_identical(
    observation$route_application$compute_inputs$crf_compute,
    "registry"
  )
  expect_false(observed)
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


test_that("non-default profile versions use their component membership", {
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
    .resolve_hia_component_readiness = function(..., crf_selection_id) {
      seen$crf_selection_id <- crf_selection_id
      make_specification_readiness()
    },
    compute_hia_paf = function(...) tibble::tibble(),
    compute_hia_impacts = function(...) expected,
    {
      result <- run_shadow_test_hia(crfs_version = "C40")
    }
  )

  expect_identical(result, expected)
  expect_equal(seen$observation$status, "resolved")
  expect_equal(seen$crf_selection_id, "legacy_profile_c40")
})
