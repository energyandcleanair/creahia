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
      observation <- .observe_default_hia_readiness(
        crfs = make_shadow_test_crfs(),
        rr_sources = tibble::tibble(
          cause = "NCD.LRI",
          source = RR_GEMM
        ),
        epi = make_shadow_test_epi(),
        epi_version = "default",
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
  observation <- .observe_default_hia_readiness(
    crfs = get_crfs("default"),
    rr_sources = parse_rr_sources(c(RR_GEMM, RR_ORIGINAL)),
    epi = get_epi("default"),
    epi_version = "default",
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
  expect_equal(nrow(observation$result$blocking_components), 33)
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
