make_adapter_test_context <- function(...) {
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


make_adapter_test_population_year <- function(
  pop_year = NULL,
  scale_base_year = NULL,
  scale_target_year = NULL,
  resolved_pop_year = pop_year
) {
  .capture_population_year_intent(
    pop_year = pop_year,
    scale_base_year = scale_base_year,
    scale_target_year = scale_target_year,
    resolved_pop_year = resolved_pop_year
  )
}


test_that("RR selection boundary preserves legacy translation and precedence", {
  seen <- new.env(parent = emptyenv())
  explicit_rr_sources <- list(custom = "IgnoredCause")
  expected <- convert_old_parameters_to_rr_sources(
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  with_mocked_bindings(
    log_info = function(message) seen$message <- message,
    {
      translated <- .resolve_hia_rr_selection_boundary(
        rr_sources = explicit_rr_sources,
        calc_causes = "GEMM and GBD",
        gbd_causes = NULL
      )
    }
  )

  expect_equal(translated$mode, "legacy_cause_selectors")
  expect_equal(translated$calc_causes, "GEMM and GBD")
  expect_equal(translated$gbd_causes, "default")
  expect_identical(translated$rr_sources, expected)
  expect_equal(
    seen$message,
    "Using old parameters for calc_causes and gbd_causes. Ignoring rr_sources."
  )
  expect_false(identical(translated$rr_sources, explicit_rr_sources))
})


test_that("RR selection boundary preserves direct and invalid inputs", {
  direct <- list(gemm = "NCD.LRI")
  unchanged <- .resolve_hia_rr_selection_boundary(
    rr_sources = direct,
    calc_causes = NULL,
    gbd_causes = NULL
  )

  expect_equal(unchanged$mode, "rr_sources")
  expect_identical(unchanged$rr_sources, direct)
  expect_error(
    .resolve_hia_rr_selection_boundary(
      rr_sources = direct,
      calc_causes = "GBD only",
      gbd_causes = "all"
    ),
    "Invalid combination of gbd_causes and calc_causes"
  )
})


test_that("compatibility boundary emits the canonical default specification", {
  readiness <- make_specification_readiness()

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      resolved <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = make_adapter_test_context(),
        population_year_intent = make_adapter_test_population_year()
      )
    }
  )

  expect_equal(resolved$status, "resolved")
  expect_equal(resolved$input_contract, "legacy_default")
  expect_identical(resolved$result, readiness)
  expect_s3_class(
    resolved$specification,
    "creahia_hia_specification"
  )
  expect_equal(
    resolved$specification$input_contract,
    "legacy_default"
  )
})


test_that("compatibility boundary emits the first legacy selector specification", {
  readiness <- make_specification_readiness()
  context <- make_adapter_test_context(
    rr_sources_supplied = TRUE,
    rr_selection_mode = "legacy_cause_selectors",
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      resolved <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = context,
        population_year_intent = make_adapter_test_population_year()
      )
    }
  )

  expect_equal(resolved$status, "resolved")
  expect_equal(resolved$input_contract, "legacy_gemm_gbd_default")
  expect_equal(
    resolved$specification$input_contract,
    "legacy_gemm_gbd_default"
  )
})


test_that("compatibility boundary identifies the GBD 2019 historical mapping", {
  readiness <- make_specification_readiness()
  context <- make_adapter_test_context(
    epi_version = "gbd2019",
    rr_selection_mode = "legacy_cause_selectors",
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      resolved <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = context,
        population_year_intent = make_adapter_test_population_year(2020)
      )
    }
  )

  expect_equal(resolved$status, "resolved")
  expect_equal(
    resolved$input_contract,
    "legacy_default_gemm_gbd2019_2020"
  )
  expect_equal(resolved$specification$epi_version, "gbd2019")
  expect_equal(
    resolved$specification$population_year$supplied$pop_year,
    2020
  )
  expect_equal(
    resolved$specification$population_year$resolved_pop_year,
    2020
  )
})


test_that("GBD 2019 historical mapping requires direct 2020 population intent", {
  context <- make_adapter_test_context(
    epi_version = "gbd2019",
    rr_selection_mode = "legacy_cause_selectors",
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )
  population_intents <- list(
    missing = make_adapter_test_population_year(),
    another_year = make_adapter_test_population_year(2021),
    deprecated_scaling = make_adapter_test_population_year(
      scale_base_year = 2019,
      scale_target_year = 2020,
      resolved_pop_year = 2020
    )
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) {
      stop("unsupported mappings must not reach readiness resolution")
    },
    {
      results <- lapply(population_intents, function(population_year_intent) {
        .resolve_hia_compatibility_boundary(
          crfs = tibble::tibble(),
          rr_sources = tibble::tibble(),
          epi = tibble::tibble(),
          context = context,
          population_year_intent = population_year_intent
        )
      })
    }
  )

  expect_true(all(vapply(
    results,
    function(result) identical(result$status, "not_applicable"),
    logical(1)
  )))
  expect_true(all(vapply(
    results,
    function(result) identical(result$reason, "population_year_contract"),
    logical(1)
  )))

  unsupported_epi <- .resolve_hia_compatibility_boundary(
    crfs = tibble::tibble(),
    rr_sources = tibble::tibble(),
    epi = tibble::tibble(),
    context = utils::modifyList(context, list(epi_version = "gbd2021")),
    population_year_intent = make_adapter_test_population_year(2020)
  )
  expect_equal(unsupported_epi$status, "not_applicable")
  expect_equal(unsupported_epi$reason, "non_default_version")
})


test_that("compatibility boundary explicitly excludes unsupported slices", {
  contexts <- list(
    registry_route = make_adapter_test_context(crf_compute = "registry"),
    legacy_cause_selectors = make_adapter_test_context(
      rr_selection_mode = "legacy_cause_selectors",
      calc_causes = "GBD only",
      gbd_causes = "default"
    ),
    non_default_version = make_adapter_test_context(crfs_version = "C40"),
    explicit_scientific_input = make_adapter_test_context(
      rr_sources_supplied = TRUE
    )
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) {
      stop("excluded slices must not reach readiness resolution")
    },
    {
      results <- lapply(contexts, function(context) {
        .resolve_hia_compatibility_boundary(
          crfs = tibble::tibble(),
          rr_sources = tibble::tibble(),
          epi = tibble::tibble(),
          context = context,
          population_year_intent = make_adapter_test_population_year()
        )
      })
    }
  )

  expect_true(all(vapply(
    results,
    function(result) identical(result$status, "not_applicable"),
    logical(1)
  )))
  expect_equal(
    names(results),
    unname(vapply(results, `[[`, character(1), "reason"))
  )
})


test_that("compatibility boundary validates argument provenance", {
  incomplete <- make_adapter_test_context()
  incomplete$epi_supplied <- NULL

  expect_error(
    .resolve_hia_compatibility_boundary(
      crfs = tibble::tibble(),
      rr_sources = tibble::tibble(),
      epi = tibble::tibble(),
      context = incomplete,
      population_year_intent = make_adapter_test_population_year()
    ),
    "missing required fields: epi_supplied"
  )
  expect_error(
    .resolve_hia_compatibility_boundary(
      crfs = tibble::tibble(),
      rr_sources = tibble::tibble(),
      epi = tibble::tibble(),
      context = make_adapter_test_context(crfs_supplied = NA),
      population_year_intent = make_adapter_test_population_year()
    ),
    "must be single TRUE/FALSE values: crfs_supplied"
  )
})
