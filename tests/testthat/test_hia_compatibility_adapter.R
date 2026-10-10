make_adapter_test_context <- function(...) {
  defaults <- list(
    rr_sources_supplied = FALSE,
    epi_supplied = FALSE,
    crfs_supplied = FALSE,
    epi_version = "default",
    crfs_version = "default",
    crf_compute = "legacy",
    calc_causes = NULL,
    gbd_causes = NULL
  )
  utils::modifyList(defaults, list(...))
}


make_adapter_test_population_year <- function() {
  .capture_population_year_intent(
    pop_year = NULL,
    scale_base_year = NULL,
    scale_target_year = NULL,
    resolved_pop_year = NULL
  )
}


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


test_that("compatibility boundary explicitly excludes unsupported slices", {
  contexts <- list(
    registry_route = make_adapter_test_context(crf_compute = "registry"),
    legacy_cause_selectors = make_adapter_test_context(
      calc_causes = "GEMM and GBD"
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
