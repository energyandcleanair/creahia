test_that("canonical default specification stores stable component contracts", {
  population_year <- .capture_population_year_intent(
    pop_year = 2030,
    scale_base_year = NULL,
    scale_target_year = NULL,
    resolved_pop_year = 2030
  )
  specification <- .new_hia_specification(
    readiness = make_specification_readiness(),
    population_year_intent = population_year,
    input_contract = "legacy_default",
    epi_version = "default"
  )

  expect_s3_class(specification, "creahia_hia_specification")
  expect_equal(specification$schema_version, 1L)
  expect_equal(specification$input_contract, "legacy_default")
  expect_equal(nrow(specification$components$crf), 1)
  expect_equal(nrow(specification$components$rr), 1)
  expect_equal(nrow(specification$components$epi), 1)
  expect_equal(
    specification$components$rr$component_id,
    "rr|gemm|CauseB|Deaths"
  )
  expect_equal(specification$epi_version, "default")
  expect_equal(specification$population_year$resolved_pop_year, 2030)
  expect_true(specification$population_year$scaling_requested)
  expect_false(specification$readiness$ready)
  expect_equal(length(specification$readiness$blocking_component_ids), 2)
  expect_false(any(c("crfs", "epi") %in% names(specification)))
})


test_that("population-year intent preserves supplied and resolved values", {
  population_year <- .capture_population_year_intent(
    pop_year = NULL,
    scale_base_year = 2019,
    scale_target_year = 2035,
    resolved_pop_year = 2035
  )

  expect_null(population_year$supplied$pop_year)
  expect_equal(population_year$supplied$scale_base_year, 2019)
  expect_equal(population_year$supplied$scale_target_year, 2035)
  expect_equal(population_year$resolved_pop_year, 2035)
  expect_true(population_year$scaling_requested)

  default_population_year <- .capture_population_year_intent(
    pop_year = NULL,
    scale_base_year = NULL,
    scale_target_year = NULL,
    resolved_pop_year = NULL
  )
  expect_false(default_population_year$scaling_requested)
})


test_that("canonical specification rejects incomplete readiness results", {
  readiness <- make_specification_readiness()
  readiness$ignored_rr_pairs <- NULL

  expect_error(
    .new_hia_specification(
      readiness = readiness,
      population_year_intent = list(),
      input_contract = "legacy_default",
      epi_version = "default"
    ),
    "missing required fields: ignored_rr_pairs"
  )
})
