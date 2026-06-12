test_that("dc groups load and validate cause-level schema", {
  dc_groups <- load_dc_groups(validate = FALSE)

  expect_true(is.data.frame(dc_groups))
  expect_true(all(CRF_DC_GROUP_REQUIRED_COLUMNS %in% names(dc_groups)))
  expect_false(any(c("pollutant", "outcome") %in% names(dc_groups)))
  expect_true(validate_dc_groups(dc_groups))
})

test_that("validate_dc_groups rejects deprecated pollutant and outcome columns", {
  dc_groups <- tibble::tribble(
    ~dc_group, ~pollutant, ~cause, ~outcome, ~role, ~notes,
    "ncdlri_mortality", "PM25", "NCD.LRI", "Deaths", "aggregate", "old schema"
  )

  expect_error(
    validate_dc_groups(dc_groups),
    "deprecated columns"
  )
})

test_that("check_dc_conflicts allows component causes without aggregate", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_ihd", "PM25", "IHD", "Deaths",
    "crf_stroke", "PM25", "Stroke", "Deaths"
  )

  expect_no_error(check_dc_conflicts(crfs))
})

test_that("check_dc_conflicts errors when aggregate and component are selected together", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_ncdlri", "PM25", "NCD.LRI", "Deaths",
    "crf_ihd", "PM25", "IHD", "Deaths"
  )

  expect_error(
    check_dc_conflicts(crfs),
    "ncdlri_mortality"
  )

  expect_error(
    check_dc_conflicts(crfs),
    "PM25/Deaths"
  )
})

test_that("check_dc_conflicts keeps pollutant groups separate", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_ncdlri", "PM25", "NCD.LRI", "Deaths",
    "crf_ihd", "NO2", "IHD", "Deaths"
  )

  expect_no_error(check_dc_conflicts(crfs))
})

test_that("check_dc_conflicts keeps outcome groups separate", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_ncdlri", "PM25", "NCD.LRI", "Deaths",
    "crf_ihd", "PM25", "IHD", "YLLs"
  )

  expect_no_error(check_dc_conflicts(crfs))
})

test_that("check_dc_conflicts detects CV aggregate and components", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_cv", "PM25", "CV", "Deaths",
    "crf_stroke", "PM25", "Stroke", "Deaths"
  )

  expect_error(
    check_dc_conflicts(crfs),
    "cv_mortality"
  )
})

test_that("describe_dc_group returns rules for a known group", {
  result <- describe_dc_group("ncdlri_mortality")

  expect_true(is.data.frame(result))
  expect_true("NCD.LRI" %in% result$cause)
  expect_true("IHD" %in% result$cause)
  expect_false(any(c("pollutant", "outcome") %in% names(result)))
})

test_that("describe_dc_group errors on unknown group", {
  expect_error(
    describe_dc_group("missing_group"),
    "Unknown dc_group"
  )
})