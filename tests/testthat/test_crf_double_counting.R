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

test_that("ensemble decisions follow final cause coverage, not stored flags", {
  slots <- tibble::tibble(
    pollutant = "NO2", outcome = "Deaths",
    cause = c("TotCV", "IHD", "Stroke", "OthCV", "TotResp", "COPD", "OthResp"),
    double_counted = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  plan <- .resolve_double_counting(slots)
  expect_equal(plan$double_counted[match(slots$cause, plan$cause)],
               c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
  alone <- .resolve_double_counting(dplyr::filter(slots, cause == "TotCV"))
  expect_false(alone$double_counted)
  components <- .resolve_double_counting(dplyr::filter(slots, !cause %in% c("TotCV", "TotResp")))
  expect_true(all(!components$double_counted))
  nested <- .resolve_double_counting(dplyr::bind_rows(
    slots, tibble::tibble(pollutant = "NO2", outcome = "Deaths", cause = "AllCause")
  ))
  expect_equal(nested$cause[!nested$double_counted], "AllCause")
})


test_that("overlap decisions stay separate by pollutant and outcome", {
  slots <- tibble::tribble(
    ~pollutant, ~cause, ~outcome,
    "PM25", "TotResp", "Deaths",
    "NO2", "COPD", "Deaths",
    "PM25", "COPD", "YLLs"
  )
  expect_true(all(!.resolve_double_counting(slots)$double_counted))
})


test_that("output validation cannot borrow an ensemble from another region or estimate", {
  slots <- tibble::tibble(pollutant = "PM25", outcome = "Deaths", cause = c("NCD.LRI", "IHD"))
  plan <- .resolve_double_counting(slots)
  hia <- dplyr::mutate(slots, region_id = "A", estimate = "central", number = c(100, 30))
  expect_equal(add_double_counted(hia, .double_counting = plan)$double_counted, c(FALSE, TRUE))
  for (field in c("region_id", "estimate")) {
    mismatch <- hia
    mismatch[[field]][2] <- "other"
    expect_error(add_double_counted(mismatch, .double_counting = plan), "selected ensemble is missing")
  }
  expect_error(
    add_double_counted(dplyr::filter(hia, cause == "IHD"), .double_counting = plan),
    "selected ensemble is missing"
  )
})
