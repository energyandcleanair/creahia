test_that("dc groups load and validate", {
  dc_groups <- load_dc_groups(validate = FALSE)

  expect_true(is.data.frame(dc_groups))
  expect_true(all(CRF_DC_GROUP_REQUIRED_COLUMNS %in% names(dc_groups)))
  expect_true(validate_dc_groups(dc_groups))
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
    "pm25_adult_mortality"
  )
})