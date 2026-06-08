test_that("select_crfs selects requested registry rows", {
  crfs <- select_crfs(tibble::tribble(
    ~pollutant, ~cause,     ~crf_id,
    "PM25",     "IHD",      "gemm_pm25_ihd_25plus_deaths_v1",
    "NO2",      "NCD.LRI",  "legacy_no2_ncdlri_deaths_v1"
  ))

  expect_s3_class(crfs, "data.frame")
  expect_equal(nrow(crfs), 2)
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% crfs$crf_id)
  expect_true("legacy_no2_ncdlri_deaths_v1" %in% crfs$crf_id)
})

test_that("select_crfs errors on unknown crf_id", {
  expect_error(
    select_crfs(tibble::tribble(
      ~pollutant, ~cause, ~crf_id,
      "PM25", "IHD", "missing_crf"
    )),
    "Unknown crf_id"
  )
})

test_that("select_crfs errors when one pollutant/cause has multiple sources", {
  expect_error(
    select_crfs(tibble::tribble(
      ~pollutant, ~cause, ~crf_id,
      "PM25", "IHD", "gemm_pm25_ihd_25plus_deaths_v1",
      "PM25", "IHD", "test_tabular_pm25_ihd_deaths_v1"
    )),
    "exactly one CRF source"
  )
})

test_that("crfs_bundle loads experimental default", {
  crfs <- crfs_bundle("experimental_default")

  expect_equal(nrow(crfs), 2)
  expect_equal(
    sort(crfs$crf_id),
    sort(c(
      "gemm_pm25_ihd_25plus_deaths_v1",
      "legacy_no2_ncdlri_deaths_v1"
    ))
  )
})

test_that("crfs_bundle errors on unknown bundle", {
  expect_error(
    crfs_bundle("missing_bundle"),
    "Unknown CRF bundle"
  )
})

test_that("crfs_override replaces selected pollutant/cause row", {
  crfs <- crfs_bundle("experimental_default")

  updated <- crfs_override(
    crfs,
    pollutant = "PM25",
    cause = "IHD",
    crf_id = "test_tabular_pm25_ihd_deaths_v1"
  )

  expect_true("test_tabular_pm25_ihd_deaths_v1" %in% updated$crf_id)
  expect_false("gemm_pm25_ihd_25plus_deaths_v1" %in% updated$crf_id)
  expect_true("legacy_no2_ncdlri_deaths_v1" %in% updated$crf_id)
})

test_that("crfs_override errors on unknown replacement crf_id", {
  crfs <- crfs_bundle("experimental_default")

  expect_error(
    crfs_override(
      crfs,
      pollutant = "PM25",
      cause = "IHD",
      crf_id = "missing_crf"
    ),
    "Unknown crf_id"
  )
})