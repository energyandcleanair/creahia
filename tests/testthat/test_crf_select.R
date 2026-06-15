test_that("resolve_crf_selection selects requested registry rows", {
  crfs <- resolve_crf_selection(tibble::tribble(
    ~pollutant, ~cause,     ~crf_id,
    "PM25",     "IHD",      "gemm_pm25_ihd_25plus_deaths_v1",
    "NO2",      "NCD.LRI",  "legacy_no2_ncdlri_deaths_v1"
  ))

  expect_s3_class(crfs, "data.frame")
  expect_equal(nrow(crfs), 2)
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% crfs$crf_id)
  expect_true("legacy_no2_ncdlri_deaths_v1" %in% crfs$crf_id)
})

test_that("resolve_crf_selection errors on unknown crf_id", {
  expect_error(
    resolve_crf_selection(tibble::tribble(
      ~pollutant, ~cause, ~crf_id,
      "PM25", "IHD", "missing_crf"
    )),
    "Unknown crf_id"
  )
})

test_that("resolve_crf_selection errors when one pollutant/cause has multiple sources", {
  expect_error(
    resolve_crf_selection(tibble::tribble(
      ~pollutant, ~cause, ~crf_id,
      "PM25", "IHD", "gemm_pm25_ihd_25plus_deaths_v1",
      "PM25", "IHD", "test_tabular_pm25_ihd_deaths_v1"
    )),
    "exactly one CRF source"
  )
})

test_that("crfs_preset loads experimental default", {
  crfs <- crfs_preset("experimental_default")

  expect_equal(nrow(crfs), 2)
  expect_equal(
    sort(crfs$crf_id),
    sort(c(
      "gemm_pm25_ihd_25plus_deaths_v1",
      "legacy_no2_ncdlri_deaths_v1"
    ))
  )
})

test_that("crfs_preset errors on unknown preset", {
  expect_error(
    crfs_preset("missing_preset"),
    "Unknown CRF preset"
  )
})

test_that("crfs_override replaces selected pollutant/cause row", {
  crfs <- crfs_preset("experimental_default")

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
  crfs <- crfs_preset("experimental_default")

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

test_that("available_crf_presets includes experimental_default", {
  expect_true("experimental_default" %in% available_crf_presets())
})

test_that("load_crf_preset loads required columns", {
  preset <- load_crf_preset("experimental_default")

  expect_true(all(CRF_PRESET_REQUIRED_COLUMNS %in% names(preset)))
  expect_equal(nrow(preset), 2)
})

test_that("all packaged CRF presets load without error", {
  for (preset_name in available_crf_presets()) {
    expect_no_error(load_crf_preset(preset_name))
  }
})

test_that("all packaged CRF presets have required columns", {
  preset_names <- available_crf_presets()
 
  expect_gt(length(preset_names), 0)

  for (preset_name in preset_names) {
    preset <- load_crf_preset(preset_name)
    expect_true(
      all(CRF_PRESET_REQUIRED_COLUMNS %in% names(preset)),
      info = paste("Preset missing required columns:", preset_name)
    )
  }
})

test_that("all packaged CRF presets resolve to registry rows without error", {
  preset_names <- available_crf_presets()
 
  expect_gt(length(preset_names), 0)

  for (preset_name in preset_names) {
    crfs <- crfs_preset(preset_name)

    expect_s3_class(crfs, "data.frame")
    expect_gt(nrow(crfs), 0)
  }
})

test_that("validate_crf_selection allows double-counting conflicts by default", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_ncdlri", "PM25", "NCD.LRI", "Deaths",
    "crf_ihd", "PM25", "IHD", "Deaths"
  )

  expect_no_error(validate_crf_selection(crfs))
})

test_that("validate_crf_selection can still check double-counting conflicts when requested", {
  crfs <- tibble::tribble(
    ~crf_id, ~pollutant, ~cause, ~outcome,
    "crf_ncdlri", "PM25", "NCD.LRI", "Deaths",
    "crf_ihd", "PM25", "IHD", "Deaths"
  )

  expect_error(
    validate_crf_selection(crfs, check_double_counting = TRUE),
    "ncdlri_mortality"
  )
})