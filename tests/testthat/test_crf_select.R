test_that("resolve_crf_selection selects requested registry rows", {
  crfs <- resolve_crf_selection(tibble::tribble(
    ~pollutant, ~cause,     ~outcome, ~crf_id,
    "PM25",     "IHD",      "Deaths", "gemm_pm25_ihd_25plus_deaths_v1",
    "NO2",      "NCD.LRI",  "Deaths", "legacy_no2_ncdlri_deaths_v1"
  ))

  expect_s3_class(crfs, "data.frame")
  expect_equal(nrow(crfs), 2)
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% crfs$crf_id)
  expect_true("legacy_no2_ncdlri_deaths_v1" %in% crfs$crf_id)
})

test_that("resolve_crf_selection errors on unknown crf_id", {
  expect_error(
    resolve_crf_selection(tibble::tribble(
      ~pollutant, ~cause, ~outcome, ~crf_id,
      "PM25", "IHD", "Deaths", "missing_crf"
    )),
    "Unknown crf_id"
  )
})

test_that("resolve_crf_selection errors when one pollutant/cause has multiple sources", {
  expect_error(
    resolve_crf_selection(tibble::tribble(
      ~pollutant, ~cause, ~outcome, ~crf_id,
      "PM25", "IHD", "Deaths", "gemm_pm25_ihd_25plus_deaths_v1",
      "PM25", "IHD", "Deaths", "test_tabular_pm25_ihd_deaths_v1"
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

test_that("describe_crf_preset returns human-readable registry-backed rows", {
  described <- describe_crf_preset("experimental_default")

  expect_s3_class(described, "data.frame")
  expect_equal(nrow(described), 2)

  expect_true(all(c(
    "preset",
    "pollutant",
    "cause",
    "outcome",
    "crf_id",
    "reference_id",
    "form",
    "notes"
  ) %in% names(described)))

  expect_true(all(described$preset == "experimental_default"))
  expect_true("burnett_2018_gemm" %in% described$reference_id)
  expect_true("legacy_default_crfs" %in% described$reference_id)
})

test_that("describe_crf_preset errors on unknown preset", {
  expect_error(
    describe_crf_preset("missing_preset"),
    "Unknown CRF preset"
  )
})

test_that("search_crf_registry filters by user-facing registry fields", {
  result <- search_crf_registry(
    pollutant = "PM25",
    cause = "IHD",
    outcome = "Deaths"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$pollutant == "PM25"))
  expect_true(all(result$cause == "IHD"))
  expect_true(all(result$outcome == "Deaths"))

  expect_true(all(c(
    "pollutant",
    "cause",
    "outcome",
    "crf_id",
    "reference_id",
    "form",
    "notes"
  ) %in% names(result)))
})

test_that("search_crf_registry filters by reference_id and form", {
  result <- search_crf_registry(
    reference_id = "burnett_2018_gemm",
    form = CRF_FORM_TABULAR
  )

  expect_gt(nrow(result), 0)
  expect_true(all(result$reference_id == "burnett_2018_gemm"))
  expect_true(all(result$form == CRF_FORM_TABULAR))
})

test_that("search_crf_registry supports text query", {
  result <- search_crf_registry(query = "gemm")

  expect_gt(nrow(result), 0)
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% result$crf_id)
})

test_that("search_crf_registry returns empty result for unmatched filter values", {
  result <- search_crf_registry(pollutant = "missing_pollutant")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c(
    "pollutant",
    "cause",
    "outcome",
    "crf_id",
    "reference_id",
    "form",
    "notes"
  ) %in% names(result)))
})

test_that("available_crf_references returns human-readable reference fields", {
  references <- available_crf_references()

  expect_s3_class(references, "data.frame")
  expect_gt(nrow(references), 0)

  expect_true(all(c(
    "reference_id",
    "author",
    "year",
    "title",
    "notes"
  ) %in% names(references)))

  expect_true("burnett_2018_gemm" %in% references$reference_id)
  expect_true("legacy_default_crfs" %in% references$reference_id)
})

test_that("available_crf_references includes registry reference IDs", {
  registry <- load_crf_registry()
  references <- available_crf_references()

  expect_true(all(unique(registry$reference_id) %in% references$reference_id))
})



test_that("crf_override_options errors on unknown preset", {
  expect_error(
    crf_override_options(
      presets = "missing_preset",
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths"
    ),
    "Unknown CRF preset"
  )
})

test_that("crf_override_options errors when presets are missing or empty", {
  expect_error(
    crf_override_options(
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths"
    ),
    "`presets` must contain at least one CRF preset name",
    fixed = TRUE
  )

  expect_error(
    crf_override_options(
      presets = character(0),
      pollutant = "PM25",
      cause = "IHD",
      outcome = "Deaths"
    ),
    "`presets` must contain at least one CRF preset name",
    fixed = TRUE
  )
})

test_that("crf_override_options supports partial pollutant filter", {
  options <- crf_override_options(
    presets = "experimental_default",
    pollutant = "PM25"
  )

  expect_gt(nrow(options), 0)
  expect_true(all(options$pollutant == "PM25"))
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% options$crf_id)
})

test_that("crf_override_options supports partial cause filter", {
  options <- crf_override_options(
    presets = "experimental_default",
    cause = "IHD"
  )

  expect_gt(nrow(options), 0)
  expect_true(all(options$cause == "IHD"))
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% options$crf_id)
})

test_that("crf_override_options supports case-insensitive partial outcome filter", {
  options <- crf_override_options(
    presets = "experimental_default",
    outcome = "death"
  )

  expect_gt(nrow(options), 0)
  expect_true(all(options$outcome == "Deaths"))
})

test_that("crf_override_options returns empty result when no selected slots match filters", {
  options <- crf_override_options(
    presets = "experimental_default",
    pollutant = "missing_pollutant"
  )

  expect_s3_class(options, "data.frame")
  expect_equal(nrow(options), 0)
  expect_true(all(c(
    "pollutant",
    "cause",
    "outcome",
    "selected",
    "selected_by_preset",
    "crf_id",
    "reference_id",
    "form",
    "notes"
  ) %in% names(options)))
})

test_that("crfs_override replaces selected pollutant/cause row", {
  crfs <- crfs_preset("experimental_default")

  updated <- crfs_override(
    crfs,
    pollutant = "PM25",
    cause = "IHD",
    outcome = "Deaths",
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
      outcome = "Deaths",
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