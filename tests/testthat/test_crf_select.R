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

test_that("search_crf_registry treats query as literal text", {
  result <- search_crf_registry(query = "IHD 25+ GEMM curve")

  expect_gt(nrow(result), 0)
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% result$crf_id)
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

test_that("preview_crf_set previews selected CRFs from presets", {
  preview <- preview_crf_set(presets = "experimental_default")

  expect_s3_class(preview, "data.frame")
  expect_equal(nrow(preview), 2)

  expect_true(all(c(
    "pollutant",
    "cause",
    "outcome",
    "action",
    "crf_id",
    "reference_id",
    "form",
    "selected_by_preset",
    "notes"
  ) %in% names(preview)))

  expect_true(all(preview$action == "selected"))
  expect_true(all(preview$selected_by_preset == "experimental_default"))

  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% preview$crf_id)
  expect_true("legacy_no2_ncdlri_deaths_v1" %in% preview$crf_id)
})

test_that("preview_crf_set errors when presets are missing or empty", {
  expect_error(
    preview_crf_set(),
    "`presets` must contain at least one CRF preset name",
    fixed = TRUE
  )

  expect_error(
    preview_crf_set(presets = character(0)),
    "`presets` must contain at least one CRF preset name",
    fixed = TRUE
  )
})

test_that("preview_crf_set errors on unknown preset", {
  expect_error(
    preview_crf_set(presets = "missing_preset"),
    "Unknown CRF preset"
  )
})

test_that("preview_crf_set previews source-based replacement", {
  preview <- preview_crf_set(
    presets = "experimental_default",
    replace = list(
      list(
        pollutant = "PM25",
        cause = "IHD",
        outcome = "Deaths",
        reference_id = "registry_fixture"
      )
    )
  )

  replaced <- preview %>%
    dplyr::filter(
      pollutant == "PM25",
      cause == "IHD",
      outcome == "Deaths"
    )

  expect_equal(nrow(replaced), 1)
  expect_equal(replaced$action, "replaced")
  expect_equal(replaced$crf_id, "test_tabular_pm25_ihd_deaths_v1")
  expect_equal(replaced$reference_id, "registry_fixture")

  expect_false("gemm_pm25_ihd_25plus_deaths_v1" %in% preview$crf_id)
})

test_that("preview_crf_set previews crf_id replacement", {
  preview <- preview_crf_set(
    presets = "experimental_default",
    replace = list(
      list(crf_id = "test_tabular_pm25_ihd_deaths_v1")
    )
  )

  replaced <- preview %>%
    dplyr::filter(
      pollutant == "PM25",
      cause == "IHD",
      outcome == "Deaths"
    )

  expect_equal(nrow(replaced), 1)
  expect_equal(replaced$action, "replaced")
  expect_equal(replaced$crf_id, "test_tabular_pm25_ihd_deaths_v1")
})

test_that("preview_crf_set errors on replacement for unselected slot", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      replace = list(
        list(
          pollutant = "NO2",
          cause = "Asthma.1to18",
          outcome = "AsthmaIncidence",
          reference_id = "legacy_default_crfs"
        )
      )
    ),
    "Cannot replace a slot that is not selected by the current presets"
  )
})

test_that("preview_crf_set errors on unknown replacement crf_id", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      replace = list(
        list(crf_id = "missing_crf")
      )
    ),
    "Unknown replacement crf_id"
  )
})

test_that("preview_crf_set previews removed slots", {
  preview <- preview_crf_set(
    presets = "experimental_default",
    remove = list(
      list(
        pollutant = "NO2",
        cause = "NCD.LRI",
        outcome = "Deaths"
      )
    )
  )

  removed <- preview %>%
    dplyr::filter(
      pollutant == "NO2",
      cause == "NCD.LRI",
      outcome == "Deaths"
    )

  expect_equal(nrow(removed), 1)
  expect_equal(removed$action, "removed")
  expect_equal(removed$crf_id, "legacy_no2_ncdlri_deaths_v1")

  selected <- preview %>%
    dplyr::filter(action == "selected")

  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% selected$crf_id)
})

test_that("preview_crf_set errors on removal for unselected slot", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      remove = list(
        list(
          pollutant = "NO2",
          cause = "Asthma.1to18",
          outcome = "AsthmaIncidence"
        )
      )
    ),
    "Cannot remove a slot that is not selected by the current presets"
  )
})

test_that("preview_crf_set errors on incomplete removal entry", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      remove = list(
        list(
          pollutant = "NO2",
          cause = "NCD.LRI"
        )
      )
    ),
    "Removal entries must include"
  )
})

test_that("preview_crf_set errors when replacing a removed slot", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      remove = list(
        list(
          pollutant = "PM25",
          cause = "IHD",
          outcome = "Deaths"
        )
      ),
      replace = list(
        list(crf_id = "test_tabular_pm25_ihd_deaths_v1")
      )
    ),
    "Cannot replace a slot that is not selected by the current presets"
  )
})

test_that("preview_crf_set previews source-based addition", {
  preview <- preview_crf_set(
    presets = "experimental_default",
    add = list(
      list(
        pollutant = "NO2",
        cause = "Asthma.1to18",
        outcome = "AsthmaIncidence",
        reference_id = "legacy_default_crfs"
      )
    )
  )

  added <- preview %>%
    dplyr::filter(
      pollutant == "NO2",
      cause == "Asthma.1to18",
      outcome == "AsthmaIncidence"
    )

  expect_equal(nrow(added), 1)
  expect_equal(added$action, "added")
  expect_equal(added$crf_id, "legacy_no2_asthma_1to18_incidence_v1")
  expect_equal(added$reference_id, "legacy_default_crfs")
  expect_true(is.na(added$selected_by_preset))
})

test_that("preview_crf_set previews crf_id addition", {
  preview <- preview_crf_set(
    presets = "experimental_default",
    add = list(
      list(crf_id = "legacy_no2_asthma_1to18_incidence_v1")
    )
  )

  added <- preview %>%
    dplyr::filter(crf_id == "legacy_no2_asthma_1to18_incidence_v1")

  expect_equal(nrow(added), 1)
  expect_equal(added$action, "added")
})

test_that("preview_crf_set errors when adding an already selected slot", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      add = list(
        list(
          pollutant = "PM25",
          cause = "IHD",
          outcome = "Deaths",
          reference_id = "burnett_2018_gemm"
        )
      )
    ),
    "Cannot add a slot that is already selected"
  )
})

test_that("preview_crf_set errors on incomplete addition entry", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      add = list(
        list(
          pollutant = "NO2",
          cause = "Asthma.1to18"
        )
      )
    ),
    "Source-based addition entries must include"
  )
})

test_that("preview_crf_set errors on unknown addition crf_id", {
  expect_error(
    preview_crf_set(
      presets = "experimental_default",
      add = list(
        list(crf_id = "missing_crf")
      )
    ),
    "Unknown addition crf_id"
  )
})

test_that("crfs_set returns final selected registry rows", {
  crfs <- crfs_set(presets = "experimental_default")

  expect_s3_class(crfs, "data.frame")
  expect_equal(nrow(crfs), 2)
  expect_true("gemm_pm25_ihd_25plus_deaths_v1" %in% crfs$crf_id)
  expect_true("legacy_no2_ncdlri_deaths_v1" %in% crfs$crf_id)
})

test_that("crfs_set applies remove, add, and replace", {
  crfs <- crfs_set(
    presets = "experimental_default",
    remove = list(
      list(
        pollutant = "NO2",
        cause = "NCD.LRI",
        outcome = "Deaths"
      )
    ),
    add = list(
      list(crf_id = "legacy_no2_asthma_1to18_incidence_v1")
    ),
    replace = list(
      list(crf_id = "test_tabular_pm25_ihd_deaths_v1")
    )
  )

  expect_true("test_tabular_pm25_ihd_deaths_v1" %in% crfs$crf_id)
  expect_true("legacy_no2_asthma_1to18_incidence_v1" %in% crfs$crf_id)
  expect_false("gemm_pm25_ihd_25plus_deaths_v1" %in% crfs$crf_id)
  expect_false("legacy_no2_ncdlri_deaths_v1" %in% crfs$crf_id)
})

test_that("crfs_set returns a typed CRF set", {
  crfs <- crfs_set(presets = "experimental_default")

  expect_s3_class(crfs, "creahia_crf_set")
  expect_true(is_crfs_set(crfs))
})

test_that("crfs_set output preserves double_counted metadata", {
  selected <- crfs_set(presets = "experimental_default")

  expect_true("double_counted" %in% names(selected))
  expect_false(any(is.na(selected$double_counted)))
})