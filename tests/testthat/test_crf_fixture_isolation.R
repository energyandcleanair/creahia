test_that("internal registry fixtures are retained for tests but hidden from discovery", {
  registry <- load_crf_registry()
  references <- load_crf_references()

  expect_true(any(registry$reference_id == CRF_INTERNAL_REFERENCE_IDS))

  search_results <- search_crf_registry(query = CRF_INTERNAL_REFERENCE_IDS)
  expect_false(any(search_results$reference_id == CRF_INTERNAL_REFERENCE_IDS))

  available_references <- available_crf_references(references = references)
  expect_false(any(available_references$reference_id == CRF_INTERNAL_REFERENCE_IDS))

  preset_rows <- crfs_preset("experimental_default")
  expect_false(any(preset_rows$reference_id == CRF_INTERNAL_REFERENCE_IDS))

  described_rows <- describe_crf_preset("experimental_default")
  expect_false(any(described_rows$reference_id == CRF_INTERNAL_REFERENCE_IDS))

  override_options <- crf_override_options(
    presets = "experimental_default",
    pollutant = "PM25",
    cause = "IHD",
    outcome = "Deaths"
  )
  expect_false(any(override_options$reference_id == CRF_INTERNAL_REFERENCE_IDS))

  preview <- preview_crf_set(presets = "experimental_default")
  expect_false(any(preview$reference_id == CRF_INTERNAL_REFERENCE_IDS))
})
