test_that("legacy default CRF PAF fingerprint remains stable", {
  # This is a small deterministic baseline for the 15 active rows in CRFs.csv.
  # It protects the legacy numerical contract; it does not endorse every row's
  # scientific metadata.
  crfs <- get_crfs("default")

  actual <- compute_hia_paf_crfs(
    species = c("pm25", "no2", "o3_8h", "so2"),
    conc_map = legacy_default_fingerprint_conc_map(),
    regions = data.frame(region_id = "fingerprint_region"),
    crfs = crfs
  )$legacy_default_baseline %>%
    dplyr::arrange(.data$pollutant, .data$cause, .data$outcome)

  expected <- read_legacy_default_paf_fingerprint()

  testthat::expect_equal(nrow(actual), 15L)
  testthat::expect_equal(
    as.data.frame(actual),
    as.data.frame(expected),
    tolerance = 1e-12
  )
})

test_that("Registry path reproduces all 15 legacy default CRF PAF rows", {
  registry_crfs <- read_legacy_default_registry_fixture()
  production_crf_ids <- load_crf_registry()$crf_id

  # These rows reproduce historical calculations for testing only. Keeping
  # their IDs out of the packaged Registry prevents numerical parity from being
  # mistaken for scientific approval.
  testthat::expect_equal(nrow(registry_crfs), 15L)
  testthat::expect_named(registry_crfs, CRF_REGISTRY_REQUIRED_COLUMNS)
  testthat::expect_false(any(registry_crfs$crf_id %in% production_crf_ids))

  actual <- compute_hia_paf_crfs_registry(
    species = c("pm25", "no2", "o3_8h", "so2"),
    conc_map = legacy_default_fingerprint_conc_map(),
    regions = data.frame(region_id = "fingerprint_region"),
    crfs = registry_crfs
  )$legacy_default_baseline %>%
    dplyr::arrange(.data$pollutant, .data$cause, .data$outcome)

  expected <- read_legacy_default_paf_fingerprint()
  actual_values <- actual %>%
    dplyr::select(dplyr::all_of(c("low", "central", "high")))

  testthat::expect_equal(nrow(actual), 15L)
  testthat::expect_equal(
    actual %>%
      dplyr::select(
        dplyr::all_of(c("pollutant", "cause", "outcome", "region_id"))
      ),
    expected %>%
      dplyr::select(
        dplyr::all_of(c("pollutant", "cause", "outcome", "region_id"))
      )
  )
  testthat::expect_equal(
    as.data.frame(actual),
    as.data.frame(expected),
    tolerance = 1e-12
  )
  testthat::expect_true(all(!is.na(actual_values)))
  testthat::expect_true(any(as.matrix(actual_values) != 0))
})
