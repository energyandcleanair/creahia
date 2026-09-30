legacy_default_fingerprint_conc_map <- function() {
  list(
    legacy_default_baseline = list(
      fingerprint_region = data.frame(
        conc_baseline_pm25 = c(35, 55),
        conc_scenario_pm25 = c(10, 25),
        conc_baseline_no2 = c(30, 50),
        conc_scenario_no2 = c(10, 20),
        conc_baseline_o3_8h = c(70, 90),
        conc_scenario_o3_8h = c(30, 50),
        conc_baseline_so2 = c(20, 30),
        conc_scenario_so2 = c(5, 10),
        pop = c(1000, 3000)
      )
    )
  )
}

read_legacy_default_paf_fingerprint <- function() {
  readr::read_csv(
    testthat::test_path("fixtures", "legacy_default_crf_paf_fingerprint.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::arrange(.data$pollutant, .data$cause, .data$outcome)
}

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
  fixture_path <- testthat::test_path(
    "fixtures",
    "legacy_default_registry.csv"
  )
  registry_crfs <- load_crf_registry(path = fixture_path)
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
