test_that("legacy default CRF PAF fingerprint remains stable", {
  # This is a small deterministic baseline for the 15 active rows in CRFs.csv.
  # It protects the legacy numerical contract while the Registry equivalent is
  # still incomplete; it does not endorse every row's scientific metadata.
  crfs <- get_crfs("default")

  conc_map <- list(
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

  actual <- compute_hia_paf_crfs(
    species = c("pm25", "no2", "o3_8h", "so2"),
    conc_map = conc_map,
    regions = data.frame(region_id = "fingerprint_region"),
    crfs = crfs
  )$legacy_default_baseline %>%
    dplyr::arrange(pollutant, cause, outcome)

  expected <- readr::read_csv(
    testthat::test_path("fixtures", "legacy_default_crf_paf_fingerprint.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::arrange(pollutant, cause, outcome)

  testthat::expect_equal(nrow(actual), 15L)
  testthat::expect_equal(
    as.data.frame(actual),
    as.data.frame(expected),
    tolerance = 1e-12
  )
})
