make_korea_c40_cause_selection <- function() {
  c(
    "Stroke_YLDs",
    "Diabetes_YLDs",
    "COPD_YLDs",
    "NCD.LRI_Deaths",
    "IHD_Deaths",
    "Stroke_Deaths",
    "COPD_Deaths",
    "LC_Deaths",
    "LRI_Deaths",
    "LRI.child_Deaths",
    "Diabetes_Deaths"
  )
}


make_korea_c40_portable_concentrations <- function() {
  list(
    korea_c40_sample = list(
      KOR = data.frame(
        conc_baseline_pm25 = c(28, 35),
        conc_scenario_pm25 = c(26, 31),
        conc_baseline_no2 = c(24, 38),
        conc_scenario_no2 = c(22, 34),
        conc_baseline_so2 = c(8, 13),
        conc_scenario_so2 = c(6, 9),
        pop = c(2e6, 3e6)
      )
    )
  )
}


test_that("Korea C40 legacy selection matches the Registry candidate", {
  seen <- new.env(parent = emptyenv())
  warnings <- character()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = NULL,
    creahia.hia_dual_run_observer = function(result) {
      seen$result <- result
    }
  )
  on.exit(options(old_options), add = TRUE)

  result <- withCallingHandlers(
    compute_hia(
      conc_map = make_korea_c40_portable_concentrations(),
      species = c("pm25", "no2", "so2"),
      regions = tibble::tibble(
        region_id = "KOR",
        region_name = "South Korea",
        country_id = "KOR"
      ),
      crfs_version = "C40",
      epi_version = "C40",
      calc_causes = make_korea_c40_cause_selection(),
      gbd_causes = "default",
      pop_year = 2022,
      diagnostic_folder = NULL
    ),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 75L)
  expect_s3_class(seen$result, "creahia_hia_dual_run_result")
  expect_equal(seen$result$status, "matched")
  expect_equal(seen$result$reason, "outputs_match")
  expect_equal(seen$result$legacy_rows, 75L)
  expect_equal(seen$result$registry_rows, 75L)
  expect_length(seen$result$differences, 0L)
  expect_true(any(grepl("No age weights found.*LRI.child", warnings)))
  expect_true(any(grepl("PAF estimates are not properly ordered.*Diabetes", warnings)))
})
