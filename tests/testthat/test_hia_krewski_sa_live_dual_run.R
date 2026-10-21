make_krewski_sa_cause_selection <- function() {
  get_calc_causes("GBD only") %>%
    grep("Death|YLD", ., value = TRUE)
}


make_krewski_sa_portable_concentrations <- function() {
  list(
    krewski_sa_sample = list(
      ZAF = data.frame(
        conc_baseline_pm25 = c(28, 35),
        conc_scenario_pm25 = c(26, 31),
        conc_baseline_no2 = c(24, 38),
        conc_scenario_no2 = c(22, 34),
        conc_baseline_o3_8h = c(62, 74),
        conc_scenario_o3_8h = c(58, 68),
        conc_baseline_so2 = c(8, 13),
        conc_scenario_so2 = c(6, 9),
        pop = c(2e6, 3e6)
      )
    )
  )
}


run_krewski_sa_portable_hia <- function() {
  compute_hia(
    conc_map = make_krewski_sa_portable_concentrations(),
    species = c("pm25", "no2", "o3_8h", "so2"),
    regions = tibble::tibble(
      region_id = "ZAF",
      region_name = "South Africa",
      country_id = "ZAF"
    ),
    crfs_version = "Krewski-South Africa",
    epi_version = "C40",
    calc_causes = make_krewski_sa_cause_selection(),
    gbd_causes = "all",
    pop_year = 2021,
    diagnostic_folder = NULL
  )
}


test_that("Krewski South Africa legacy selection matches the Registry candidate", {
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
    run_krewski_sa_portable_hia(),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 90L)
  expect_s3_class(seen$result, "creahia_hia_dual_run_result")
  expect_equal(seen$result$status, "matched")
  expect_equal(seen$result$reason, "outputs_match")
  expect_equal(seen$result$legacy_rows, nrow(result))
  expect_equal(seen$result$registry_rows, nrow(result))
  expect_length(seen$result$differences, 0L)
  expect_true(any(grepl("No age weights found.*LRI.child", warnings)))
  expect_true(any(grepl(
    "PAF estimates are not properly ordered.*Diabetes",
    warnings
  )))
})


test_that("Krewski South Africa can use the activated compatibility route", {
  seen <- new.env(parent = emptyenv())
  warnings <- character()
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(observation) {
      seen$observation <- observation
    },
    creahia.hia_dual_run_observer = NULL,
    creahia.hia_compatibility_routing = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  result <- withCallingHandlers(
    run_krewski_sa_portable_hia(),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 90L)
  expect_equal(seen$observation$status, "resolved")
  expect_equal(seen$observation$route_plan$route, "registry")
  expect_equal(seen$observation$route_application$status, "applied")
  expect_identical(
    seen$observation$route_application$compute_inputs$crf_compute,
    "registry"
  )
  expect_true(any(grepl("No age weights found.*LRI.child", warnings)))
  expect_true(any(grepl(
    "PAF estimates are not properly ordered.*Diabetes",
    warnings
  )))
})
