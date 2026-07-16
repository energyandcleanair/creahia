test_that("compute_hia switches to registry path for crfs_set output", {
  registry_crfs <- crfs_set(presets = "experimental_default")
  seen <- new.env(parent = emptyenv())

  conc_map <- list(
    scenario1 = list(
      BGD = tibble::tibble(
        age = "25+",
        conc_baseline_no2 = c(30, 35, 40),
        conc_scenario_no2 = c(20, 25, 30)
      )
    )
  )

  regions <- tibble::tibble(
    region_id = "BGD",
    name = "Bangladesh"
  )

  with_mocked_bindings(
    validate_crfs_against_epi = function(...) {
      invisible(TRUE)
    },
    compute_hia_paf = function(..., crf_compute) {
      seen$crf_compute <- crf_compute

      tibble::tibble(
        scenario = "scenario1",
        pollutant = "NO2",
        cause = "NCD.LRI",
        outcome = "Deaths",
        region_id = "BGD",
        estimate = "central",
        value = 0.02
      )
    },
    compute_hia_impacts = function(...) {
      tibble::tibble(
        scenario = "scenario1",
        pollutant = "NO2",
        cause = "NCD.LRI",
        outcome = "Deaths",
        region_id = "BGD",
        estimate = "central",
        number = 1
      )
    },
    {
      result <- compute_hia(
        conc_map = conc_map,
        species = "no2",
        regions = regions,
        rr_sources = tibble::tibble(
                      cause = character(),
                      source = character()
                    ),
        epi = tibble::tibble(estimate = c("low", "central", "high")),
        crfs = registry_crfs
      )
    }
  )

  expect_equal(seen$crf_compute, "registry")
  expect_s3_class(result, "data.frame")
})