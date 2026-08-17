test_that("registry-only compute_hia reaches real PAF and impact calculations", {
  registry_crfs <- crfs_set(presets = "experimental_default")

  conc_map <- list(
    scenario1 = list(
      BGD = tibble::tibble(
        conc_baseline_no2 = c(30, 35, 40),
        conc_scenario_no2 = c(20, 25, 30),
        pop = c(100, 200, 300)
      )
    )
  )

  regions <- tibble::tibble(
    region_id = "BGD",
    region_name = "Bangladesh",
    country_id = "BGD"
  )

  epi <- tibble::tibble(
    location_id = 1,
    estimate = c("low", "central", "high"),
    NCD.LRI_Deaths = c(80, 100, 120),
    pop = c(100000, 100000, 100000),
    country = "BGD",
    iso3 = "BGD"
  )

  # Use the real CRF/PAF and impact functions. Only external location lookup
  # and diagnostic plotting are replaced so this test stays deterministic.
  with_mocked_bindings(
    get_epi_location_id = function(...) 1,
    diagnose_paf = function(...) invisible(NULL),
    {
      result <- compute_hia(
        conc_map = conc_map,
        species = "no2",
        regions = regions,
        rr_sources = tibble::tibble(
          cause = character(),
          source = character()
        ),
        epi_version = "default",
        epi = epi,
        crfs = registry_crfs,
        diagnostic_folder = NULL
      )
    }
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$pollutant == "NO2"))
  expect_true(all(result$cause == "NCD.LRI"))
  expect_true(all(result$outcome == "Deaths"))
  expect_true(all(!is.na(result$number)))
  expect_true(any(result$number != 0))
  expect_true(all(!is.na(result$double_counted)))
  expect_false(any(result$double_counted))
})
