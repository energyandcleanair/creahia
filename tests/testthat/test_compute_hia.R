test_that("compute_hia preserves positional arguments before dots", {
  expected_before_dots <- c(
    "conc_map", "species", "regions", "scenarios", "rr_sources",
    "epi_version", "epi", "crfs_version", "crfs", "diagnostic_folder",
    "pop_year", "scale_base_year", "scale_target_year", "calc_causes",
    "gbd_causes"
  )
  argument_names <- names(formals(compute_hia))
  dots_position <- match("...", argument_names)

  expect_equal(argument_names[seq_len(dots_position - 1L)], expected_before_dots)
  expect_gt(match("crf_compute", argument_names), dots_position)
})


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
