test_that("legacy CRF versions remain available", {
  expected_rows <- c(
    default = 15,
    C40 = 19,
    `Krewski-South Africa` = 21
  )

  for (version in names(expected_rows)) {
    crfs <- get_crfs(version = version)

    expect_s3_class(crfs, "data.frame")
    expect_equal(nrow(crfs), unname(expected_rows[[version]]))
    expect_true(all(c("pollutant", "cause", "outcome") %in% names(crfs)))
    expect_false(any(grepl("PM2\\.5", crfs$pollutant)))
  }
})

test_that("legacy cause arguments still translate to RR sources", {
  converted <- convert_old_parameters_to_rr_sources(
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  expect_named(converted, c(RR_GEMM, RR_ORIGINAL))
  expect_equal(
    converted[[RR_GEMM]],
    c(CAUSE_NCDLRI, CAUSE_LRI, CAUSE_COPD, CAUSE_IHD, CAUSE_LUNGCANCER, CAUSE_STROKE)
  )
  expect_equal(converted[[RR_ORIGINAL]], c(CAUSE_LRICHILD, CAUSE_DIABETES))
})

test_that("compute_hia preserves the legacy input and output boundary", {
  seen <- new.env(parent = emptyenv())
  legacy_crfs <- data.frame(
    pollutant = "NO2",
    cause = "NCD.LRI",
    outcome = "Deaths",
    double_counted = FALSE
  )

  with_mocked_bindings(
    validate_crfs_against_epi = function(...) invisible(TRUE),
    compute_hia_paf = function(..., rr_sources, crf_compute) {
      seen$rr_sources <- rr_sources
      seen$crf_compute <- crf_compute
      tibble::tibble(
        scenario = "scenario1",
        pollutant = "NO2",
        cause = "NCD.LRI",
        outcome = "Deaths",
        region_id = "BGD",
        low = 0.01,
        central = 0.02,
        high = 0.03
      )
    },
    compute_hia_impacts = function(...) {
      tibble::tibble(
        scenario = "scenario1",
        pollutant = "NO2",
        cause = "NCD.LRI",
        outcome = "Deaths",
        region_id = "BGD",
        number = 1
      )
    },
    {
      result <- compute_hia(
        conc_map = list(scenario1 = list(BGD = data.frame(pop = 100))),
        species = "no2",
        regions = data.frame(region_id = "BGD"),
        rr_sources = tibble::tibble(cause = character(), source = character()),
        epi = data.frame(),
        crfs = legacy_crfs,
        calc_causes = "GEMM and GBD",
        gbd_causes = "default",
        diagnostic_folder = NULL
      )
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(seen$crf_compute, "legacy")
  expect_true(is.data.frame(seen$rr_sources))
  expect_true(all(c("cause", "source") %in% names(seen$rr_sources)))
  expect_true(nrow(seen$rr_sources) > 0)
  expect_true(all(c("scenario", "pollutant", "cause", "outcome", "region_id", "number") %in% names(result)))
})

test_that("a registry CRF set is the explicit opt-in to registry routing", {
  seen <- new.env(parent = emptyenv())
  registry_crfs <- crfs_set(presets = "experimental_default")

  with_mocked_bindings(
    validate_crfs_against_epi = function(...) invisible(TRUE),
    compute_hia_paf = function(..., crf_compute) {
      seen$crf_compute <- crf_compute
      tibble::tibble()
    },
    compute_hia_impacts = function(...) tibble::tibble(),
    {
      compute_hia(
        conc_map = list(scenario1 = list(BGD = data.frame(pop = 100))),
        species = "no2",
        regions = data.frame(region_id = "BGD"),
        rr_sources = tibble::tibble(cause = character(), source = character()),
        epi = data.frame(),
        crfs = registry_crfs,
        diagnostic_folder = NULL
      )
    }
  )

  expect_equal(seen$crf_compute, "registry")
})
