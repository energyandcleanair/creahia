test_that("scenario selection restricts all impact routes without changing values", {
  rr <- parse_rr_sources(c(RR_GEMM, RR_ORIGINAL))
  epi <- legacy_hia_epi_fixture(get_crfs(), rr)
  epi_long <- legacy_hia_epi_long_fixture(rr)
  local_mocked_bindings(
    get_epi_location_id = function(x) rep(1L, length(x)),
    get_epi_count_long = function(...) epi_long
  )
  conc <- legacy_default_fingerprint_conc_map()
  conc$other <- conc[[1]]
  conc$other[[1]]$conc_scenario_pm25 <- c(5, 10)
  selected <- names(conc)[1]
  results <- list()
  for (routing in c(FALSE, TRUE)) {
    withr::local_options(creahia.hia_compatibility_routing = routing)
    run <- function(data, scenarios = names(data)) {
      suppressWarnings(compute_hia(
        conc_map = data, scenarios = scenarios,
        species = c("pm25", "no2", "o3_8h", "so2"),
        regions = legacy_hia_fingerprint_regions(), epi = epi,
        diagnostic_folder = NULL
      ))
    }
    all <- run(conc)
    actual <- run(conc, selected)
    expect_equal(unique(actual$scenario), selected)
    expect_equal(actual, dplyr::filter(all, scenario == selected), tolerance = 1e-12)
    conc$other <- list(broken_region = data.frame(unrelated = NA))
    expect_equal(run(conc, selected), actual, tolerance = 1e-12)
    conc$other <- conc[[1]]
    results[[as.character(routing)]] <- dplyr::arrange(
      actual, scenario, region_id, pollutant, cause, outcome, estimate
    )
  }
  expect_equal(results[[1]], results[[2]], tolerance = 1e-12)
})

test_that("standalone PAF selection applies to legacy and Registry CRFs", {
  conc <- legacy_default_fingerprint_conc_map()
  conc$excluded <- list(broken_region = data.frame(unrelated = NA))
  for (route in c("legacy", "registry")) {
    crfs <- if (route == "legacy") get_crfs() else resolve_historical_default_crfs()
    run <- function(data, scenarios = names(data)) compute_hia_paf(
      conc_map = data, scenarios = scenarios,
      species = c("pm25", "no2", "o3_8h", "so2"),
      regions = data.frame(region_id = "fingerprint_region"), crfs = crfs,
      crf_compute = route, diagnostic_folder = NULL
    )
    expect_equal(run(conc, names(conc)[1]), run(conc[1]), tolerance = 1e-12)
  }
})

test_that("unknown scenario names fail before calculation", {
  conc <- list(available = list())
  expect_error(compute_hia(conc, scenarios = "typo"), "Unknown scenarios: typo")
  expect_error(compute_hia_paf(conc, scenarios = "typo"), "Unknown scenarios: typo")
})
