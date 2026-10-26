test_that("Krewski plus GBD preserves components used by Eskom reporting", {
  causes <- grep("Death|YLD", get_calc_causes("GBD only"), value = TRUE)
  rr <- convert_old_parameters_to_rr_sources(causes, "all")
  epi <- legacy_hia_epi_fixture(get_crfs("Krewski-South Africa"), rr)
  epi_long <- legacy_hia_epi_long_fixture(rr)
  local_mocked_bindings(
    get_epi_location_id = function(x) rep(1L, length(x)),
    get_epi_count_long = function(...) epi_long
  )
  for (routing in c(FALSE, TRUE)) {
    withr::local_options(creahia.hia_compatibility_routing = routing)
    result <- suppressWarnings(compute_hia(
      conc_map = legacy_default_fingerprint_conc_map(),
      species = c("pm25", "no2", "so2", "o3_8h"),
      regions = legacy_hia_fingerprint_regions(), epi = epi,
      crfs_version = "Krewski-South Africa", epi_version = "C40",
      calc_causes = causes, gbd_causes = "all", diagnostic_folder = NULL
    ))
    deaths <- dplyr::filter(result, pollutant == "PM25", outcome == "Deaths", estimate == "central")
    expect_true("AllCause" %in% deaths$cause)
    expect_true(all(!deaths$double_counted))
    reported <- dplyr::filter(deaths, cause != "AllCause", !double_counted)
    expect_setequal(reported$cause, c("COPD", "Diabetes", "IHD", "LC", "LRI", "Stroke"))
    expect_true(all(is.finite(reported$number)))
    expect_true(any(reported$number != 0))
  }
})


test_that("Krewski plus GEMM aggregate fails before PAF calculation", {
  rr <- parse_rr_sources(c(RR_GEMM, RR_ORIGINAL))
  epi <- legacy_hia_epi_fixture(get_crfs("Krewski-South Africa"), rr)
  local_mocked_bindings(compute_hia_paf = function(...) stop("PAF must not run"))
  for (routing in c(FALSE, TRUE)) {
    withr::local_options(creahia.hia_compatibility_routing = routing)
    expect_error(compute_hia(
      conc_map = legacy_default_fingerprint_conc_map(), species = "pm25",
      regions = legacy_hia_fingerprint_regions(), epi = epi,
      crfs_version = "Krewski-South Africa", diagnostic_folder = NULL
    ), "Conflicting aggregate estimates selected:.*AllCause, NCD.LRI")
  }
})
