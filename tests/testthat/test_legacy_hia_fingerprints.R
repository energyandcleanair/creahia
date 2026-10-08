test_that("the actual compute_hia defaults have a fixed legacy fingerprint", {
  default_crfs <- get_crfs("default")
  default_rr_sources <- c(RR_GEMM, RR_ORIGINAL)
  epi <- legacy_hia_epi_fixture(default_crfs, default_rr_sources)
  epi_long <- legacy_hia_epi_long_fixture(default_rr_sources)

  legacy_warnings <- character()
  withCallingHandlers(
    with_mocked_bindings(
      get_epi = function(...) epi,
      get_epi_count_long = function(...) epi_long,
      get_epi_location_id = function(...) 1,
      diagnose_paf = function(...) invisible(NULL),
      {
        result <- compute_hia(
          conc_map = legacy_default_fingerprint_conc_map(),
          species = c("pm25", "no2", "o3_8h", "so2"),
          regions = legacy_hia_fingerprint_regions()
        )
      }
    ),
    warning = function(condition) {
      legacy_warnings <<- c(legacy_warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  fingerprint <- legacy_hia_fingerprint(
    result,
    configuration_id = "actual_no_argument_default"
  )
  declared_default_rr_sources <- eval(
    formals(compute_hia)$rr_sources,
    envir = environment(compute_hia)
  )
  resolved_rr <- legacy_hia_resolved_rr_components(default_rr_sources)

  expect_equal(declared_default_rr_sources, default_rr_sources)
  expect_equal(
    resolved_rr$source[resolved_rr$cause == CAUSE_LRICHILD] %>% unique(),
    RR_GEMM
  )
  expect_true(any(grepl("Diabetes YLDs", legacy_warnings, fixed = TRUE)))
  expect_true(any(grepl("Diabetes Deaths", legacy_warnings, fixed = TRUE)))
  expect_equal(
    fingerprint,
    read_legacy_actual_default_hia_fingerprint(),
    tolerance = 1e-12
  )
  expect_setequal(unique(result$estimate), c("low", "central", "high"))
  expect_true(all(!is.na(result$number)))
  expect_true(any(result$number != 0))
})

test_that("all active legacy CRF profile memberships have fixed fingerprints", {
  profile_versions <- c("default", "C40", "Krewski-South Africa")
  expected_profile_rows <- c(
    default = 15L,
    C40 = 19L,
    `Krewski-South Africa` = 21L
  )

  fingerprints <- purrr::map_dfr(profile_versions, function(profile_version) {
    crfs <- get_crfs(profile_version)
    epi <- legacy_hia_epi_fixture(crfs, legacy_hia_empty_rr_sources())

    with_mocked_bindings(
      get_epi_location_id = function(...) 1,
      diagnose_paf = function(...) invisible(NULL),
      {
        result <- compute_hia(
          conc_map = legacy_default_fingerprint_conc_map(),
          species = c("pm25", "no2", "o3_8h", "so2"),
          regions = legacy_hia_fingerprint_regions(),
          rr_sources = legacy_hia_empty_rr_sources(),
          epi = epi,
          crfs = crfs,
          diagnostic_folder = NULL,
          crf_compute = "legacy"
        )
      }
    )

    fingerprint <- legacy_hia_fingerprint(
      result,
      configuration_id = profile_version
    )

    expect_equal(nrow(fingerprint), expected_profile_rows[[profile_version]])
    expect_setequal(unique(result$estimate), c("low", "central", "high"))
    expect_true(all(!is.na(result$number)))
    expect_true(any(result$number != 0))

    fingerprint
  }) %>%
    dplyr::arrange(
      .data$configuration_id,
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome
    )

  expect_equal(
    fingerprints,
    read_legacy_crf_profiles_hia_fingerprint(),
    tolerance = 1e-12
  )
  expect_equal(nrow(fingerprints), 55L)
})
