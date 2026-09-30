test_that("Registry path reproduces legacy default impacts end to end", {
  legacy_crfs <- get_crfs("default")
  registry_crfs <- new_crfs_set(read_legacy_default_registry_fixture())
  conc_map <- legacy_default_fingerprint_conc_map()
  epi <- legacy_default_hia_epi_fixture()
  regions <- tibble::tibble(
    region_id = "fingerprint_region",
    region_name = "Fingerprint region",
    country_id = "FPR"
  )
  empty_rr_sources <- tibble::tibble(
    cause = character(),
    source = character()
  )

  # Keep external location lookup and diagnostic plotting outside this
  # deterministic parity test. The real PAF and impact calculations still run.
  with_mocked_bindings(
    get_epi_location_id = function(...) 1,
    diagnose_paf = function(...) invisible(NULL),
    {
      legacy_result <- compute_hia(
        conc_map = conc_map,
        species = c("pm25", "no2", "o3_8h", "so2"),
        regions = regions,
        rr_sources = empty_rr_sources,
        epi = epi,
        crfs = legacy_crfs,
        diagnostic_folder = NULL,
        crf_compute = "legacy"
      )

      registry_result <- compute_hia(
        conc_map = conc_map,
        species = c("pm25", "no2", "o3_8h", "so2"),
        regions = regions,
        rr_sources = empty_rr_sources,
        epi = epi,
        crfs = registry_crfs,
        diagnostic_folder = NULL
      )
    }
  )

  arrange_columns <- c(
    "scenario",
    "region_id",
    "pollutant",
    "cause",
    "outcome",
    "estimate"
  )

  legacy_comparison <- legacy_result %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(arrange_columns)))
  registry_comparison <- registry_result %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(arrange_columns)))

  testthat::expect_s3_class(legacy_result, "data.frame")
  testthat::expect_s3_class(registry_result, "data.frame")
  testthat::expect_equal(nrow(legacy_comparison), 45L)
  testthat::expect_equal(nrow(registry_comparison), 45L)
  testthat::expect_equal(
    registry_comparison,
    legacy_comparison,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    dplyr::n_distinct(
      registry_comparison$pollutant,
      registry_comparison$cause,
      registry_comparison$outcome
    ),
    15L
  )
  testthat::expect_true(all(!is.na(registry_comparison$number)))
  testthat::expect_true(any(registry_comparison$number != 0))
  testthat::expect_true(all(!is.na(registry_comparison$double_counted)))
})
