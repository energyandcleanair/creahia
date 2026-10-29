test_that("GBD death specifications preserve version-specific contracts", {
  specs <- gbd_deaths_specs()

  expect_equal(nrow(specs), 22)
  expect_equal(
    as.integer(table(specs$version)),
    c(7L, 7L, 8L)
  )
  expect_equal(length(unique(specs$crf_id)), nrow(specs))
  expect_true(all(specs$outcome == MEASURE_DEATHS))

  dementia_versions <- specs %>%
    dplyr::filter(cause == CAUSE_DEMENTIA) %>%
    dplyr::pull(version)
  expect_equal(dementia_versions, RR_GBD2023)

  expect_equal(
    gbd_expected_ages(RR_GBD2019, CAUSE_IHD),
    AGE_ADULTS_SPLIT
  )
  expect_equal(
    gbd_expected_ages(RR_GBD2019, CAUSE_STROKE),
    AGE_ADULTS_SPLIT
  )
  expect_equal(gbd_expected_ages(RR_GBD2021, CAUSE_IHD), AGE_ADULTS)
  expect_equal(gbd_expected_ages(RR_GBD2023, CAUSE_DEMENTIA), AGE_ADULTS)
  expect_equal(gbd_expected_ages(RR_GBD2023, CAUSE_LRICHILD), AGE_CHILDREN)

  expect_error(gbd_deaths_specs("gbd_missing"), "Unsupported GBD versions")
  expect_error(
    gbd_expected_ages(RR_GBD2021, CAUSE_DEMENTIA),
    "Unsupported gbd2021 death cause"
  )
})


test_that("GBD compiler preserves every processed source curve", {
  specs <- gbd_deaths_specs()

  for (version in gbd_versions()) {
    source <- readr::read_csv(
      get_hia_path(gbd_source_path(version), error_if_not_exists = TRUE),
      col_types = readr::cols()
    )
    version_specs <- specs %>%
      dplyr::filter(.data$version == .env$version)

    for (i in seq_len(nrow(version_specs))) {
      cause <- version_specs$cause[[i]]
      ages <- gbd_expected_ages(version, cause)
      compiled <- compile_gbd_deaths_data(
        source,
        version = version,
        cause = cause
      )
      packaged <- readr::read_csv(
        get_hia_path(
          version_specs$data_path[[i]],
          error_if_not_exists = TRUE
        ),
        col_types = readr::cols()
      ) %>%
        tibble::as_tibble()
      expected <- source %>%
        dplyr::filter(
          .data$cause == .env$cause,
          .data$source == .env$version
        ) %>%
        dplyr::mutate(.age_order = match(.data$age, .env$ages)) %>%
        dplyr::arrange(.data$.age_order, .data$exposure) %>%
        dplyr::select(exposure, age, low, central, high) %>%
        tibble::as_tibble()

      info <- paste(version, cause)
      expect_equal(compiled, expected, info = info)
      expect_equal(packaged, expected, info = info)
      expect_equal(unique(packaged$age), ages, info = info)
    }
  }
})


test_that("GBD validation rejects incomplete split-age curves", {
  source <- readr::read_csv(
    get_hia_path(gbd_source_path(RR_GBD2019), error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  incomplete <- source %>%
    dplyr::filter(
      !(cause == CAUSE_IHD & age == AGE_ADULTS_SPLIT[[1]])
    )

  expect_error(
    compile_gbd_deaths_data(
      incomplete,
      version = RR_GBD2019,
      cause = CAUSE_IHD
    ),
    paste0("missing: ", AGE_ADULTS_SPLIT[[1]])
  )
})


test_that("GBD candidate paths match legacy age-weighted PAFs", {
  specs <- gbd_deaths_specs()
  source_tables <- stats::setNames(lapply(gbd_versions(), function(version) {
    readr::read_csv(
      get_hia_path(gbd_source_path(version), error_if_not_exists = TRUE),
      col_types = readr::cols()
    )
  }), gbd_versions())
  compiled <- stats::setNames(lapply(seq_len(nrow(specs)), function(i) {
    compile_gbd_deaths_data(
      source_tables[[specs$version[[i]]]],
      version = specs$version[[i]],
      cause = specs$cause[[i]]
    )
  }), specs$crf_id)
  epi_by_version <- stats::setNames(lapply(gbd_versions(), function(version) {
    version_specs <- specs %>%
      dplyr::filter(.data$version == .env$version)
    dplyr::bind_rows(lapply(version_specs$cause, function(cause) {
      ages <- deduplicate_adult_ages(gbd_expected_ages(version, cause))
      tibble::tibble(
        location_id = 1,
        cause = cause,
        measure_name = MEASURE_DEATHS,
        age = ages,
        estimate = "central",
        val = seq_along(ages) * 100
      )
    }))
  }), gbd_versions())
  crfs <- specs %>%
    dplyr::transmute(
      version,
      crf_id,
      pollutant = "PM25",
      cause,
      outcome,
      form = CRF_FORM_TABULAR,
      data_path
    )
  conc_base <- c(20, 30, 40)
  conc_perm <- c(15, 25, 35)
  pop <- c(1000, 1500, 2000)

  with_mocked_bindings(
    load_crf_tabular = function(crf) compiled[[crf$crf_id]],
    get_rr = function(version) source_tables[[version]],
    get_epi_count_long = function(version) epi_by_version[[version]],
    get_epi_location_id = function(...) 1,
    {
      for (i in seq_len(nrow(specs))) {
        version <- specs$version[[i]]
        cause <- specs$cause[[i]]
        crf <- crfs %>%
          dplyr::filter(
            .data$version == .env$version,
            .data$cause == .env$cause
          ) %>%
          dplyr::select(-version)
        registry_result <- apply_crf_tabular(
          crf,
          conc_base,
          conc_perm,
          pop,
          region_id = "TEST",
          epi_version = version
        )
        legacy_result <- country_paf_perm(
          pm.base = conc_base,
          pm.perm = conc_perm,
          pop = pop,
          region_id = "TEST",
          cause = cause,
          measure = MEASURE_DEATHS,
          rr_source = version,
          epi_version = version
        )

        registry_values <- registry_result %>%
          dplyr::select(low, central, high) %>%
          unlist() %>%
          unname()

        info <- paste(version, cause)
        expect_equal(
          registry_values,
          unname(legacy_result),
          tolerance = 1e-12,
          info = info
        )
        expect_true(all(is.finite(registry_values)), info = info)
        expect_true(any(registry_values != 0), info = info)
      }
    }
  )
})


test_that("only GBD 2019 and GBD 2023 have completed provenance review", {
  releases <- lapply(gbd_versions(), gbd_release_metadata)

  expect_equal(
    vapply(releases, function(release) release$source_path, character(1)),
    paste0("rr/processed/rr_", gbd_versions(), ".csv")
  )
  expect_equal(
    vapply(releases, function(release) release$reviewed, logical(1)),
    c(TRUE, FALSE, TRUE)
  )
  expect_null(gbd_release_metadata(RR_GBD2021)$reference)
  expect_equal(
    gbd_release_metadata(RR_GBD2023)$reference$doi,
    "10.6069/K4TW-Q814"
  )
  expect_equal(gbd_reviewed_versions(), c(RR_GBD2019, RR_GBD2023))
  expect_error(
    gbd_deaths_registry_rows(RR_GBD2021),
    "has not completed provenance review"
  )
  expect_error(
    gbd_reference_row(RR_GBD2021),
    "has not completed provenance review"
  )
  expect_error(
    compile_gbd_deaths(
      versions = RR_GBD2021,
      update_registry = TRUE
    ),
    "Cannot register unreviewed GBD versions"
  )
  expect_error(gbd_release_metadata("gbd_missing"), "Unsupported GBD version")
})


test_that("future reviewed GBD releases use their own provenance metadata", {
  catalog <- gbd_release_catalog()
  catalog[[RR_GBD2021]]$reviewed <- TRUE

  with_mocked_bindings(
    gbd_release_catalog = function() catalog,
    expect_error(
      gbd_release_metadata(RR_GBD2021),
      "incomplete reference metadata"
    )
  )

  future_reference <- gbd_reference_row(RR_GBD2019) %>%
    dplyr::mutate(
      reference_id = "ihme_future_gbd2021_pm_risk_curves",
      year = 2024,
      title = "Future reviewed GBD 2021 fixture",
      doi = "10.0000/future-gbd2021",
      url = "https://doi.org/10.0000/future-gbd2021",
      notes = "Test-only version-specific provenance"
    )
  catalog[[RR_GBD2021]]$reference <- future_reference

  with_mocked_bindings(
    gbd_release_catalog = function() catalog,
    {
      rows <- gbd_deaths_registry_rows(RR_GBD2021)

      expect_true(all(
        rows$reference_id == "ihme_future_gbd2021_pm_risk_curves"
      ))
      expect_true(all(grepl("rr_gbd2021.csv", rows$notes, fixed = TRUE)))
      expect_false(any(grepl("rr_gbd2019.csv", rows$notes, fixed = TRUE)))
      expect_equal(gbd_reference_row(RR_GBD2021), future_reference)
    }
  )
})


test_that("GBD 2019 death candidates create valid Registry rows", {
  rows <- gbd_deaths_registry_rows()
  reference <- gbd_reference_row()

  expect_named(rows, CRF_REGISTRY_REQUIRED_COLUMNS)
  expect_equal(rows$cause, gbd_death_causes(RR_GBD2019))
  expect_true(all(rows$outcome == MEASURE_DEATHS))
  expect_false("double_counted" %in% names(rows))
  expect_true(all(
    rows$reference_id == "ihme_2021_gbd2019_pm_risk_curves"
  ))
  expect_equal(reference$doi, "10.6069/KHWH-2703")
  expect_match(
    reference$notes,
    "Git history does not establish regeneration",
    fixed = TRUE
  )
  expect_match(reference$notes, "YLL and YLD rows apply the death curves", fixed = TRUE)
  expect_match(
    reference$notes,
    "not an independently downloaded child-specific curve",
    fixed = TRUE
  )
  expect_true(validate_crf_references(reference))
  expect_true(validate_crf_registry(rows, references = reference))
})


test_that("GBD 2019 child LRI provenance is explicit", {
  child <- gbd_deaths_registry_rows() %>%
    dplyr::filter(cause == CAUSE_LRICHILD)
  source <- readr::read_csv(
    get_hia_path(gbd_source_path(RR_GBD2019), error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  child_curve <- source %>%
    dplyr::filter(cause == CAUSE_LRICHILD) %>%
    dplyr::select(exposure, low, central, high)
  lri_curve <- source %>%
    dplyr::filter(cause == CAUSE_LRI) %>%
    dplyr::select(exposure, low, central, high)

  expect_equal(child$age_low, 0)
  expect_equal(child$age_high, 4)
  expect_equal(child_curve, lri_curve)
  expect_match(child$notes, "legacy-derived Under 5 alias", fixed = TRUE)
  expect_match(
    child$notes,
    "not an independently downloaded child curve",
    fixed = TRUE
  )
})


test_that("GBD 2019 death rows are registered exactly once", {
  expected <- gbd_deaths_registry_rows()
  registered <- load_crf_registry() %>%
    dplyr::filter(crf_id %in% expected$crf_id) %>%
    dplyr::arrange(match(crf_id, expected$crf_id)) %>%
    tibble::as_tibble()

  expect_equal(nrow(registered), nrow(expected))
  expect_equal(registered, expected)

  references <- load_crf_references() %>%
    dplyr::filter(reference_id == "ihme_2021_gbd2019_pm_risk_curves")
  expect_equal(nrow(references), 1)
  expect_equal(references$doi, "10.6069/KHWH-2703")
  expect_equal(references$notes, gbd_reference_row()$notes)
})


test_that("GBD 2019 deaths are exposed as an opt-in preset", {
  preset <- load_crf_preset("gbd2019_deaths")
  selected <- crfs_set(presets = "gbd2019_deaths")

  expect_equal(nrow(preset), 7)
  expect_equal(nrow(selected), 7)
  expect_true(is_crfs_set(selected))
  expect_true(all(
    selected$reference_id == "ihme_2021_gbd2019_pm_risk_curves"
  ))
  expect_false(any(grepl("gbd2021|gbd2023", selected$crf_id)))
})
