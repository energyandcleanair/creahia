test_that("Fusion adult death compiler preserves each supported legacy curve", {
  source <- readr::read_csv(
    get_hia_path("rr/processed/rr_fusion.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  specs <- fusion_adult_deaths_specs()
  expected_causes <- c(
    "NCD.LRI", "LRI", "COPD", "Diabetes", "IHD", "LC", "Stroke"
  )

  expect_equal(specs$cause, expected_causes)
  expect_equal(
    specs$age_structure,
    c("25plus", "25plus", "25plus", "25plus", "adult", "25plus", "adult")
  )
  expect_equal(length(unique(specs$crf_id)), length(expected_causes))

  for (i in seq_len(nrow(specs))) {
    cause <- specs$cause[[i]]
    ages <- fusion_expected_ages(cause)
    compiled <- compile_fusion_deaths_data(source, cause = cause)
    packaged <- readr::read_csv(
      get_hia_path(specs$data_path[[i]], error_if_not_exists = TRUE),
      col_types = readr::cols()
    ) %>%
      tibble::as_tibble()
    legacy <- source %>%
      dplyr::filter(
        .data$cause == .env$cause,
        source == RR_FUSION
      ) %>%
      dplyr::mutate(.age_order = match(age, ages)) %>%
      dplyr::arrange(.age_order, exposure) %>%
      dplyr::select(exposure, age, low, central, high) %>%
      tibble::as_tibble()

    expect_equal(compiled, legacy, info = cause)
    expect_equal(packaged, compiled, info = cause)
    expect_equal(unique(packaged$age), ages, info = cause)
    expect_equal(
      nrow(packaged),
      if (cause %in% fusion_split_age_causes()) 39013 else 3001,
      info = cause
    )
  }
})


test_that("Fusion compiler excludes duplicated derived child LRI curves", {
  source <- readr::read_csv(
    get_hia_path("rr/processed/rr_fusion.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  child_lri <- source %>%
    dplyr::filter(cause == CAUSE_LRICHILD, source == RR_FUSION)

  expect_false(CAUSE_LRICHILD %in% fusion_adult_death_causes())
  expect_equal(
    sum(duplicated(child_lri[c("age", "exposure")])),
    3001
  )
  expect_error(
    compile_fusion_deaths_data(source, cause = CAUSE_LRICHILD),
    "Unsupported Fusion adult death cause"
  )
})


test_that("Fusion candidate paths match legacy age-weighted PAFs", {
  source <- readr::read_csv(
    get_hia_path("rr/processed/rr_fusion.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  specs <- fusion_adult_deaths_specs()
  compiled <- stats::setNames(
    lapply(
      specs$cause,
      function(cause) compile_fusion_deaths_data(source, cause = cause)
    ),
    specs$cause
  )
  fake_epi <- dplyr::bind_rows(lapply(
    specs$cause,
    function(cause) {
      ages <- deduplicate_adult_ages(fusion_expected_ages(cause))
      tibble::tibble(
        location_id = 1,
        cause = cause,
        measure_name = MEASURE_DEATHS,
        age = ages,
        estimate = "central",
        val = seq_along(ages) * 100
      )
    }
  ))
  crfs <- specs %>%
    dplyr::transmute(
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
    load_crf_tabular = function(crf) compiled[[crf$cause]],
    get_rr = function(...) source,
    get_epi_count_long = function(...) fake_epi,
    get_epi_location_id = function(...) 1,
    {
      for (cause in specs$cause) {
        crf <- dplyr::filter(crfs, .data$cause == .env$cause)
        candidate_result <- apply_crf_tabular(
          crf,
          conc_base,
          conc_perm,
          pop,
          region_id = "TEST",
          epi_version = "test"
        )
        legacy_result <- country_paf_perm(
          pm.base = conc_base,
          pm.perm = conc_perm,
          pop = pop,
          region_id = "TEST",
          cause = cause,
          measure = MEASURE_DEATHS,
          rr_source = RR_FUSION,
          epi_version = "test"
        )

        candidate_values <- candidate_result %>%
          dplyr::select(low, central, high) %>%
          unlist() %>%
          unname()

        expect_equal(
          candidate_values,
          unname(legacy_result),
          tolerance = 1e-12,
          info = cause
        )
        expect_true(all(is.finite(candidate_values)), info = cause)
        expect_true(any(candidate_values != 0), info = cause)
      }
    }
  )
})


test_that("Fusion adult death candidates create valid Registry rows", {
  specs <- fusion_adult_deaths_specs()
  rows <- dplyr::bind_rows(lapply(
    seq_len(nrow(specs)),
    function(i) {
      fusion_deaths_registry_row(
        cause = specs$cause[[i]],
        crf_id = specs$crf_id[[i]],
        data_path = specs$data_path[[i]],
        notes = fusion_adult_deaths_notes(specs$cause[[i]])
      )
    }
  ))
  reference <- fusion_reference_row()

  expect_named(rows, CRF_REGISTRY_REQUIRED_COLUMNS)
  expect_equal(rows$cause, specs$cause)
  expect_true(all(rows$outcome == MEASURE_DEATHS))
  expect_true(all(!rows$double_counted))
  expect_true(all(rows$reference_id == "burnett_2022_fusion"))
  expect_equal(reference$doi, "10.1016/j.envres.2021.112245")
  expect_match(reference$notes, "Fusion Parameters Jul 7, 2021.csv", fixed = TRUE)
  expect_match(
    reference$notes,
    "LL Fusion Parameters theta eq 97.5.csv",
    fixed = TRUE
  )
  expect_match(reference$notes, "validation reference only", fixed = TRUE)
  expect_match(
    reference$notes,
    "does not establish regeneration",
    fixed = TRUE
  )
  expect_true(validate_crf_references(reference))
  expect_true(validate_crf_registry(rows, references = reference))
})


test_that("Fusion adult death rows are registered exactly once", {
  specs <- fusion_adult_deaths_specs()
  expected <- dplyr::bind_rows(lapply(
    seq_len(nrow(specs)),
    function(i) {
      fusion_deaths_registry_row(
        cause = specs$cause[[i]],
        crf_id = specs$crf_id[[i]],
        data_path = specs$data_path[[i]],
        notes = fusion_adult_deaths_notes(specs$cause[[i]])
      )
    }
  ))
  registered <- load_crf_registry() %>%
    dplyr::filter(crf_id %in% specs$crf_id) %>%
    dplyr::arrange(match(crf_id, specs$crf_id)) %>%
    tibble::as_tibble()

  expect_equal(nrow(registered), nrow(specs))
  expect_equal(registered, expected)

  registered_reference <- load_crf_references() %>%
    dplyr::filter(reference_id == "burnett_2022_fusion")
  expect_equal(nrow(registered_reference), 1)
  expect_equal(
    registered_reference$doi,
    "10.1016/j.envres.2021.112245"
  )
  expect_equal(registered_reference$notes, fusion_reference_row()$notes)
})
