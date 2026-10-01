test_that("migrated GEMM IHD 25+ tabular CRF matches legacy rr_gemm", {
  legacy <- readr::read_csv(
    get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  ) %>%
    dplyr::filter(cause == "IHD", age == "25+", source == "gemm") %>%
    dplyr::arrange(exposure) %>%
    dplyr::select(exposure, age, low, central, high) %>% 
    as_tibble()

  migrated <- readr::read_csv(
    get_hia_path(
      "crf/tabular/gemm_pm25_ihd_25plus_deaths_v1.csv",
      error_if_not_exists = TRUE
    ),
    col_types = readr::cols()
  ) %>% 
  as_tibble()

  expect_equal(migrated, legacy)
})


test_that("adult GEMM IHD compiler preserves every legacy age curve", {
  source <- readr::read_csv(
    get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  expected_ages <- c(AGE_ADULTS, AGE_ADULTS_SPLIT)

  compiled <- compile_gemm_ihd_deaths_data(source, ages = expected_ages)
  packaged <- readr::read_csv(
    get_hia_path(
      "crf/tabular/gemm_pm25_ihd_adult_deaths_v1.csv",
      error_if_not_exists = TRUE
    ),
    col_types = readr::cols()
  ) %>%
    tibble::as_tibble()
  legacy <- source %>%
    dplyr::filter(
      cause == "IHD",
      source == "gemm",
      age %in% expected_ages
    ) %>%
    dplyr::mutate(.age_order = match(age, expected_ages)) %>%
    dplyr::arrange(.age_order, exposure) %>%
    dplyr::select(exposure, age, low, central, high) %>%
    tibble::as_tibble()

  expect_equal(compiled, legacy)
  expect_equal(packaged, compiled)
  expect_equal(nrow(compiled), 39013)
  expect_setequal(unique(compiled$age), expected_ages)
  expect_true(all(dplyr::count(compiled, age)$n == 3001))

  calculation_ages <- deduplicate_adult_ages(unique(compiled$age))
  expect_equal(calculation_ages, AGE_ADULTS_SPLIT)
  expect_false(AGE_ADULTS %in% calculation_ages)
})


test_that("full adult GEMM candidate covers the six legacy death causes", {
  source <- readr::read_csv(
    get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  specs <- gemm_adult_deaths_specs()
  expected_causes <- c("NCD.LRI", "LRI", "COPD", "IHD", "LC", "Stroke")

  expect_equal(specs$cause, expected_causes)
  expect_false("LRI.child" %in% specs$cause)
  expect_equal(length(unique(specs$crf_id)), length(expected_causes))
  expect_error(
    compile_gemm_deaths_data(source, cause = "LRI.child"),
    "Unsupported GEMM adult death cause"
  )

  for (i in seq_len(nrow(specs))) {
    cause <- specs$cause[[i]]
    compiled <- compile_gemm_deaths_data(source, cause = cause)
    packaged <- readr::read_csv(
      get_hia_path(specs$data_path[[i]], error_if_not_exists = TRUE),
      col_types = readr::cols()
    ) %>%
      tibble::as_tibble()

    expect_equal(packaged, compiled, info = cause)
    expect_equal(nrow(packaged), 39013, info = cause)
    expect_setequal(
      unique(packaged$age),
      c(AGE_ADULTS, AGE_ADULTS_SPLIT)
    )
  }
})


test_that("adult GEMM IHD validation rejects incomplete or ambiguous curves", {
  valid <- tidyr::expand_grid(
    age = c("25+", "25-29"),
    exposure = c(0, 10)
  ) %>%
    dplyr::mutate(
      low = 1 + exposure / 200,
      central = 1 + exposure / 100,
      high = 1 + exposure / 50
    ) %>%
    dplyr::select(exposure, age, low, central, high)

  expect_true(
    validate_compiled_gemm_ihd(
      valid,
      expected_ages = c("25+", "25-29")
    )
  )

  expect_error(
    validate_compiled_gemm_ihd(
      dplyr::filter(valid, age == "25+"),
      expected_ages = c("25+", "25-29")
    ),
    "missing: 25-29"
  )

  expect_error(
    validate_compiled_gemm_ihd(
      dplyr::bind_rows(valid, dplyr::slice(valid, 1)),
      expected_ages = c("25+", "25-29")
    ),
    "duplicate age-exposure"
  )

  unsorted <- dplyr::bind_rows(
    dplyr::filter(valid, age == "25+") %>% dplyr::arrange(dplyr::desc(exposure)),
    dplyr::filter(valid, age == "25-29")
  )
  expect_error(
    validate_compiled_gemm_ihd(
      unsorted,
      expected_ages = c("25+", "25-29")
    ),
    "sorted within each age"
  )

  expect_error(
    validate_compiled_gemm_ihd(
      dplyr::filter(valid, !(age == "25-29" & exposure == 10)),
      expected_ages = c("25+", "25-29")
    ),
    "same exposure grid"
  )
})


test_that("adult GEMM Registry paths match legacy age-weighted PAFs", {
  source <- readr::read_csv(
    get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  )
  specs <- gemm_adult_deaths_specs()
  compiled <- stats::setNames(
    lapply(
      specs$cause,
      function(cause) compile_gemm_deaths_data(source, cause = cause)
    ),
    specs$cause
  )
  fake_epi <- tidyr::crossing(
    cause = specs$cause,
    age = AGE_ADULTS_SPLIT
  ) %>%
    dplyr::mutate(
      location_id = 1,
      measure_name = "Deaths",
      estimate = "central",
      val = match(age, AGE_ADULTS_SPLIT) * 100
    )
  registry_rows <- dplyr::bind_rows(lapply(
    seq_len(nrow(specs)),
    function(i) {
      gemm_deaths_registry_row(
        cause = specs$cause[[i]],
        crf_id = specs$crf_id[[i]],
        data_path = specs$data_path[[i]],
        notes = "Test candidate"
      )
    }
  ))
  crfs <- registry_rows %>%
    dplyr::select(crf_id, pollutant, cause, outcome, form, data_path)
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
        registry_result <- apply_crf_tabular(
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
          measure = "Deaths",
          rr_source = "gemm",
          epi_version = "test"
        )

        registry_values <- registry_result %>%
          dplyr::select(low, central, high) %>%
          unlist() %>%
          unname()

        expect_equal(
          registry_values,
          unname(legacy_result),
          tolerance = 1e-12,
          info = cause
        )
        expect_true(all(is.finite(registry_values)), info = cause)
        expect_true(any(registry_values != 0), info = cause)
      }
    }
  )
})


test_that("GEMM IHD compiler creates a complete Registry row", {
  row <- gemm_ihd_deaths_registry_row(
    crf_id = "gemm_pm25_ihd_adult_deaths_v1",
    data_path = "crf/tabular/gemm_pm25_ihd_adult_deaths_v1.csv",
    notes = "Candidate adult-age GEMM IHD curve"
  )

  expect_named(row, CRF_REGISTRY_REQUIRED_COLUMNS)
  expect_false(row$double_counted)
  expect_equal(row$form, CRF_FORM_TABULAR)
  expect_equal(row$reference_id, "burnett_2018_gemm")
})


test_that("full adult GEMM candidate creates valid Registry rows", {
  specs <- gemm_adult_deaths_specs()
  rows <- dplyr::bind_rows(lapply(
    seq_len(nrow(specs)),
    function(i) {
      gemm_deaths_registry_row(
        cause = specs$cause[[i]],
        crf_id = specs$crf_id[[i]],
        data_path = specs$data_path[[i]],
        notes = gemm_adult_deaths_notes(specs$cause[[i]])
      )
    }
  ))

  expect_named(rows, CRF_REGISTRY_REQUIRED_COLUMNS)
  expect_equal(rows$cause, specs$cause)
  expect_true(all(rows$outcome == "Deaths"))
  expect_true(all(!rows$double_counted))
  expect_true(validate_crf_registry(rows, references = gemm_reference_row()))
})
