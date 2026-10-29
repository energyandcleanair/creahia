test_that("CRF references load and validate correctly", {
  references <- load_crf_references(validate = FALSE)
  expect_true(is.data.frame(references))
  expect_true(validate_crf_references(references))
})

test_that("CRF registry load and validate correctly", {
  registry <- load_crf_registry(validate = FALSE)
  expect_true(is.data.frame(registry))
  expect_true(validate_crf_registry(registry))
})

test_that("CRF registry_id and reference_id match correctly", {
  registry <- load_crf_registry(validate = FALSE)
  references <- load_crf_references(validate = FALSE)

  expect_true(all(registry$reference_id %in% references$reference_id))
})

test_that("log-linear CRFs have required numeric fields", {
  registry <- load_crf_registry(validate = FALSE)

  log_linear <- registry %>% filter(form == CRF_FORM_LOG_LINEAR)

  expect_true(all(!is.na(log_linear$rr_central)))
  expect_true(all(!is.na(log_linear$rr_low)))
  expect_true(all(!is.na(log_linear$rr_high)))
  expect_true(all(!is.na(log_linear$conc_change)))
  expect_true(all(!is.na(log_linear$counterfact)))
  expect_true(all(!is.na(log_linear$units_multiplier)))

  expect_true(all(log_linear$rr_central > 0))
  expect_true(all(log_linear$rr_low > 0))
  expect_true(all(log_linear$rr_high > 0))
  expect_true(all(log_linear$conc_change > 0))
  expect_true(all(log_linear$counterfact >= 0))
  expect_true(all(log_linear$units_multiplier >= 0))

  expect_true(all(log_linear$rr_low <= log_linear$rr_central))
  expect_true(all(log_linear$rr_central <= log_linear$rr_high))
})

test_that("invalid CRF registry form values are caught", {
  registry <- load_crf_registry(validate = FALSE)
  registry$form[1] <- "invalid_form"

  expect_error(
    validate_crf_registry(registry), 
    "CRF registry file contains invalid form values: invalid_form. Valid values are 'log_linear' and 'tabular'."
  )

})

test_that("unknown reference_id fails validation", {
  registry <- load_crf_registry(validate = FALSE)
  registry$reference_id[1] <- "missing_reference"

  expect_error(
    validate_crf_registry(registry),
    "CRF registry file contains reference_ids that are not present in the references file: missing_reference"
  )
})

test_that("CRF definitions do not store calculation overlap decisions", {
  registry <- load_crf_registry()
  expect_false("double_counted" %in% names(registry))
  expect_false("double_counted" %in% CRF_REGISTRY_REQUIRED_COLUMNS)
  expect_no_error(validate_crf_registry(registry))
})

test_that("reviewed CRF references preserve formal citation metadata", {
  references <- load_crf_references()

  asthma <- references %>%
    dplyr::filter(
      reference_id == "achakulwisut_2019_paediatric_asthma_incidence"
    )
  gemm <- references %>%
    dplyr::filter(reference_id == "burnett_2018_gemm")

  expect_equal(
    asthma$title,
    paste(
      "Global, national, and urban burdens of paediatric asthma incidence",
      "attributable to ambient NO2 pollution: estimates from global datasets"
    )
  )
  expect_equal(asthma$journal, "The Lancet Planetary Health")
  expect_equal(gemm$url, "https://doi.org/10.1073/pnas.1803222115")
})

test_that("Registry rows can be upserted as one batch", {
  registry_path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(crf_id = c("keep", "replace"), value = c(1, 2)),
    registry_path
  )
  updates <- tibble::tibble(
    crf_id = c("replace", "add"),
    value = c(20, 30)
  )

  with_mocked_bindings(
    get_hia_path = function(...) registry_path,
    validate_crf_registry = function(...) invisible(TRUE),
    {
      expect_true(upsert_crf_registry_rows(updates))
    }
  )

  result <- readr::read_csv(registry_path, show_col_types = FALSE)
  expect_equal(result$crf_id, c("keep", "replace", "add"))
  expect_equal(result$value, c(1, 20, 30))
})

test_that("Registry batch upsert rejects duplicate incoming IDs", {
  duplicate_rows <- tibble::tibble(
    crf_id = c("duplicate", "duplicate")
  )

  expect_error(
    upsert_crf_registry_rows(duplicate_rows),
    "duplicate crf_ids: duplicate"
  )
})
