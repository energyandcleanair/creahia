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

  log_linear <- registry %>% filter(form == "log_linear")

  expect_true(all(!is.na(log_linear$rr_central)))
  expect_true(all(!is.na(log_linear$rr_low)))
  expect_true(all(!is.na(log_linear$rr_high)))
  expect_true(all(!is.na(log_linear$conc_ref)))
  expect_true(all(!is.na(log_linear$counterfact)))
  expect_true(all(!is.na(log_linear$units_multiplier)))

  expect_true(all(log_linear$rr_central > 0))
  expect_true(all(log_linear$rr_low > 0))
  expect_true(all(log_linear$rr_high > 0))
  expect_true(all(log_linear$conc_ref > 0))
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