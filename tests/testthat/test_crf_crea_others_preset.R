test_that("CREA other functions are exposed as an opt-in preset", {
  selected <- crfs_set(presets = "crea_others_v1")

  expect_true("crea_others_v1" %in% available_crf_presets())
  expect_equal(nrow(selected), 15L)
  expect_true(all(selected$form == CRF_FORM_LOG_LINEAR))
  expect_true(validate_crf_selection(selected, check_double_counting = TRUE))
})


test_that("CREA other functions combine with an RR curve preset in one Set", {
  selected <- crfs_set(
    presets = c("gemm_adult_deaths_components", "crea_others_v1")
  )

  expect_equal(nrow(selected), 20L)
  expect_setequal(unique(selected$form), c(CRF_FORM_LOG_LINEAR, CRF_FORM_TABULAR))
  expect_true(validate_crf_selection(selected, check_double_counting = TRUE))
})


test_that("crea_others_v1 preset reproduces the 15 legacy default CRF PAF rows", {
  # Public rows copy the historical default parameters exactly, so the
  # legacy default fingerprint applies unchanged.
  historical <- resolve_historical_default_crfs()
  public <- crfs_set(presets = "crea_others_v1")
  parameters <- c(
    "pollutant", "cause", "outcome", "rr_central", "rr_low", "rr_high",
    "conc_change", "counterfact", "unit", "units_multiplier"
  )
  key <- c("pollutant", "cause", "outcome")
  expect_equal(
    public %>% dplyr::select(dplyr::all_of(parameters)) %>% dplyr::arrange(dplyr::across(dplyr::all_of(key))) %>% as.data.frame(),
    historical %>% dplyr::select(dplyr::all_of(parameters)) %>% dplyr::arrange(dplyr::across(dplyr::all_of(key))) %>% as.data.frame()
  )

  actual <- compute_hia_paf_crfs_registry(
    species = c("pm25", "no2", "o3_8h", "so2"),
    conc_map = legacy_default_fingerprint_conc_map(),
    regions = data.frame(region_id = "fingerprint_region"),
    crfs = public
  )$legacy_default_baseline %>%
    dplyr::arrange(.data$pollutant, .data$cause, .data$outcome)

  expect_equal(nrow(actual), 15L)
  expect_equal(
    as.data.frame(actual),
    as.data.frame(read_legacy_default_paf_fingerprint()),
    tolerance = 1e-12
  )
})
