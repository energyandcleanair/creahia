custom_pm25_allcause <- function(...) {
  crf_log_linear(
    "PM25", cause = "AllCause", outcome = "Deaths",
    rr = c(central = 1.08, low = 1.06, high = 1.09),
    per = 10, counterfactual = 0, ...
  )
}

test_that("crf_log_linear builds a custom log-linear row", {
  crf <- crf_log_linear(
    "PM2.5", cause = "AllCause", outcome = "Deaths",
    rr = c(low = 1.06, high = 1.09, central = 1.08),
    per = 10, counterfactual = 0, source = "Sensitivity run"
  )

  expect_true(is_custom_crf(crf))
  expect_equal(nrow(crf), 1)
  expect_setequal(names(crf), CRF_REGISTRY_REQUIRED_COLUMNS)
  expect_equal(crf$crf_id, "custom_pm25_allcause_deaths")
  expect_equal(crf$pollutant, "PM25")
  expect_equal(crf$form, CRF_FORM_LOG_LINEAR)
  expect_equal(c(crf$rr_low, crf$rr_central, crf$rr_high), c(1.06, 1.08, 1.09))
  expect_equal(crf$conc_change, 10)
  expect_equal(crf$counterfact, 0)
  expect_equal(crf$units_multiplier, 1)
  expect_true(is.na(crf$reference_id))
  expect_match(crf$notes, "not reviewed: Sensitivity run")
})

test_that("crf_log_linear takes unnamed RRs as central, low, high", {
  crf <- crf_log_linear(
    "NO2", cause = "NCD.LRI", outcome = "Deaths",
    rr = c(1.03, 1.01, 1.05), per = 10, counterfactual = 5, unit = "ppb"
  )

  expect_equal(c(crf$rr_central, crf$rr_low, crf$rr_high), c(1.03, 1.01, 1.05))
})

test_that("crf_log_linear takes ppb multipliers from the Registry", {
  registry <- load_crf_registry()
  ppb <- function(pollutant) {
    crf_log_linear(
      pollutant, cause = "NCD.LRI", outcome = "Deaths",
      rr = c(1.03, 1.01, 1.05), per = 10, counterfactual = 0, unit = "ppb"
    )$units_multiplier
  }

  for (pollutant in c("NO2", "O3_8h", "SO2")) {
    expected <- unique(registry$units_multiplier[
      registry$form == CRF_FORM_LOG_LINEAR &
        registry$pollutant == pollutant &
        registry$unit == "ppb"
    ])
    expect_equal(ppb(pollutant), expected)
  }

  expect_error(ppb("PM25"), "Provide `units_multiplier`")
  expect_equal(
    crf_log_linear(
      "PM25", cause = "AllCause", outcome = "Deaths",
      rr = c(1.08, 1.06, 1.09), per = 10, counterfactual = 0,
      unit = "ppb", units_multiplier = 0.5
    )$units_multiplier,
    0.5
  )
})

test_that("crf_log_linear rejects invalid parameters", {
  expect_error(custom_pm25_allcause(unit = "ppm"), "`unit` must be one of")
  expect_error(
    crf_log_linear("PM25", "AllCause", "Deaths", rr = c(1.08, 1.09, 1.06),
                   per = 10, counterfactual = 0),
    "low <= central <= high"
  )
  expect_error(
    crf_log_linear("PM25", "AllCause", "Deaths", rr = c(1.08, 1.06),
                   per = 10, counterfactual = 0),
    "three numbers"
  )
  expect_error(
    crf_log_linear("PM25", "AllCause", "Deaths",
                   rr = c(mid = 1.08, low = 1.06, high = 1.09),
                   per = 10, counterfactual = 0),
    "central, low and high"
  )
  expect_error(
    crf_log_linear("PM25", "AllCause", "Deaths", rr = c(1.08, 1.06, 1.09),
                   per = 0, counterfactual = 0),
    "`per` must be positive"
  )
  expect_error(
    crf_log_linear("PM25", "AllCause", "Deaths", rr = c(1.08, 1.06, 1.09), per = 10),
    "counterfactual"
  )
})

test_that("crfs_set adds a custom CRF without changing the Registry", {
  crfs <- crfs_set(
    presets = c("gbd2023", "crea_others_v1"),
    remove = list(list(
      pollutant = "PM25", outcome = "Deaths",
      cause = c("COPD", "Dementia", "Diabetes", "IHD", "LC", "LRI", "Stroke")
    )),
    add = list(custom_pm25_allcause())
  )

  pm25_deaths <- crfs %>%
    dplyr::filter(pollutant == "PM25", outcome == "Deaths")
  expect_setequal(pm25_deaths$cause, c("AllCause", "LRI.child"))
  expect_true("custom_pm25_allcause_deaths" %in% crfs$crf_id)
  expect_true(validate_crf_selection(crfs, check_double_counting = TRUE))

  expect_false("custom_pm25_allcause_deaths" %in% load_crf_registry()$crf_id)
  expect_false(any(startsWith(search_crf_registry()$crf_id, "custom_")))
})

test_that("crfs_set accepts a single custom CRF and previews it", {
  crfs <- suppressWarnings(
    crfs_set(presets = "gbd2023", add = custom_pm25_allcause())
  )
  expect_true("custom_pm25_allcause_deaths" %in% crfs$crf_id)

  preview <- preview_crf_set(presets = "gbd2023", add = list(custom_pm25_allcause()))
  added <- preview %>% dplyr::filter(action == "added")
  expect_equal(added$crf_id, "custom_pm25_allcause_deaths")
})

test_that("crfs_set replaces a slot with a custom CRF", {
  custom_ihd <- crf_log_linear(
    "PM25", cause = "IHD", outcome = "Deaths",
    rr = c(1.1, 1.0, 1.2), per = 10, counterfactual = 0
  )

  crfs <- crfs_set(presets = "gbd2023", replace = list(custom_ihd))
  ihd <- crfs %>% dplyr::filter(cause == "IHD")

  expect_equal(
    ihd$crf_id[ihd$outcome == "Deaths"],
    "custom_pm25_ihd_deaths"
  )
  expect_equal(
    ihd$crf_id[ihd$outcome == "YLLs"],
    "gbd2023_pm25_ihd_25plus_ylls_v1"
  )
  expect_error(
    crfs_set(presets = "gbd2023", add = list(custom_ihd)),
    "Use `replace` instead"
  )
})

test_that("crfs_set rejects two different custom CRFs with one id", {
  expect_error(
    crfs_set(
      presets = "gbd2023",
      replace = list(
        crf_log_linear("PM25", "IHD", "Deaths", rr = c(1.1, 1.0, 1.2),
                       per = 10, counterfactual = 0, id = "sensitivity"),
        crf_log_linear("PM25", "Stroke", "Deaths", rr = c(1.1, 1.0, 1.2),
                       per = 10, counterfactual = 0, id = "sensitivity")
      )
    ),
    "already used by another CRF"
  )
})

test_that("crfs_set warns when a custom CRF overlaps selected causes", {
  expect_warning(
    crfs_set(presets = "gbd2023", add = list(custom_pm25_allcause())),
    "AllCause includes COPD, Dementia, Diabetes, IHD, LC, LRI, Stroke"
  )

  # No warning for overlaps that only involve Registry rows
  expect_no_warning(crfs_set(presets = "gemm"))
})

test_that("a custom CRF gives the same HIA as the Registry row it copies", {
  registry_row <- load_crf_registry() %>%
    dplyr::filter(crf_id == "legacy_no2_ncdlri_deaths_v1")
  custom_copy <- crf_log_linear(
    "NO2", cause = "NCD.LRI", outcome = "Deaths",
    rr = c(
      central = registry_row$rr_central,
      low = registry_row$rr_low,
      high = registry_row$rr_high
    ),
    per = registry_row$conc_change,
    counterfactual = registry_row$counterfact,
    unit = registry_row$unit
  )

  conc_map <- list(
    scenario1 = list(
      BGD = tibble::tibble(
        conc_baseline_no2 = c(30, 35, 40),
        conc_scenario_no2 = c(20, 25, 30),
        pop = c(100, 200, 300)
      )
    )
  )
  regions <- tibble::tibble(
    region_id = "BGD",
    region_name = "Bangladesh",
    country_id = "BGD"
  )
  epi <- tibble::tibble(
    location_id = 1,
    estimate = c("low", "central", "high"),
    NCD.LRI_Deaths = c(80, 100, 120),
    pop = c(100000, 100000, 100000),
    country = "BGD",
    iso3 = "BGD"
  )

  run <- function(crfs) {
    with_mocked_bindings(
      get_epi_location_id = function(...) 1,
      diagnose_paf = function(...) invisible(NULL),
      compute_hia(
        conc_map = conc_map,
        species = "no2",
        regions = regions,
        epi = epi,
        crfs = crfs,
        diagnostic_folder = NULL
      )
    ) %>%
      dplyr::filter(pollutant == "NO2") %>%
      dplyr::arrange(estimate)
  }

  registry_hia <- run(crfs_set(presets = "experimental_default"))
  custom_hia <- run(crfs_set(
    presets = "experimental_default",
    replace = list(custom_copy)
  ))

  expect_gt(nrow(custom_hia), 0)
  expect_true(all(custom_hia$number != 0))
  expect_equal(custom_hia$number, registry_hia$number, tolerance = 1e-12)
  expect_equal(custom_hia$double_counted, registry_hia$double_counted)
})
