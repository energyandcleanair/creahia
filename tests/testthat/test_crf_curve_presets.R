curve_preset_fixture <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- list(
        conc_map = list(scen = list(IDN = data.frame(
          conc_baseline_pm25 = c(20, 45),
          conc_scenario_pm25 = c(15, 35),
          pop = c(1000, 2000)
        ))),
        regions = tibble::tibble(
          region_id = "IDN",
          region_name = "Indonesia",
          country_id = "IDN"
        ),
        epi = get_epi("default"),
        epi_long = get_epi_count_long("default")
      )
    }
    cache
  }
})


run_curve_preset_hia <- function(...) {
  fixture <- curve_preset_fixture()

  testthat::with_mocked_bindings(
    get_epi_count_long = function(...) fixture$epi_long,
    compute_hia(
      conc_map = fixture$conc_map,
      species = "pm25",
      regions = fixture$regions,
      epi = fixture$epi,
      diagnostic_folder = NULL,
      ...
    ),
    .package = "creahia"
  )
}


curve_preset_rows <- function(hia, causes) {
  hia %>%
    dplyr::filter(.data$pollutant == "PM25", .data$cause %in% causes) %>%
    dplyr::select(dplyr::any_of(c(
      "scenario", "region_id", "cause", "outcome", "estimate", "number",
      "double_counted"
    ))) %>%
    dplyr::arrange(.data$cause, .data$outcome, .data$estimate) %>%
    as.data.frame()
}


test_that("curve presets select one source per PM2.5 slot", {
  expected_rows <- c(gemm = 16L, fusion = 18L, gbd2019 = 16L)

  for (preset in names(expected_rows)) {
    selected <- crfs_set(presets = preset)

    expect_true(preset %in% available_crf_presets(), info = preset)
    expect_equal(nrow(selected), expected_rows[[preset]], info = preset)
    expect_true(all(selected$pollutant == "PM25"), info = preset)
    expect_true(all(selected$form == CRF_FORM_TABULAR), info = preset)
    expect_setequal(unique(selected$outcome), c("Deaths", "YLLs", "YLDs"))
    expect_true(validate_crf_selection(selected), info = preset)
  }

  expect_true("NCD.LRI" %in% crfs_set(presets = "gemm")$cause)
  expect_true("NCD.LRI" %in% crfs_set(presets = "fusion")$cause)
  expect_false("NCD.LRI" %in% crfs_set(presets = "gbd2019")$cause)
})


test_that("curve presets combine with crea_others_v1 in one Set", {
  for (preset in c("gemm", "fusion", "gbd2019")) {
    selected <- crfs_set(presets = c(preset, "crea_others_v1"))
    expect_equal(
      nrow(selected),
      nrow(crfs_set(presets = preset)) + 15L,
      info = preset
    )
  }
})


test_that("LRI.child curves are the adult LRI curves relabelled Under 5", {
  for (preset in c("gemm", "fusion", "gbd2019")) {
    selected <- crfs_set(presets = preset)
    read_curve <- function(cause) {
      path <- selected$data_path[
        selected$cause == cause & selected$outcome == "Deaths"
      ]
      readr::read_csv(
        get_hia_path(path, error_if_not_exists = TRUE),
        col_types = readr::cols()
      )
    }
    child <- read_curve("LRI.child")
    adult <- read_curve("LRI") %>% dplyr::filter(.data$age == "25+")

    expect_true(all(child$age == "Under 5"), info = preset)
    expect_equal(
      as.data.frame(dplyr::select(child, -age)),
      as.data.frame(dplyr::select(adult, -age)),
      info = preset
    )
  }
})


test_that("curve presets reproduce the matching rr_sources results", {
  sources <- c(gemm = RR_GEMM, fusion = RR_FUSION, gbd2019 = RR_GBD2019)

  for (preset in names(sources)) {
    causes <- unique(crfs_set(presets = preset)$cause)
    from_preset <- run_curve_preset_hia(crfs = crfs_set(presets = preset))
    from_rr_sources <- run_curve_preset_hia(rr_sources = sources[[preset]])

    expected <- curve_preset_rows(from_rr_sources, causes)
    expect_gt(nrow(expected), 0)
    expect_equal(
      curve_preset_rows(from_preset, causes),
      expected,
      info = preset
    )
  }
})


test_that("gemm + crea_others_v1 reproduces the default HIA apart from Diabetes", {
  # The default rr_sources take Diabetes from RR_ORIGINAL, which has no
  # public Registry row.
  from_preset <- run_curve_preset_hia(
    crfs = crfs_set(presets = c("gemm", "crea_others_v1"))
  )
  from_default <- run_curve_preset_hia()

  keep <- function(hia) {
    hia %>%
      dplyr::filter(.data$cause != CAUSE_DIABETES) %>%
      dplyr::select(dplyr::any_of(c(
        "scenario", "region_id", "pollutant", "cause", "outcome", "estimate",
        "number", "double_counted"
      ))) %>%
      dplyr::arrange(.data$pollutant, .data$cause, .data$outcome, .data$estimate) %>%
      as.data.frame()
  }

  expect_true(CAUSE_DIABETES %in% from_default$cause)
  expect_false(CAUSE_DIABETES %in% from_preset$cause)
  expect_equal(keep(from_preset), keep(from_default))
})
