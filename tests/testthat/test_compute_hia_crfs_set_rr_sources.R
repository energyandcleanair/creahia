crfs_set_rr_fixture <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- list(
        conc_map = list(scen = list(IDN = data.frame(
          conc_baseline_pm25 = c(20, 30),
          conc_scenario_pm25 = c(25, 35),
          pop = c(1000, 2000)
        ))),
        regions = tibble::tibble(
          region_id = "IDN",
          region_name = "Indonesia",
          country_id = "IDN"
        ),
        epi = get_epi("default"),
        epi_long = get_epi_count_long("default"),
        crfs = crfs_set(presets = "gemm_adult_deaths_components")
      )
    }
    cache
  }
})


run_crfs_set_hia <- function(crfs, rr_sources, rr_sources_missing = FALSE) {
  fixture <- crfs_set_rr_fixture()
  call <- list(
    conc_map = fixture$conc_map,
    species = "pm25",
    regions = fixture$regions,
    epi = fixture$epi,
    crfs = crfs,
    diagnostic_folder = NULL
  )
  if (!rr_sources_missing) {
    call$rr_sources <- rr_sources
  }

  testthat::with_mocked_bindings(
    get_epi_count_long = function(...) fixture$epi_long,
    do.call(compute_hia, call),
    .package = "creahia"
  )
}


run_crfs_set_paf <- function(crfs, rr_sources = NULL) {
  fixture <- crfs_set_rr_fixture()

  testthat::with_mocked_bindings(
    get_epi_count_long = function(...) fixture$epi_long,
    compute_hia_paf(
      conc_map = fixture$conc_map,
      species = "pm25",
      regions = fixture$regions,
      rr_sources = rr_sources,
      crfs = crfs,
      diagnostic_folder = NULL
    ),
    .package = "creahia"
  )
}


run_default_crfs_set_comparison_hia <- function() {
  fixture <- crfs_set_rr_fixture()

  testthat::with_mocked_bindings(
    get_epi = function(...) fixture$epi,
    get_epi_count_long = function(...) fixture$epi_long,
    compute_hia(
      conc_map = fixture$conc_map,
      species = "pm25",
      regions = fixture$regions,
      diagnostic_folder = NULL
    ),
    .package = "creahia"
  )
}


arrange_crfs_set_impacts <- function(impacts) {
  identity_columns <- intersect(
    c(
      "scenario", "region_id", "pollutant", "cause", "outcome",
      "estimate", "age_group", "double_counted", "region_name", "iso3"
    ),
    names(impacts)
  )
  impacts %>% dplyr::arrange(dplyr::across(dplyr::all_of(identity_columns)))
}


expect_no_crfs_set_duplicate_rows <- function(paf, impacts) {
  paf_duplicates <- paf %>%
    dplyr::count(
      .data$scenario,
      .data$region_id,
      .data$pollutant,
      .data$cause,
      .data$outcome,
      name = "n"
    ) %>%
    dplyr::filter(.data$n > 1)
  impact_keys <- intersect(
    c(
      "scenario", "region_id", "pollutant", "cause", "outcome",
      "estimate", "age_group"
    ),
    names(impacts)
  )
  impact_duplicates <- impacts %>%
    dplyr::count(dplyr::across(dplyr::all_of(impact_keys)), name = "n") %>%
    dplyr::filter(.data$n > 1)

  expect_equal(nrow(paf_duplicates), 0L)
  expect_equal(nrow(impact_duplicates), 0L)
}


test_that("a crfs_set is a complete method Set when rr_sources is omitted", {
  fixture <- crfs_set_rr_fixture()
  paf <- run_crfs_set_paf(fixture$crfs)
  implicit <- run_crfs_set_hia(
    fixture$crfs,
    rr_sources = NULL,
    rr_sources_missing = TRUE
  )
  explicit_empty <- run_crfs_set_hia(
    fixture$crfs,
    tibble::tibble(cause = character(), source = character())
  )

  expect_no_crfs_set_duplicate_rows(paf, implicit)
  expect_equal(
    arrange_crfs_set_impacts(implicit),
    arrange_crfs_set_impacts(explicit_empty),
    tolerance = 1e-12
  )
})


test_that("an inspected Registry Set can be reused without duplicate RR rows", {
  inspection <- inspect_hia_configuration()
  inspected <- run_crfs_set_hia(
    inspection$registry_set,
    rr_sources = NULL,
    rr_sources_missing = TRUE
  )
  default <- run_default_crfs_set_comparison_hia()
  inspected_paf <- run_crfs_set_paf(inspection$registry_set)

  expect_no_crfs_set_duplicate_rows(inspected_paf, inspected)
  expect_equal(
    arrange_crfs_set_impacts(inspected),
    arrange_crfs_set_impacts(default),
    tolerance = 1e-12
  )
})


test_that("overlapping explicit RR sources are rejected before PAF calculation", {
  fixture <- crfs_set_rr_fixture()
  observed_messages <- character()
  error <- tryCatch(
    withCallingHandlers(
      run_crfs_set_hia(fixture$crfs, RR_GEMM),
      message = function(condition) {
        observed_messages <<- c(observed_messages, conditionMessage(condition))
      }
    ),
    error = function(error) error
  )

  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "overlapping HIA slots", fixed = TRUE)
  expect_match(conditionMessage(error), "PM25/IHD/Deaths", fixed = TRUE)
  expect_false(any(observed_messages == "Computing PAF"))
})


test_that("non-overlapping explicit RR sources are combined with a crfs_set", {
  fixture <- crfs_set_rr_fixture()
  yll_rr <- tibble::tibble(
    cause = "IHD",
    source = RR_GEMM,
    outcome = "YLLs"
  )
  paf <- run_crfs_set_paf(fixture$crfs, rr_sources = yll_rr)
  impacts <- run_crfs_set_hia(fixture$crfs, rr_sources = yll_rr)

  expect_no_crfs_set_duplicate_rows(paf, impacts)
  expect_true(any(paf$cause == "IHD" & paf$outcome == "Deaths"))
  expect_true(any(paf$cause == "IHD" & paf$outcome == "YLLs"))
})


test_that("empty RR source spellings all mean no RR selection", {
  fixture <- crfs_set_rr_fixture()
  expected <- run_crfs_set_hia(
    fixture$crfs,
    tibble::tibble(cause = character(), source = character())
  )
  null_result <- run_crfs_set_hia(fixture$crfs, NULL)
  c_result <- run_crfs_set_hia(fixture$crfs, c())
  character_result <- run_crfs_set_hia(fixture$crfs, character(0))

  expect_equal(arrange_crfs_set_impacts(null_result),
               arrange_crfs_set_impacts(expected), tolerance = 1e-12)
  expect_equal(arrange_crfs_set_impacts(c_result),
               arrange_crfs_set_impacts(expected), tolerance = 1e-12)
  expect_equal(arrange_crfs_set_impacts(character_result),
               arrange_crfs_set_impacts(expected), tolerance = 1e-12)
})


test_that("compute_hia_paf accepts all empty RR source spellings", {
  fixture <- crfs_set_rr_fixture()
  expected <- run_crfs_set_paf(
    fixture$crfs,
    rr_sources = tibble::tibble(cause = character(), source = character())
  )

  expect_equal(run_crfs_set_paf(fixture$crfs, NULL), expected, tolerance = 1e-12)
  expect_equal(run_crfs_set_paf(fixture$crfs, c()), expected, tolerance = 1e-12)
  expect_equal(
    run_crfs_set_paf(fixture$crfs, character(0)),
    expected,
    tolerance = 1e-12
  )
})


test_that("legacy cause selectors still combine only when slots do not overlap", {
  fixture <- crfs_set_rr_fixture()
  non_overlap <- testthat::with_mocked_bindings(
    get_epi_count_long = function(...) fixture$epi_long,
    compute_hia(
      conc_map = fixture$conc_map,
      species = "pm25",
      regions = fixture$regions,
      epi = fixture$epi,
      crfs = fixture$crfs,
      calc_causes = "IHD_YLLs",
      diagnostic_folder = NULL
    ),
    .package = "creahia"
  )

  expect_true(any(non_overlap$cause == "IHD" & non_overlap$outcome == "YLLs"))
  expect_error(
    compute_hia(
      conc_map = fixture$conc_map,
      species = "pm25",
      regions = fixture$regions,
      epi = fixture$epi,
      crfs = fixture$crfs,
      calc_causes = "IHD_Deaths",
      diagnostic_folder = NULL
    ),
    "PM25/IHD/Deaths",
    fixed = TRUE
  )
})
