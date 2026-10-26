crfs_set_double_counting_fixture <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- list(
        conc_map = list(scen = list(IDN = data.frame(
          conc_baseline_pm25 = c(20, 30),
          conc_scenario_pm25 = c(25, 35),
          conc_baseline_no2 = c(10, 12),
          conc_scenario_no2 = c(11, 14),
          pop = c(1000, 2000)
        ))),
        regions = tibble::tibble(
          region_id = "IDN",
          region_name = "Indonesia",
          country_id = "IDN"
        ),
        epi = get_epi("default")
      )
    }
    cache
  }
})


run_crfs_set_double_counting_hia <- function(
    crfs,
    rr_sources = NULL,
    rr_sources_supplied = FALSE,
    species = "pm25") {
  fixture <- crfs_set_double_counting_fixture()
  call <- list(
    conc_map = fixture$conc_map,
    species = species,
    regions = fixture$regions,
    epi = fixture$epi,
    crfs = crfs,
    diagnostic_folder = NULL
  )
  if (rr_sources_supplied) {
    call$rr_sources <- rr_sources
  }

  do.call(compute_hia, call)
}


death_double_counting <- function(impacts, target_pollutant = "PM25") {
  impacts %>%
    dplyr::filter(
      .data$pollutant == target_pollutant,
      .data$outcome == "Deaths",
      .data$estimate == "central"
    ) %>%
    dplyr::select(cause, double_counted) %>%
    dplyr::arrange(.data$cause)
}


expected_ncdlri_overlap_flags <- function() {
  tibble::tibble(
    cause = c("COPD", "IHD", "LC", "LRI", "NCD.LRI", "Stroke"),
    double_counted = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
}


test_that("a Set plus non-overlapping aggregate RR marks component impacts", {
  impacts <- run_crfs_set_double_counting_hia(
    crfs_set(presets = "gemm_adult_deaths_components"),
    rr_sources = list(gemm = "NCD.LRI"),
    rr_sources_supplied = TRUE
  )

  expect_equal(death_double_counting(impacts), expected_ncdlri_overlap_flags())
})


test_that("a Set containing aggregate and components marks component impacts", {
  impacts <- run_crfs_set_double_counting_hia(
    crfs_set(presets = c(
      "gemm_adult_deaths_aggregate",
      "gemm_adult_deaths_components"
    ))
  )

  expect_equal(death_double_counting(impacts), expected_ncdlri_overlap_flags())
})


test_that("Set aggregate/component flags match the legacy GEMM route", {
  set_impacts <- run_crfs_set_double_counting_hia(
    crfs_set(presets = c(
      "gemm_adult_deaths_aggregate",
      "gemm_adult_deaths_components"
    ))
  )
  fixture <- crfs_set_double_counting_fixture()
  legacy_impacts <- compute_hia(
    conc_map = fixture$conc_map,
    species = "pm25",
    regions = fixture$regions,
    epi = fixture$epi,
    rr_sources = c("gemm"),
    diagnostic_folder = NULL
  )

  comparison_causes <- expected_ncdlri_overlap_flags()$cause
  expect_equal(
    death_double_counting(set_impacts) %>%
      dplyr::filter(.data$cause %in% comparison_causes),
    death_double_counting(legacy_impacts) %>%
      dplyr::filter(.data$cause %in% comparison_causes)
  )
})


test_that("a component-only Set does not mark components as double-counted", {
  impacts <- run_crfs_set_double_counting_hia(
    crfs_set(presets = "gemm_adult_deaths_components")
  )

  expect_true(all(!death_double_counting(impacts)$double_counted))
})


test_that("an aggregate-only Set remains available for reporting", {
  impacts <- run_crfs_set_double_counting_hia(
    crfs_set(presets = "gemm_adult_deaths_aggregate")
  )

  expect_equal(
    death_double_counting(impacts),
    tibble::tibble(cause = "NCD.LRI", double_counted = FALSE)
  )
})


test_that("Set double-counting groups do not cross pollutants", {
  impacts <- run_crfs_set_double_counting_hia(
    crfs_set(presets = "experimental_default"),
    species = c("pm25", "no2")
  )

  expect_equal(
    death_double_counting(impacts, target_pollutant = "PM25"),
    tibble::tibble(cause = "IHD", double_counted = FALSE)
  )
  expect_equal(
    death_double_counting(impacts, target_pollutant = "NO2"),
    tibble::tibble(cause = "NCD.LRI", double_counted = FALSE)
  )
})





test_that("a filtered public Set still marks component impacts", {
  filtered_set <- crfs_set(presets = c(
    "gemm_adult_deaths_aggregate",
    "gemm_adult_deaths_components"
  )) %>%
    dplyr::filter(.data$cause != "LRI") %>%
    dplyr::mutate(analysis_note = "kept")

  impacts <- run_crfs_set_double_counting_hia(filtered_set)

  expect_equal(
    death_double_counting(impacts),
    expected_ncdlri_overlap_flags() %>%
      dplyr::filter(.data$cause != "LRI")
  )
})


test_that("editing an inspected Set rebuilds its overlap decisions", {
  inspected <- suppressMessages(inspect_hia_configuration(
    rr_sources = list(gemm = c("IHD", "Stroke", "COPD", "LC", "LRI"))
  ))$registry_set
  expanded <- dplyr::bind_rows(
    inspected, crfs_set("gemm_adult_deaths_aggregate")
  )
  expect_equal(
    death_double_counting(run_crfs_set_double_counting_hia(expanded)),
    expected_ncdlri_overlap_flags()
  )
  filtered <- dplyr::filter(expanded, cause != "NCD.LRI")
  expect_true(all(!death_double_counting(
    run_crfs_set_double_counting_hia(filtered)
  )$double_counted))
  expect_false("double_counted" %in% names(inspected))
})
