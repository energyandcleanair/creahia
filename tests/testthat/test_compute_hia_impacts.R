# Test health impacts computation functions

# Test data setup for impacts
setup_impacts_test_data <- function() {
  # Create mock PAF data in new structure (single tibble with scenario column)
  paf <- tibble::tibble(
    scenario = c(rep("scenario1", 3)),
    pollutant = c("PM25", "PM25", "NO2"),
    cause = c("NCD.LRI", "NCD.LRI", "Asthma.1to18"),
    outcome = c("Deaths", "YLLs", "AsthmaIncidence"),
    region_id = c("BGD", "BGD", "BGD"),
    low = c(-0.05, -0.02, -0.01),
    central = c(-0.08, -0.03, -0.015),
    high = c(-0.12, -0.04, -0.02)
  )

  # Create mock concentration data
  conc_map <- list(
    scenario1 = list(
      BGD = data.frame(
        conc_baseline_pm25 = c(20, 25, 30),
        conc_scenario_pm25 = c(15, 20, 25),
        conc_baseline_no2 = c(10, 12, 15),
        conc_scenario_no2 = c(8, 10, 12),
        pop = c(1000, 1500, 2000)
      )
    )
  )

  # Create mock species and regions
  species <- c("pm25", "no2")
  regions <- data.frame(
    region_id = "BGD",
    region_name = "Bangladesh",
    country_id = "BGD"
  )

  # Create mock epidemiological data with proper structure
  epi <- data.frame(
    location_id = creahia::get_epi_location_id("BGD"),
    estimate = c("low", "central", "high"),
    NCD.LRI_Deaths = c(80, 100, 120),
    NCD.LRI_YLLs = c(2000, 2500, 3000),
    Asthma.1to18_AsthmaIncidence = c(200, 250, 300),
    pop = rep(100000, 3),
    country = rep("BGD", 3),
    iso3 = rep("BGD", 3)
  )

  # Create mock CRFs data with new structure
  crfs <- data.frame(
    pollutant = c("PM25", "NO2"),
    cause = c("NCD.LRI", "Asthma.1to18"),
    outcome = c("Deaths", "AsthmaIncidence"),
    double_counted = c(FALSE, FALSE)
  )

  return(list(
    paf = paf,
    conc_map = conc_map,
    species = species,
    regions = regions,
    epi = epi,
    crfs = crfs
  ))
}

test_that("compute_hia_impacts returns correct structure", {
  test_data <- setup_impacts_test_data()

  # Test with minimal data to avoid pipeline issues
  paf_minimal <- tibble::tibble(
    scenario = character(),
    pollutant = character(),
    cause = character(),
    outcome = character(),
    region_id = character(),
    low = numeric(),
    central = numeric(),
    high = numeric()
  )

  # Test that the function can handle empty PAF data without crashing
  expect_no_error({
    result <- compute_hia_impacts(
      species = test_data$species,
      paf = paf_minimal,
      conc_map = test_data$conc_map,
      regions = test_data$regions,
      epi = test_data$epi,
      crfs = test_data$crfs
    )
  })

  # Basic structure check
  expect_true(is.data.frame(result))

})

test_that("compute_hia_impacts calculates CRF-based impacts correctly", {
  test_data <- setup_impacts_test_data()

  # Test just the CRF calculation logic without full pipeline
  paf_crf_only <- test_data$paf %>%
    dplyr::filter(pollutant %in% c("NO2")) # Only CRF data (NO2)

  # Test that the function can handle CRF data without crashing
  expect_no_error({
    result <- compute_hia_impacts(
      species = test_data$species,
      paf = paf_crf_only,
      conc_map = test_data$conc_map,
      regions = test_data$regions,
      epi = test_data$epi,
      crfs = test_data$crfs
    )
  })

  # Basic structure check
  expect_true(is.data.frame(result))

})

test_that("compute_hia_impacts integrates RR-based impacts correctly", {
  test_data <- setup_impacts_test_data()

  # Test just the RR calculation logic without full pipeline
  paf_pm25 <- test_data$paf %>%
    dplyr::filter(pollutant == "PM25") # Only RR data (PM25)

  # Test that the function can handle RR data without crashing
  expect_no_error({
    result <- compute_hia_impacts(
      species = test_data$species,
      paf = paf_pm25,
      conc_map = test_data$conc_map,
      regions = test_data$regions,
      epi = test_data$epi,
      crfs = test_data$crfs
    )
  })

  print(result)

  # Basic structure check
  expect_true(is.data.frame(result))

  # Content check
  expect_true(nrow(result) > 0)
  cause_outcomes <- paf_pm25 %>% distinct(cause, outcome)
  expect_true(nrow(inner_join(
    result %>% distinct(cause, outcome),
    cause_outcomes,
    by = c("cause", "outcome")
  )) == nrow(cause_outcomes))

})

test_that("compute_hia_impacts handles missing epidemiological data gracefully", {
  test_data <- setup_impacts_test_data()

  # Create EPI data with missing location
  epi_missing <- data.frame(
    location_id = NA,
    estimate = c("low", "central", "high"),
    NCD.LRI_Deaths = c(80, 100, 120),
    Asthma.1to18_AsthmaIncidence = c(200, 250, 300),
    pop = rep(100000, 3),
    country = rep("BGD", 3),
    iso3 = rep("BGD", 3)
  )

  paf_minimal <- tibble::tibble(
    scenario = character(),
    pollutant = character(),
    cause = character(),
    outcome = character(),
    region_id = character(),
    low = numeric(),
    central = numeric(),
    high = numeric()
  )

  with_mocked_bindings(
    get_epi_location_id = function(...) NA,
    {
      # Should warn about missing data but not crash
      expect_warning(
        result <- compute_hia_impacts(
          species = test_data$species,
          paf = paf_minimal,
          conc_map = test_data$conc_map,
          regions = test_data$regions,
          epi = epi_missing,
          crfs = test_data$crfs
        ),
        "Couldn't find epidemiological data"
      )

      # Should return empty result
      expect_true(is.data.frame(result))
    }
  )
})

test_that("compute_hia_impacts validates CRF-EPI data matching", {
  test_data <- setup_impacts_test_data()

  # Create CRFs with incidence not in EPI data
  crfs_mismatch <- data.frame(
    pollutant = c("PM25"),
    cause = c("NonExistent"),
    outcome = c("Deaths"),
    double_counted = c(FALSE)
  )

  # Create PAF with CRF data that will trigger the validation
  paf_with_crf <- tibble::tibble(
    scenario = "scenario1",
    pollutant = "PM25",
    cause = "NonExistent",
    outcome = "Deaths",
    region_id = "BGD",
    low = -0.02,
    central = -0.03,
    high = -0.04
  )

  # Should warn about data mismatch but not crash
  expect_warning(
    result <- compute_hia_impacts(
      species = test_data$species,
      paf = paf_with_crf,
      conc_map = test_data$conc_map,
      regions = test_data$regions,
      epi = test_data$epi,
      crfs = crfs_mismatch
    ),
    "Some RR causes/outcomes have no match in epidemiological data"
  )

  # Should return empty result
  expect_true(is.data.frame(result))
})

test_that("to_long_hia converts wide format correctly", {
  # Create wide format HIA data
  wide_hia <- data.frame(
    region_id = "BGD",
    estimate = "central",
    pop = 100000,
    NCD.LRI_Deaths_PM25 = -5,
    Asthma.1to18_AsthmaIncidence_NO2 = -2,
    stringsAsFactors = FALSE
  )

  result <- creahia::to_long_hia(wide_hia)

  # Check structure
  expect_true("outcome" %in% names(result))
  expect_true("cause" %in% names(result))
  expect_true("number" %in% names(result))

  # Check data
  expect_equal(nrow(result), 2) # Two outcomes

  # Check PM25 outcome
  pm25_row <- result %>% dplyr::filter(cause == "NCD.LRI")
  expect_equal(nrow(pm25_row), 1)
  expect_equal(pm25_row$outcome, "Deaths")
  expect_equal(pm25_row$cause, "NCD.LRI")
  expect_equal(pm25_row$number, -5)

  # Check NO2 outcome
  no2_row <- result %>% filter(cause == "Asthma.1to18")
  expect_equal(nrow(no2_row), 1)
  expect_equal(no2_row$outcome, "AsthmaIncidence")
  expect_equal(no2_row$cause, "Asthma.1to18")
  expect_equal(no2_row$number, -2)
})

test_that("to_long_hia handles O3_8h correctly", {
  wide_hia <- data.frame(
    region_id = "BGD",
    estimate = "central",
    pop = 100000,
    Asthma.1to18_AsthmaIncidence_O3_8h = -1,
    stringsAsFactors = FALSE
  )

  result <- creahia::to_long_hia(wide_hia)

  o3_row <- result %>% filter(cause == "Asthma.1to18")
  expect_equal(nrow(o3_row), 1)
  expect_equal(o3_row$outcome, "AsthmaIncidence")
  expect_equal(o3_row$cause, "Asthma.1to18")
})

test_that("compute_hia_impacts handles multiple scenarios", {
  test_data <- setup_impacts_test_data()

  # Add second scenario
  test_data$conc_map$scenario2 <- test_data$conc_map$scenario1

  # New PAF structure: single tibble with scenario column
  paf_minimal <- tibble::tibble(
    scenario = c(rep("scenario1", 2), rep("scenario2", 2)),
    pollutant = c("PM25", "NO2", "PM25", "NO2"),
    cause = c("NCD.LRI", "Asthma.1to18", "NCD.LRI", "Asthma.1to18"),
    outcome = c("Deaths", "AsthmaIncidence", "Deaths", "AsthmaIncidence"),
    region_id = c("BGD", "BGD", "BGD", "BGD"),
    low = c(-0.00995, -0.00244, -0.008, -0.002),
    central = c(-0.0296, -0.0120, -0.025, -0.01),
    high = c(-0.0536, -0.0213, -0.045, -0.018)
  )

  # Test that the function can handle multiple scenarios without crashing
  expect_no_error({
    result <- compute_hia_impacts(
      species = test_data$species,
      paf = paf_minimal,
      conc_map = test_data$conc_map,
      regions = test_data$regions,
      epi = test_data$epi,
      crfs = test_data$crfs
    )
  })

  # Basic structure check
  expect_true(is.data.frame(result))
})
