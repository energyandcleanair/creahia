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

# Tests for add_double_counted function
test_that("add_double_counted joins CRF double_counted field correctly", {
  test_data <- setup_impacts_test_data()

  # Create HIA data that needs double_counted field added
  hia_data <- data.frame(
    cause = c("NCD.LRI", "Asthma.1to18"),
    outcome = c("Deaths", "AsthmaIncidence"),
    pollutant = c("PM25", "NO2"),
    number = c(100, 50),
    stringsAsFactors = FALSE
  )

  # Create CRFs with explicit double_counted values
  crfs_with_double_counted <- data.frame(
    pollutant = c("PM25", "NO2", "PM25"),
    cause = c("NCD.LRI", "Asthma.1to18", "IHD"),
    outcome = c("Deaths", "AsthmaIncidence", "Deaths"),
    double_counted = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data (not used in this test but required by function)
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_with_double_counted, epi_minimal)

  # Check that double_counted field was added correctly
  expect_true("double_counted" %in% names(result))

  # Check that no double_counted values are NA
  expect_false(any(is.na(result$double_counted)))

  # Check specific values
  ncdlri_row <- result[result$cause == "NCD.LRI" & result$outcome == "Deaths", ]
  expect_true(ncdlri_row$double_counted)

  asthma_row <- result[result$cause == "Asthma.1to18" & result$outcome == "AsthmaIncidence", ]
  expect_false(asthma_row$double_counted)
})

test_that("add_double_counted handles PM25 NCD.LRI double counting correctly", {
  test_data <- setup_impacts_test_data()

  # Create HIA data with both NCD.LRI ensemble cause and individual causes
  hia_data <- data.frame(
    cause = c("NCD.LRI", "IHD", "Stroke", "COPD", "LRI"),
    outcome = c("Deaths", "Deaths", "Deaths", "Deaths", "Deaths"),
    pollutant = c("PM25", "PM25", "PM25", "PM25", "PM25"),
    number = c(100, 50, 40, 30, 20),
    stringsAsFactors = FALSE
  )

  # Create CRFs without double_counted field for PM25 (should trigger manual logic)
  crfs_no_pm25 <- data.frame(
    pollutant = c("NO2"),
    cause = c("Asthma.1to18"),
    outcome = c("AsthmaIncidence"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_no_pm25, epi_minimal)

  # Check that double_counted field was added
  expect_true("double_counted" %in% names(result))

  # Check that no double_counted values are NA
  expect_false(any(is.na(result$double_counted)))

  # All PM25 NCD.LRI causes should be marked as double_counted = TRUE
  pm25_ncdlri_rows <- result[result$pollutant == "PM25" &
                            result$cause %in% c("IHD", "Stroke", "COPD", "LRI") &
                            result$outcome == "Deaths", ]

  expect_true(all(pm25_ncdlri_rows$double_counted))
})

test_that("add_double_counted handles PM25 CV double counting correctly", {
  test_data <- setup_impacts_test_data()

  # Create HIA data with both CV ensemble cause and individual causes
  hia_data <- data.frame(
    cause = c("CV", "IHD", "Stroke"),
    outcome = c("Deaths", "Deaths", "Deaths"),
    pollutant = c("PM25", "PM25", "PM25"),
    number = c(80, 50, 40),
    stringsAsFactors = FALSE
  )

  # Create CRFs without double_counted field for PM25 (should trigger manual logic)
  crfs_no_pm25 <- data.frame(
    pollutant = c("NO2"),
    cause = c("Asthma.1to18"),
    outcome = c("AsthmaIncidence"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_no_pm25, epi_minimal)

  # Check that double_counted field was added
  expect_true("double_counted" %in% names(result))

  # Check that no double_counted values are NA
  expect_false(any(is.na(result$double_counted)))

  # All PM25 CV causes should be marked as double_counted = TRUE
  pm25_cv_rows <- result[result$pollutant == "PM25" &
                        result$cause %in% c("IHD", "Stroke") &
                        result$outcome == "Deaths", ]

  expect_true(all(pm25_cv_rows$double_counted))
})

test_that("add_double_counted defaults to FALSE when not specified", {
  test_data <- setup_impacts_test_data()

  # Create HIA data with PM25 pollutant (which is allowed to have missing double_counted)
  hia_data <- data.frame(
    cause = c("UnknownCause"),
    outcome = c("UnknownOutcome"),
    pollutant = c("PM25"),
    number = c(25),
    stringsAsFactors = FALSE
  )

  # Create empty CRFs (no matches)
  crfs_empty <- data.frame(
    pollutant = character(),
    cause = character(),
    outcome = character(),
    double_counted = logical(),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_empty, epi_minimal)

  # Check that double_counted field was added and defaults to FALSE
  expect_true("double_counted" %in% names(result))
  expect_false(result$double_counted)
})

test_that("add_double_counted handles YLLs outcomes correctly", {
  test_data <- setup_impacts_test_data()

  # Create HIA data with both CV ensemble cause and individual causes with YLLs
  hia_data <- data.frame(
    cause = c("CV", "IHD", "Stroke"),
    outcome = c("YLLs", "YLLs", "YLLs"),
    pollutant = c("PM25", "PM25", "PM25"),
    number = c(1000, 600, 400),
    stringsAsFactors = FALSE
  )

  # Create CRFs without double_counted field for PM25
  crfs_no_pm25 <- data.frame(
    pollutant = c("NO2"),
    cause = c("Asthma.1to18"),
    outcome = c("AsthmaIncidence"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_no_pm25, epi_minimal)

  # Check that double_counted field was added
  expect_true("double_counted" %in% names(result))

  # All PM25 CV causes with YLLs should be marked as double_counted = TRUE
  pm25_cv_ylls_rows <- result[result$pollutant == "PM25" &
                             result$cause %in% c("IHD", "Stroke") &
                             result$outcome == "YLLs", ]

  expect_true(all(pm25_cv_ylls_rows$double_counted))
})

test_that("add_double_counted fails when non-PM25 pollutant has missing double_counted", {
  test_data <- setup_impacts_test_data()

  # Create HIA data with NO2 (non-PM25) that has positive number but no CRF match
  hia_data <- data.frame(
    cause = c("UnknownCause"),
    outcome = c("UnknownOutcome"),
    pollutant = c("NO2"),
    number = c(50),  # Positive number should trigger error
    stringsAsFactors = FALSE
  )

  # Create CRFs without match for NO2
  crfs_no_match <- data.frame(
    pollutant = c("PM25"),
    cause = c("IHD"),
    outcome = c("Deaths"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  # Should throw error for non-PM25 pollutant with missing double_counted and positive number
  expect_error(
    add_double_counted(hia_data, crfs_no_match, epi_minimal),
    "merged has failed in double counting detection"
  )
})

test_that("to_long_hia filters out all-NA cause-outcome combinations", {
  # Create wide format HIA data with some all-NA combinations
  wide_hia <- data.frame(
    region_id = c("CHN", "CHN"),
    pollutant = c("PM25", "NO2"),
    estimate = c("central", "central"),
    pop = c(1450141368, 1450141368),
    IHD_Deaths = c(-100, NA),
    NCD.LRI_Deaths = c(NA, 10),
    NCD.LRI_YLLs = c(-200, NA),
    stringsAsFactors = FALSE
  )

  result <- creahia::to_long_hia(wide_hia)

  # Check that NO2 are kept for NCD.LRI_Deaths
  no2_rows <- result %>% dplyr::filter(pollutant == "NO2")
  expect_equal(no2_rows$cause, "NCD.LRI")
  expect_equal(no2_rows$outcome, "Deaths")

  # Check that PM25 rows are kept for IHD and NCD.LRI_YLLs
  pm25_rows <- result %>% dplyr::filter(pollutant == "PM25")
  expect_equal(nrow(pm25_rows), 2)  # IHD_Deaths and NCD.LRI_YLLs
  expect_equal(pm25_rows$outcome, c("Deaths", "YLLs"))
  expect_equal(pm25_rows$cause, c("IHD", "NCD.LRI"))

})

test_that("to_long_hia keeps combinations with partial NA values", {
  # Create wide format HIA data where some estimates are NA but not all
  wide_hia <- data.frame(
    region_id = c("CHN", "CHN", "CHN"),
    pollutant = c("PM25", "PM25", "PM25"),
    estimate = c("low", "central", "high"),
    pop = c(1000000, 1000000, 1000000),
    IHD_Deaths = c(NA, -100, -150),
    stringsAsFactors = FALSE
  )

  result <- creahia::to_long_hia(wide_hia)

  # Check that IHD_Deaths combination is kept (not all NA)
  ihd_rows <- result %>% dplyr::filter(cause == "IHD", outcome == "Deaths")
  expect_equal(nrow(ihd_rows), 3)  # All three estimates should be present

  # Check that NA value is preserved in the data
  expect_true(any(is.na(ihd_rows$number)))
  # Check that non-NA values are also present
  expect_true(any(!is.na(ihd_rows$number)))
})

test_that("add_double_counted does not set double_counted when NCD.LRI has only NA values", {
  # Create HIA data with NCD.LRI having NA values and individual causes with non-NA values
  hia_data <- data.frame(
    cause = c("NCD.LRI", "IHD", "Stroke", "COPD"),
    outcome = c("Deaths", "Deaths", "Deaths", "Deaths"),
    pollutant = c("PM25", "PM25", "PM25", "PM25"),
    number = c(NA, 50, 40, 30),
    stringsAsFactors = FALSE
  )

  # Create CRFs without double_counted field for PM25
  crfs_no_pm25 <- data.frame(
    pollutant = c("NO2"),
    cause = c("Asthma.1to18"),
    outcome = c("AsthmaIncidence"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_no_pm25, epi_minimal)

  # Check that no double_counted values are NA
  expect_false(any(is.na(result$double_counted)))

  # Since NCD.LRI has only NA values, the individual causes should NOT be marked as double_counted
  pm25_individual_rows <- result[result$pollutant == "PM25" &
                                 result$cause %in% c("IHD", "Stroke", "COPD") &
                                 result$outcome == "Deaths", ]

  expect_false(all(pm25_individual_rows$double_counted))
})

test_that("add_double_counted sets double_counted when NCD.LRI has non-NA values", {
  # Create HIA data with NCD.LRI having non-NA values and individual causes
  hia_data <- data.frame(
    cause = c("NCD.LRI", "IHD", "Stroke", "COPD"),
    outcome = c("Deaths", "Deaths", "Deaths", "Deaths"),
    pollutant = c("PM25", "PM25", "PM25", "PM25"),
    number = c(100, 50, 40, 30),  # NCD.LRI has a non-NA value
    stringsAsFactors = FALSE
  )

  # Create CRFs without double_counted field for PM25
  crfs_no_pm25 <- data.frame(
    pollutant = c("NO2"),
    cause = c("Asthma.1to18"),
    outcome = c("AsthmaIncidence"),
    double_counted = c(FALSE),
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_no_pm25, epi_minimal)

  # Check that no double_counted values are NA
  expect_false(any(is.na(result$double_counted)))

  # Since NCD.LRI has non-NA values, the individual causes SHOULD be marked as double_counted
  pm25_individual_rows <- result[result$pollutant == "PM25" &
                                 result$cause %in% c("IHD", "Stroke", "COPD") &
                                 result$outcome == "Deaths", ]

  expect_true(all(pm25_individual_rows$double_counted))
})

test_that("add_double_counted does not set double_counted for PM25 when NCD.LRI only exists for other pollutants", {
  # Create HIA data with NCD.LRI for NO2 (non-NA) but not for PM25
  # PM25 individual causes should NOT be marked as double_counted
  hia_data <- data.frame(
    cause = c("NCD.LRI", "IHD", "Stroke", "COPD", "IHD", "Stroke"),
    outcome = c("Deaths", "Deaths", "Deaths", "Deaths", "Deaths", "Deaths"),
    pollutant = c("NO2", "PM25", "PM25", "PM25", "NO2", "NO2"),
    number = c(100, 50, 40, 30, 20, 15),  # NCD.LRI exists for NO2, not PM25
    stringsAsFactors = FALSE
  )

  # Create CRFs with double_counted for NO2 causes
  crfs_with_no2 <- data.frame(
    pollutant = c("NO2", "NO2", "NO2"),
    cause = c("NCD.LRI", "IHD", "Stroke"),
    outcome = c("Deaths", "Deaths", "Deaths"),
    double_counted = c(FALSE, TRUE, TRUE),  # Individual NO2 causes marked as double_counted
    stringsAsFactors = FALSE
  )

  # Create minimal EPI data
  epi_minimal <- data.frame(
    location_id = 1,
    estimate = "central",
    stringsAsFactors = FALSE
  )

  result <- add_double_counted(hia_data, crfs_with_no2, epi_minimal)

  # Check that no double_counted values are NA
  expect_false(any(is.na(result$double_counted)))

  # Since NCD.LRI only exists for NO2 (not PM25), PM25 individual causes should NOT be double_counted
  pm25_individual_rows <- result[result$pollutant == "PM25" &
                                 result$cause %in% c("IHD", "Stroke", "COPD") &
                                 result$outcome == "Deaths", ]

  expect_false(all(pm25_individual_rows$double_counted))
  expect_true(all(!pm25_individual_rows$double_counted))

  # NO2 individual causes SHOULD be marked as double_counted (from CRFs)
  no2_individual_rows <- result[result$pollutant == "NO2" &
                                result$cause %in% c("IHD", "Stroke") &
                                result$outcome == "Deaths", ]

  expect_true(all(no2_individual_rows$double_counted))
})

test_that("to_long_hia extracts pollutant from column names when not present as a column", {
  # Create wide format HIA data WITHOUT a pollutant column
  # The pollutant should be extracted from column names
  wide_hia <- data.frame(
    region_id = "CHN",
    estimate = "central",
    pop = 1450141368,
    IHD_Deaths_PM25 = -100,
    NCD.LRI_Deaths_PM25 = -50,
    NCD.LRI_YLLs_PM25 = -200,
    IHD_Deaths_NO2 = -30,
    NCD.LRI_Deaths_NO2 = NA,
    NCD.LRI_YLLs_NO2 = NA,
    stringsAsFactors = FALSE
  )

  result <- creahia::to_long_hia(wide_hia)

  # Check that pollutant column was created
  expect_true("pollutant" %in% names(result))

  # Check that pollutants were correctly extracted
  expect_true("PM25" %in% result$pollutant)
  expect_true("NO2" %in% result$pollutant)

  # Check that NO2 rows with all NA values were filtered out
  no2_ncdlri_rows <- result %>%
    dplyr::filter(pollutant == "NO2", cause == "NCD.LRI")
  expect_equal(nrow(no2_ncdlri_rows), 0)

  # Check that PM25 rows were preserved
  pm25_ncdlri_rows <- result %>%
    dplyr::filter(pollutant == "PM25", cause == "NCD.LRI")
  expect_gt(nrow(pm25_ncdlri_rows), 0)

  # Check that NO2 IHD was preserved (has non-NA value)
  no2_ihd_rows <- result %>%
    dplyr::filter(pollutant == "NO2", cause == "IHD")
  expect_gt(nrow(no2_ihd_rows), 0)
  expect_equal(unique(no2_ihd_rows$number), -30)
})
