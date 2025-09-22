# Test PAF computation functions

# Test data setup
setup_test_data <- function() {
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
  
  # Create mock species
  species <- c("pm25", "no2")
  
  # Create mock regions
  regions <- data.frame(
    region_id = "BGD",
    region_name = "Bangladesh",
    country_id = "BGD"
  )
  
  # Create mock CRFs data
  crfs <- data.frame(
    Exposure = c("PM25", "NO2"),
    Incidence = c("NCD.LRI_Deaths", "Asthma.Inci.1to18"),
    effectname = c("NCD.LRI_Deaths_PM25", "Asthma.Inci.1to18_NO2"),
    Counterfact = c(5.8, 0),
    Conc.change = c(10, 10),
    Units.multiplier = c(1, 1),
    low = c(1.02, 1.01),
    central = c(1.06, 1.05),
    high = c(1.11, 1.09),
    Double.Counted = c(FALSE, FALSE)
  )
  
  # Create mock RR sources
  rr_sources <- c("GBD")
  
  return(list(
    conc_map = conc_map,
    species = species,
    regions = regions,
    crfs = crfs,
    rr_sources = rr_sources
  ))
}

test_that("compute_hia_paf_crfs returns correct structure", {
  test_data <- setup_test_data()
  
  # Mock the get_crfs function to return our test data
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    {
      result <- compute_hia_paf_crfs(
        species = test_data$species,
        conc_map = test_data$conc_map,
        regions = test_data$regions,
        crfs = test_data$crfs
      )
      
      # Check structure
      expect_true(is.list(result))
      expect_true("scenario1" %in% names(result))
      
      scenario_result <- result$scenario1
      expect_true(is.data.frame(scenario_result))
      expect_true("region_id" %in% names(scenario_result))
      expect_true("estimate" %in% names(scenario_result))
      expect_true("pop" %in% names(scenario_result))
      
      # Check estimates are present
      expect_equal(unique(scenario_result$estimate), c("low", "central", "high"))
      
      # Check PAF columns are present
      paf_cols <- names(scenario_result)[grepl("PM25|NO2", names(scenario_result))]
      expect_true(length(paf_cols) > 0)
    }
  )
})

test_that("compute_hia_paf_crfs calculates PAF correctly", {
  test_data <- setup_test_data()
  
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    {
      result <- compute_hia_paf_crfs(
        species = test_data$species,
        conc_map = test_data$conc_map,
        regions = test_data$regions,
        crfs = test_data$crfs
      )
      
      scenario_result <- result$scenario1
      
      # Check that PAF values are between 0 and 1 (for protective effects, they can be negative)
      paf_cols <- names(scenario_result)[grepl("PM25|NO2", names(scenario_result))]
      
      for(col in paf_cols) {
        paf_values <- scenario_result[[col]]
        # PAF should be finite numbers
        expect_true(all(is.finite(paf_values)))
        # PAF should not be extremely large (sanity check)
        expect_true(all(abs(paf_values) < 10))
      }
    }
  )
})

test_that("compute_hia_paf_crfs handles different estimates correctly", {
  test_data <- setup_test_data()
  
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    {
      result <- compute_hia_paf_crfs(
        species = test_data$species,
        conc_map = test_data$conc_map,
        regions = test_data$regions,
        crfs = test_data$crfs
      )
      
      scenario_result <- result$scenario1
      
      # Check that we have exactly 3 rows per region (one for each estimate)
      region_counts <- table(scenario_result$region_id)
      expect_true(all(region_counts == 3))
      
      # Check that estimates are properly ordered
      for(region in unique(scenario_result$region_id)) {
        region_data <- scenario_result[scenario_result$region_id == region, ]
        expect_equal(region_data$estimate, c("low", "central", "high"))
      }
    }
  )
})

test_that("compute_hia_paf_rr_curves returns correct structure", {
  test_data <- setup_test_data()
  
  # Mock required functions
  with_mocked_bindings(
    get_ihme = function(...) data.frame(
      location_id = 1,
      cause_short = "NCD.LRI",
      measure_name = "Deaths",
      age = "25+",
      estimate = "central",
      val = 100
    ),
    get_cause_source = function(...) data.frame(
      cause = "NCD.LRI",
      measure = "Deaths",
      source = "GBD"
    ),
    get_rr = function(...) data.frame(
      cause = "NCD.LRI",
      age = "25+",
      exposure = c(10, 20, 30),
      low = c(1.0, 1.5, 2.0),
      central = c(1.0, 1.6, 2.1),
      high = c(1.0, 1.7, 2.2)
    ),
    get_paf_from_rr_lauri = function(...) c(low = -0.1, central = -0.15, high = -0.2),
    get_epi_location_id = function(...) 1,
    {
      result <- compute_hia_paf_rr_curves(
        conc_map = test_data$conc_map,
        epi_version = "gbd2019",
        ihme_version = "gbd2019",
        rr_sources = test_data$rr_sources,
        scenarios = names(test_data$conc_map)
      )
      
      # Check structure
      expect_true(is.list(result))
      expect_true("scenario1" %in% names(result))
      
      scenario_result <- result$scenario1
      expect_true(is.data.frame(scenario_result))
      expect_true("var" %in% names(scenario_result))
      expect_true("region_id" %in% names(scenario_result))
    }
  )
})

test_that("compute_hia_paf coordinator function works", {
  test_data <- setup_test_data()
  
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    get_ihme = function(...) data.frame(
      location_id = 1,
      cause_short = "NCD.LRI",
      measure_name = "Deaths",
      age = "25+",
      estimate = "central",
      val = 100
    ),
    get_cause_source = function(...) data.frame(
      cause = "NCD.LRI",
      measure = "Deaths",
      source = "GBD"
    ),
    get_rr = function(...) data.frame(
      cause = "NCD.LRI",
      age = "25+",
      exposure = c(10, 20, 30),
      low = c(1.0, 1.5, 2.0),
      central = c(1.0, 1.6, 2.1),
      high = c(1.0, 1.7, 2.2)
    ),
    get_paf_from_rr_lauri = function(...) c(low = -0.1, central = -0.15, high = -0.2),
    get_epi_location_id = function(...) 1,
    {
      result <- compute_hia_paf(
        conc_map = test_data$conc_map,
        species = test_data$species,
        regions = test_data$regions,
        scenarios = names(test_data$conc_map),
        rr_sources = test_data$rr_sources,
        crfs = test_data$crfs
      )
      
      # Check structure
      expect_true(is.list(result))
      expect_true("rr" %in% names(result))
      expect_true("crf" %in% names(result))
      
      # Check RR-based PAF
      expect_true(is.list(result$rr))
      expect_true("scenario1" %in% names(result$rr))
      
      # Check CRF-based PAF
      expect_true(is.list(result$crf))
      expect_true("scenario1" %in% names(result$crf))
    }
  )
})

test_that("compute_hia_paf handles empty rr_sources", {
  test_data <- setup_test_data()
  
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    {
      result <- compute_hia_paf(
        conc_map = test_data$conc_map,
        species = test_data$species,
        regions = test_data$regions,
        scenarios = names(test_data$conc_map),
        rr_sources = c(),  # Empty RR sources
        crfs = test_data$crfs
      )
      
      # Should only have CRF results
      expect_true(is.list(result))
      expect_true("crf" %in% names(result))
      expect_false("rr" %in% names(result))
      
      # CRF results should still be present
      expect_true(is.list(result$crf))
      expect_true("scenario1" %in% names(result$crf))
    }
  )
})

test_that("PAF calculations handle edge cases", {
  test_data <- setup_test_data()
  
  # Test with zero concentrations
  conc_map_zero <- list(
    scenario1 = list(
      BGD = data.frame(
        conc_baseline_pm25 = c(0, 0, 0),
        conc_scenario_pm25 = c(0, 0, 0),
        conc_baseline_no2 = c(0, 0, 0),
        conc_scenario_no2 = c(0, 0, 0),
        pop = c(1000, 1500, 2000)
      )
    )
  )
  
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    {
      result <- compute_hia_paf_crfs(
        species = test_data$species,
        conc_map = conc_map_zero,
        regions = test_data$regions,
        crfs = test_data$crfs
      )
      
      # Should handle zero concentrations gracefully
      expect_true(is.list(result))
      expect_true("scenario1" %in% names(result))
      
      scenario_result <- result$scenario1
      paf_cols <- names(scenario_result)[grepl("PM25|NO2", names(scenario_result))]
      
      for(col in paf_cols) {
        paf_values <- scenario_result[[col]]
        expect_true(all(is.finite(paf_values)))
      }
    }
  )
})

test_that("PAF calculations are mathematically consistent", {
  test_data <- setup_test_data()
  
  with_mocked_bindings(
    get_crfs = function(...) test_data$crfs,
    {
      result <- compute_hia_paf_crfs(
        species = test_data$species,
        conc_map = test_data$conc_map,
        regions = test_data$regions,
        crfs = test_data$crfs
      )
      
      scenario_result <- result$scenario1
      
      # For PM25, manually calculate expected PAF
      # Using the formula: 1 - exp(-log(RR) * delta_conc / conc_change)
      # With RR = 1.06, delta_conc = -5, conc_change = 10
      expected_paf_pm25 <- 1 - exp(-log(1.06) * (-5) / 10)
      
      # Find PM25 PAF values
      pm25_col <- names(scenario_result)[grepl("PM25", names(scenario_result))]
      if(length(pm25_col) > 0) {
        pm25_paf <- scenario_result[[pm25_col[1]]]
        # Should be close to expected value (within 10% tolerance)
        expect_true(abs(pm25_paf[2] - expected_paf_pm25) / abs(expected_paf_pm25) < 0.1)
      }
    }
  )
})

# Test RR-based PAF computation (moved from test_get_paf_from_rr.R)
test_that("RR-based PAF computation from rr is correct", {

  # Define parameters for the test
  n_pixels <- 4
  n_agegroups <- 2
  estimates <- c("low", "central", "high")

  # Initialize rr_base and rr_perm as 3D arrays
  # Dimensions: [pixel, estimate, agegroup]
  rr_base <- array(1, dim = c(n_pixels, 3, n_agegroups),
                   dimnames = list(
                     pixel = paste0("pixel", 1:n_pixels),
                     estimate = estimates,
                     agegroup = paste0("agegroup", 1:n_agegroups)
                   ))

  rr_perm <- array(1, dim = c(n_pixels, 3, n_agegroups),
                   dimnames = list(
                     pixel = paste0("pixel", 1:n_pixels),
                     estimate = estimates,
                     agegroup = paste0("agegroup", 1:n_agegroups)
                   ))

  # Assign unique RR_base and RR_perm values for each pixel and agegroup
  # Pixel 1
  rr_base[1, , 1] <- 2.0
  rr_base[1, , 2] <- 3.0

  rr_perm[1, , 1] <- 1.5
  rr_perm[1, , 2] <- 2.5

  # Pixel 2
  rr_base[2, , 1] <- 1.8
  rr_base[2, , 2] <- 2.2

  rr_perm[2, , 1] <- 1.2
  rr_perm[2, , 2] <- 1.6

  # Pixel 3
  rr_base[3, , 1] <- 1.5
  rr_base[3, , 2] <- 2.5

  rr_perm[3, , 1] <- 1.0
  rr_perm[3, , 2] <- 2.0

  # Pixel 4
  rr_base[4, , 1] <- 2.2
  rr_base[4, , 2] <- 3.3

  rr_perm[4, , 1] <- 1.7
  rr_perm[4, , 2] <- 2.8

  # Define age_weights and population per pixel
  age_weights <- c(0.2, 0.8) # Weights for two age groups
  pop <- c(1000, 2000, 1500, 2500) # Population for 4 pixels

  # Compute expected PAF per pixel
  # PAF_i = sum_over_agegroups(PAF_ig * age_weights_norm)

  # For simplicity, since age_weights_norm = age_weights / sum(age_weights) = c(0.5, 0.5)

  # Calculate per-pixel PAF
  # Pixel 1
  paf1_age1 <- (1.5 / 2.0) - 1 # -0.25
  paf1_age2 <- (2.5 / 3.0) - 1 # -0.1666667
  paf1 <- age_weights[1] * paf1_age1 + age_weights[2] * paf1_age2 # -0.2166667

  # Pixel 2
  paf2_age1 <- (1.2 / 1.8) - 1 # -0.3333333
  paf2_age2 <- (1.6 / 2.2) - 1 # -0.2727273
  paf2 <- age_weights[1] * paf2_age1 + age_weights[2] * paf2_age2 # -0.3030303

  # Pixel 3
  paf3_age1 <- (1.0 / 1.5) - 1 # -0.3333333
  paf3_age2 <- (2.0 / 2.5) - 1 # -0.2
  paf3 <- age_weights[1] * paf3_age1 + age_weights[2] * paf3_age2 # -0.2666667

  # Pixel 4
  paf4_age1 <- (1.7 / 2.2) - 1 # ≈ -0.2272727
  paf4_age2 <- (2.8 / 3.3) - 1 # ≈ -0.1515152
  paf4 <- age_weights[1] * paf4_age1 + age_weights[2] * paf4_age2 # ≈ -0.1893939

  # Compute overall expected PAF as population-weighted average
  total_pop <- sum(pop)
  paf_overall_expected <- (
    paf1 * pop[1] +
      paf2 * pop[2] +
      paf3 * pop[3] +
      paf4 * pop[4]
  ) / total_pop

  # Compute the expected PAF triplet (low, central, high)
  # Since there's no uncertainty, all should be equal to paf_overall_expected
  expected_paf_triplet <- c(
    low = paf_overall_expected,
    central = paf_overall_expected,
    high = paf_overall_expected
  )

  # Run the get_paf_from_rr function
  paf_bootstrap <- creahia::get_paf_from_rr_boostrap(rr_base, rr_perm, age_weights, pop, n_boot = 1000, ci_level = 0.95, seed = 123)
  paf_delta <- get_paf_from_rr_delta(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)

  # Comparison
  testthat::expect_equal(paf_bootstrap, expected_paf_triplet)
  testthat::expect_equal(paf_delta, expected_paf_triplet)

  testthat::expect_equal(names(paf_bootstrap), c("low", "central", "high"))
  testthat::expect_equal(names(paf_delta), c("low", "central", "high"))

  testthat::expect_equal(length(paf_bootstrap), 3)
  testthat::expect_equal(length(paf_delta), 3)

  # Now introduce some uncertainty
  rr_perm[,1,] <- rr_perm[,1,] - 0.1
  rr_perm[,3,] <- rr_perm[,3,] + 0.1

  paf_bootstrap <- get_paf_from_rr_boostrap(rr_base, rr_perm, age_weights, pop, n_boot = 1000, ci_level = 0.95, seed = 123)
  paf_delta <- get_paf_from_rr_delta(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)

  testthat::expect_true(paf_bootstrap["low"] < paf_bootstrap["central"] & paf_bootstrap["central"] < paf_bootstrap["high"])
  testthat::expect_true(paf_delta["low"] < paf_delta["central"] & paf_delta["central"] < paf_delta["high"])

  # Check that paf_delta is less confident than paf_bootstrap
  testthat::expect_true(paf_delta["high"] - paf_delta["low"] > paf_bootstrap["high"] - paf_bootstrap["low"])

  # Check that it isn't too far off though
  testthat::expect_true(abs((paf_delta["low"] - paf_bootstrap["low"])/paf_bootstrap["low"]) < 0.1)
  testthat::expect_true(abs((paf_delta["high"] - paf_bootstrap["high"])/paf_bootstrap["high"]) < 0.1)

  # Restore rr_perm
  rr_perm[,1,] <- rr_perm[,1,] + 0.1
  rr_perm[,3,] <- rr_perm[,3,] - 0.1

  # And change rr_base
  rr_base[,1,] <- rr_base[,1,] - 0.1
  rr_base[,3,] <- rr_base[,3,] + 0.1

  paf_bootstrap <- get_paf_from_rr_boostrap(rr_base, rr_perm, age_weights, pop, n_boot = 1000, ci_level = 0.95, seed = 123)
  paf_delta <- get_paf_from_rr_delta(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)

  testthat::expect_true(paf_bootstrap["low"] < paf_bootstrap["central"] & paf_bootstrap["central"] < paf_bootstrap["high"])
  testthat::expect_true(paf_delta["low"] < paf_delta["central"] & paf_delta["central"] < paf_delta["high"])

  # Check that paf_delta is less confident than paf_bootstrap
  testthat::expect_true(paf_delta["high"] - paf_delta["low"] > paf_bootstrap["high"] - paf_bootstrap["low"])

  # Check that it isn't too far off though
  testthat::expect_true(abs((paf_delta["low"] - paf_bootstrap["low"])/paf_bootstrap["low"]) < 0.1)
  testthat::expect_true(abs((paf_delta["high"] - paf_bootstrap["high"])/paf_bootstrap["high"]) < 0.1)
})
