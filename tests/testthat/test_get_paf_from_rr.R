test_that("paf bootstrap computation from rr is correct", {

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
  paf_delta <- creahia::get_paf_from_rr_delta(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)
  paf_correlated <- get_paf_from_rr_correlated(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)

  # Comparison
  testthat::expect_equal(paf_bootstrap, expected_paf_triplet)
  testthat::expect_equal(paf_delta, expected_paf_triplet)
  testthat::expect_equal(paf_correlated, expected_paf_triplet)


  testthat::expect_equal(names(paf_bootstrap), c("low", "central", "high"))
  testthat::expect_equal(names(paf_delta), c("low", "central", "high"))
  testthat::expect_equal(names(paf_correlated), c("low", "central", "high"))

  testthat::expect_equal(length(paf_bootstrap), 3)
  testthat::expect_equal(length(paf_delta), 3)
  testthat::expect_equal(length(paf_correlated), 3)

  # Now introduce some uncertainty
  rr_perm[,1,] <- rr_perm[,1,] - 0.1
  rr_perm[,3,] <- rr_perm[,3,] + 0.1

  paf_bootstrap <- creahia::get_paf_from_rr_boostrap(rr_base, rr_perm, age_weights, pop, n_boot = 1000, ci_level = 0.95, seed = 123)
  paf_delta <- creahia::get_paf_from_rr_delta(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)
  paf_correlated <- creahia::get_paf_from_rr_correlated(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)


  testthat::expect_true(paf_bootstrap["low"] < paf_bootstrap["central"] & paf_bootstrap["central"] < paf_bootstrap["high"])
  testthat::expect_true(paf_delta["low"] < paf_delta["central"] & paf_delta["central"] < paf_delta["high"])
  # testthat::expect_true(paf_correlated["low"] < paf_correlated["central"] & paf_correlated["central"] < paf_correlated["high"])

  # Check that paf_delta is less confident than paf_bootstrap
  testthat::expect_true(paf_delta["high"] - paf_delta["low"] > paf_bootstrap["high"] - paf_bootstrap["low"])

  # Check that paf_correlated is more confident than all
  testthat::expect_true(abs(paf_correlated["high"] - paf_correlated["low"]) < abs(paf_delta["high"] - paf_delta["low"]))

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

  # Check that paf_correlated is more confident than all
  testthat::expect_true(abs(paf_correlated["high"] - paf_correlated["low"]) < abs(paf_delta["high"] - paf_delta["low"]))

  # Check that it isn't too far off though
  testthat::expect_true(abs((paf_delta["low"] - paf_bootstrap["low"])/paf_bootstrap["low"]) < 0.1)
  testthat::expect_true(abs((paf_delta["high"] - paf_bootstrap["high"])/paf_bootstrap["high"]) < 0.1)

  # Have both uncertainties
  rr_perm[,1,] <- rr_perm[,1,] - 0.05
  rr_perm[,3,] <- rr_perm[,3,] + 0.05

  paf_bootstrap <- get_paf_from_rr_boostrap(rr_base, rr_perm, age_weights, pop, n_boot = 1000, ci_level = 0.95, seed = 123)
  paf_delta <- get_paf_from_rr_delta(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)
  paf_correlated <- get_paf_from_rr_correlated(rr_base, rr_perm, age_weights, pop, ci_level = 0.95)

  testthat::expect_true(paf_bootstrap["low"] < paf_bootstrap["central"] & paf_bootstrap["central"] < paf_bootstrap["high"])
  testthat::expect_true(paf_delta["low"] < paf_delta["central"] & paf_delta["central"] < paf_delta["high"])
  testthat::expect_true(paf_correlated["low"] < paf_correlated["central"] & paf_correlated["central"] < paf_correlated["high"])

  # Check that paf_delta is less confident than paf_bootstrap
  testthat::expect_true(paf_delta["high"] - paf_delta["low"] > paf_bootstrap["high"] - paf_bootstrap["low"])

  # Check that paf_correlated is more confident than all
  testthat::expect_true(abs(paf_correlated["high"] - paf_correlated["low"]) < abs(paf_delta["high"] - paf_delta["low"]))

  # Check that it isn't too far off though
  testthat::expect_true(abs((paf_delta["low"] - paf_bootstrap["low"])/paf_bootstrap["low"]) < 0.1)
  testthat::expect_true(abs((paf_delta["high"] - paf_bootstrap["high"])/paf_bootstrap["high"]) < 0.1)



})
