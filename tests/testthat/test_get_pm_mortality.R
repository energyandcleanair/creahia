testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("get_pm_mortality computes confidence intervals correctly", {

  testthat::skip_on_ci()

  # Sample PAF data
  generate_test_data <- function(central_paf,
                                 se_paf,
                                 central_epi,
                                 se_epi,
                                 pop,
                                 region_id = "BGD",
                                 var = c("cause1_yll")) {

    z <- 1.96 # 95% confidence interval

    cause <- sub('_.*$', '', var)
    outcome <- sub('^[^_]*_', '', var)

    central_paf_vals <- rep_len(central_paf, length(var))
    se_paf_vals <- rep_len(se_paf, length(var))

    paf_scenario <- tibble::tibble(
      pollutant = rep("PM25", length(var)),
      cause = cause,
      outcome = outcome,
      region_id = rep(region_id, length(var)),
      low = central_paf_vals - z * se_paf_vals,
      central = central_paf_vals,
      high = central_paf_vals + z * se_paf_vals
    )

    central_epi_vals <- rep_len(central_epi, length(var))
    se_epi_vals <- rep_len(se_epi, length(var))

    epi_loc <- tibble::tibble(
      iso3 = region_id,
      region_id = region_id,
      estimate = c("low", "central", "high"),
      pop = rep(pop, 3)
    )

    for(idx in seq_along(var)) {
      epi_values <- c(
        central_epi_vals[idx] - z * se_epi_vals[idx],
        central_epi_vals[idx],
        central_epi_vals[idx] + z * se_epi_vals[idx]
      )
      epi_loc[[var[idx]]] <- epi_values
    }

    return(list(paf_scenario = paf_scenario, epi_loc = epi_loc))
  }

  test_config <- function(central_paf, se_paf, central_epi, se_epi, pop, var) {
    data <- generate_test_data(central_paf, se_paf, central_epi, se_epi, pop, var=var)
    expected_se <- sqrt(central_epi^2*se_paf^2 + central_paf^2*se_epi^2) * pop / 1e5

    pm_mortality <- creahia::get_pm_mortality(
      paf_scenario = data$paf_scenario,
      epi_loc = data$epi_loc
    )

    expected_cols <- c("region_id", "pop", "estimate", paste0(var, "_PM25"))
    testthat::expect_true(all(expected_cols %in% colnames(pm_mortality)))

    high_val <- pm_mortality %>%
      dplyr::filter(estimate == "high") %>%
      dplyr::pull(paste0(var, "_PM25"))
    low_val <- pm_mortality %>%
      dplyr::filter(estimate == "low") %>%
      dplyr::pull(paste0(var, "_PM25"))
    actual_se <- (high_val - low_val) / (2 * 1.96)

    testthat::expect_equal(actual_se, expected_se, tolerance = 1e-4)
  }


  # Config 1
  var <- c("cause1_yll")
  pop <- 1000
  central_paf <- 0.1
  se_paf <- 0.01
  central_epi <- 500
  se_epi <- 0
  test_config(central_paf, se_paf, central_epi, se_epi, pop, var=var)

  # Config2
  se_paf <- 0
  se_epi <- 10
  test_config(central_paf, se_paf, central_epi, se_epi, pop, var=var)

  # Config3
  se_paf <- 0.01
  se_epi <- 10
  # TODO Restore
  # test_config(central_paf, se_paf, central_epi, se_epi, pop, var=var)

})
