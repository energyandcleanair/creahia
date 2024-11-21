test_that("get_pm_mortality computes attributable burden correctly", {

  # Sample PAF data
  generate_test_data <- function(central_paf, se_paf, central_epi, se_epi, pop, region_id = "BGD", var=c("cause1_yll")) {

    z <- 1.96 # 95% confidence interval

    # Create paf_scenario dataframe
    paf_scenario <- tibble::tibble(
      var = vars,
      low = central_paf - z * se_paf,
      central = central_paf,
      high = central_paf + z * se_paf,
      region_id = region_id
    )

    # Create epi_loc dataframe
    epi_loc <- tibble::tibble(
      iso3 = region_id,
      estimate = rep(c("low", "central", "high"), each = length(vars)),
      pop = rep(pop, 3 * length(vars)),
      value = c(
          central_epi - z * se_epi,
          central_epi,
          central_epi + z * se_epi
        )
    ) %>%
      rename_at("value", ~var)

    return(list(paf_scenario = paf_scenario, epi_loc = epi_loc))
  }

  test_config <- function(central_paf, se_paf, central_epi, se_epi, pop, var) {
    data <- generate_test_data(central_paf, se_paf, central_epi, se_epi, pop, var=var)
    expected_se <- sqrt(central_epi^2*se_paf^2 + central_paf^2*se_epi^2) * pop / 1e5

    pm_mortality <- creahia::get_pm_mortality(
      paf_scenario = data$paf_scenario,
      epi_loc = data$epi_loc,
      calc_causes = var
    )

    testthat::expect_true(all(c("region_id", "pop", paste0(var, "_PM25")) %in% colnames(pm_mortality)))

    actual_se <- (pm_mortality[which(pm_mortality$estimate == "high"),paste0(var, "_PM25")] - pm_mortality[which(pm_mortality$estimate == "low"),paste0(var, "_PM25")]) / (2 * 1.96)
    testthat::expect_equal(actual_se, expected_se, tolerance = 1e-4)
  }


  # Config 1
  var <- c("cause1_yll")
  pop <- 1000
  central_paf <- 0.1
  se_paf <- 0.01
  central_epi <- 500
  se_epi <- 0
  test_config(central_paf, se_paf, central_epi, se_epi, pop, var)

  # Config2
  se_paf <- 0
  se_epi <- 10
  test_config(central_paf, se_paf, central_epi, se_epi, pop, var)

  # Config3
  se_paf <- 0.01
  se_epi <- 10
  test_config(central_paf, se_paf, central_epi, se_epi, pop, var)

})
