library(creahia)
library(tidyverse)
library(testthat)

local_test_data <- function(env = parent.frame()) {

  ages <- c("25-29")
  cause <- "NCD.LRI"
  iso3 <- "CHN"
  z <- 40 #µg/m3

  # Get data
  gemm <- get_gemm() %>% filter(region=="inc_China",
                                age %in% ages,
                                cause==!!cause)

  gbd_rr <- get_gbd_rr(version='original')
  gbd_rr <- get_gbd_rr(version='original')

  gemm_params <<- gemm %>%
    spread(param, value)

  theta <- gemm_params$t
  alpha <- gemm_params$a
  mu <- gemm_params$u
  tau_r <- gemm_params$p

  # Manually compute it
  # See https://www.mdpi.com/2073-4433/11/6/589 for formula
  hr_validated <- exp(theta * log((z-2.4)/alpha + 1) / (1+exp(-((z-2.4)-mu)/tau_r)))

  return(list(ages=ages, cause=cause, iso3=iso3, z=z, hr_validated=hr_validated, gemm=gemm, gbd_rr=gbd_rr))
}



test_that("GEMM hazards ratios are properly computed", {

  # Load data
  data <- local_test_data()
  list2env(data, envir = environment())

  # Hazard ratios should be 1 under the counterfactual, set at 2.4µg/m3
  hr_0 <- get_hazard_ratio(pm=0, .age=ages, .cause=cause, .region="inc_China", gemm=gemm, gbd_rr=gbd_rr)
  expect_true(all(hr_0==1))

  hr_cf <- get_hazard_ratio(pm=2.4, .age=ages, .cause=cause, .region="inc_China", gemm=gemm, gbd_rr=gbd_rr)
  expect_true(all(hr_cf==1))

  # Now compute hazard ratio and compare with manually computed one
  hr <- get_hazard_ratio(pm=z, .age=ages, .cause=cause, .region="inc_China", gemm=gemm, gbd_rr=gbd_rr)
  expect_equal(unname(hr[,"central"]), hr_validated)

})


test_that("GEMM PAF is properly computed", {

  # Load data
  data <- local_test_data()
  list2env(data, envir = environment())

  # Check that our PAF is properly computed
  conc <- data.frame(
    region_id="CHN",
    conc_scenario_pm25=0,
    conc_baseline_pm25=z,
    pop=1000
  )

  ihme <- get_ihme(version="gbd2019") %>%
    filter(iso3==!!iso3,
           measure_name=="Deaths",
           age %in% ages,
           cause==cause
    )

  paf <- country_paf_perm(pm.base = conc[, 'conc_baseline_pm25'],
                          pm.perm = conc[, 'conc_scenario_pm25'],
                          pop = conc[, 'pop'],
                          region_id = "CHN",
                          cause = cause,
                          measure = "Deaths",
                          adult_ages = ages,
                          epi_version = "gbd2019",
                          gemm = gemm,
                          gbd_rr = NULL,
                          ihme = ihme,
                          .region = "inc_China",
                          .mode = "change")

  # This is not exactly traditional PAF (opposite)
  paf_validated <- (1/hr_validated - 1)
  testthat::expect_equal(paf[["central"]], paf_validated)

})
