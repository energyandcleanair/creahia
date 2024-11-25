testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("HIAs are consistent across levels", {


  hias <- generate_random_exposure_hias(levels=c(0,1),
                                        administrative_res="low")

  # Comparison
  comparison <- hias %>%
    filter(estimate=="central") %>%
    group_by(level, Outcome, Cause, scenario, estimate) %>%
    dplyr::summarise_at("number", sum) %>%
    ungroup() %>%
    group_by(Outcome, Cause, scenario, estimate) %>%
    dplyr::summarise(deviation=(max(number) - min(number))/mean(number)) %>%
    ungroup() %>%
    arrange(desc(abs(deviation)))


  # For central, expect very close results
  max_deviation_central <- comparison %>%
    filter(estimate=="central") %>%
    summarise(max_deviation=max(abs(deviation), na.rm=T))
  testthat::expect_lt(max_deviation_central$max_deviation, 0.05)

})

