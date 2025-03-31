testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("Confidence interval makes sense when comparing two scenarios", {


  hia_1 <- generate_uniform_exposure_hia(60, 0)
  hia_2 <- generate_uniform_exposure_hia(60, 5)


  # Because p1 is a larger (negative) perturbation
  # We expect number_1 to be larger (negative) numbers
  join_by <- setdiff(names(hia_1), c("number"))

  error <- hia_1 %>%
    left_join(hia_2, by = join_by, suffix = c("_1", "_2")) %>%
    filter(number_1 > number_2)

  testthat::expect_true(nrow(error) == 0)

})


test_that("Order of estimates is consistent", {


  hia <- generate_uniform_exposure_hia(60, 0)

  # Expect the order low <= central <= high to be consistent
  inconsistent_order <- hia %>%
    spread(estimate, number) %>%
    mutate(order=case_when(
      low <= central & central <= high ~ "increasing",
      low >= central & central >= high ~ "decreasing",
      T ~ "inconsistent"
    )) %>%
    summarise(ok = n_distinct(order) == 1 & !("inconsistent" %in% order))

  testthat::expect_true(inconsistent_order$ok)

})


test_that("Estimates are of the same sign with uniform exposure", {

  # This can happen when small changes at low level?
  hia <- generate_uniform_exposure_hia(6, 5)

  hia %>%
    summarise(ok = all(number >= 0) | all(number <= 0)) %>%
    pull(ok) %>%
    testthat::expect_true()

})
