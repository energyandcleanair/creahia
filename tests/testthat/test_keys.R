testthat::test_that("build_metric_key and split_metric_key round-trip", {
  causes <- c("IHD", "NCD.LRI")
  outcomes <- c("Deaths", "YLLs")

  keys <- build_metric_key(causes, outcomes)
  parts <- split_metric_key(keys)

  testthat::expect_equal(parts$cause, causes)
  testthat::expect_equal(parts$outcome, outcomes)
})

testthat::test_that("build_metric_key validates separator presence", {
  # cause contains underscore
  testthat::expect_error(build_metric_key("Bad_Cause", "Deaths"))
  # outcome contains underscore
  testthat::expect_error(build_metric_key("IHD", "Bad_Outcome"))
  # NA inputs
  testthat::expect_error(build_metric_key(NA_character_, "Deaths"))
  testthat::expect_error(build_metric_key("IHD", NA_character_))
})

testthat::test_that("to_long_hia uses stable key splitting", {
  # Minimal wide-like input after multiply_paf_epi
  df <- tibble::tibble(
    scenario = factor("scenario1"),
    region_id = "BGD",
    pollutant = "PM25",
    pop = 1000,
    estimate = "central",
    `IHD_Deaths` = 10,
    `NCD.LRI_YLLs` = 20
  )

  long <- to_long_hia(df)

  # Expect two rows for the two metrics
  testthat::expect_equal(nrow(long), 2L)
  # The numbers should be preserved
  vals <- setNames(long$number, build_metric_key(long$cause, long$outcome))
  testthat::expect_equal(unname(vals["IHD_Deaths"]), 10)
  testthat::expect_equal(unname(vals["NCD.LRI_YLLs"]), 20)
})

