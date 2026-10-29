test_that("EPI metric keys exclude metadata columns", {
  epi <- tibble::tibble(
    location_id = 1,
    estimate = "central",
    pop = 1000,
    IHD_Deaths = 10,
    LRI_YLLs = 20
  )

  expect_setequal(
    get_epi_metric_keys(epi),
    c("IHD_Deaths", "LRI_YLLs")
  )
})


test_that("EPI compatibility checks require an estimate column", {
  valid_epi <- tibble::tibble(
    estimate = c("low", "central", "high"),
    IHD_Deaths = c(8, 10, 12)
  )

  expect_invisible(validate_epi_for_crf_check(valid_epi))
  expect_error(
    validate_epi_for_crf_check(dplyr::select(valid_epi, -estimate)),
    "missing required columns.*estimate"
  )
})


test_that("missing EPI metric estimates are reported explicitly", {
  epi <- tibble::tibble(
    estimate = c("low", "central", "high"),
    IHD_Deaths = c(8, 10, NA_real_),
    LRI_YLLs = c(18, 20, 22)
  )

  expect_equal(CRF_EPI_REQUIRED_ESTIMATES, c("low", "central", "high"))
  expect_equal(
    find_missing_epi_metric_estimates(
      epi = epi,
      metric_keys = c("IHD_Deaths", "LRI_YLLs")
    ),
    tibble::tibble(
      metric_key = "IHD_Deaths",
      missing_estimate = "high"
    )
  )

  epi_without_high <- dplyr::filter(epi, estimate != "high")
  expect_equal(
    find_missing_epi_metric_estimates(
      epi = epi_without_high,
      metric_keys = c("IHD_Deaths", "LRI_YLLs")
    ),
    tibble::tibble(
      metric_key = c("IHD_Deaths", "LRI_YLLs"),
      missing_estimate = c("high", "high")
    )
  )

  complete_epi <- dplyr::mutate(epi, IHD_Deaths = c(8, 10, 12))
  expect_equal(
    nrow(find_missing_epi_metric_estimates(
      epi = complete_epi,
      metric_keys = c("IHD_Deaths", "LRI_YLLs")
    )),
    0
  )
})
