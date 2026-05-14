test_that("migrated GEMM IHD 25+ tabular CRF matches legacy rr_gemm", {
  legacy <- readr::read_csv(
    get_hia_path("rr/processed/rr_gemm.csv", error_if_not_exists = TRUE),
    col_types = readr::cols()
  ) %>%
    dplyr::filter(cause == "IHD", age == "25+", source == "gemm") %>%
    dplyr::arrange(exposure) %>%
    dplyr::select(exposure, age, low, central, high) %>% 
    as_tibble()

  migrated <- readr::read_csv(
    get_hia_path(
      "crf/tabular/gemm_pm25_ihd_25plus_deaths_v1.csv",
      error_if_not_exists = TRUE
    ),
    col_types = readr::cols()
  ) %>% 
  as_tibble()

  expect_equal(migrated, legacy)
})
