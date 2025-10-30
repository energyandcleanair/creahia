# Test our results vs GBD
testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("Deaths causes are similar to GBD2021", {

  skip_on_ci()
  hia_gbd2021 <- generate_donkelaar_exposure_hia(target=5,
                                                 iso3 = "ZAF",
                                                 epi_version = "gbd2021",
                                                 rr_sources=RR_GBD2021)

  hia_gbd2023 <- generate_donkelaar_exposure_hia(target=5,
                                                 iso3 = "ZAF",
                                                 epi_version = "gbd2021",
                                                 rr_sources=RR_GBD2023)

  # Manually collected GBD Dataa
  crea_deaths_gbd2021 <- hia_gbd2021 %>%
    filter(outcome=="Deaths",
           estimate=="central") %>%
    arrange(desc(abs(number))) %>%
    mutate(source="CREA (GBD2021)",
           number=-number) %>%
    select(number, cause, source)

  # Just for visual check
  crea_deaths_gbd2023 <- hia_gbd2023 %>%
    filter(outcome=="Deaths",
           estimate=="central") %>%
    arrange(desc(abs(number))) %>%
    mutate(source="CREA (GBD2023)",
           number=-number
    ) %>%
    select(number, cause, source)


  # Expected from PM air pollution
  # https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/dfef5c8ac1a77a6caec1c119fb5f3322
  expected <- tibble::tribble(
    ~cause, ~number, ~source,
    "Stroke",   6223,   "GBD2021",
    "IHD",      5973,   "GBD2021",
    "LRI",      5069,   "GBD2021",
    "Diabetes", 4727,   "GBD2021",
    "COPD",     2654,   "GBD2021",
    "LC",       1266,   "GBD2021"
  )

  bind_rows(crea_deaths_gbd2021, crea_deaths_gbd2023, expected) %>%
    ggplot() +
    geom_col(aes(cause, abs(number), fill=source), position='dodge')


  # Test that roughly equal
  comparison <- crea_deaths_gbd2021 %>%
    left_join(expected, by="cause") %>%
    mutate(diff = abs(number.x - number.y)/number.y) %>%
    select(cause, diff)

  testthat::expect_true(all(abs(comparison$diff) < 0.07))
})
