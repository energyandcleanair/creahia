testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("HIA returns expected cause outcomes", {

  hia_1 <- generate_uniform_exposure_hia(
    exposure_values = list(
      pm25 = list(baseline = 60, target = 0),
      no2 = list(baseline = 40, target = 10)
    ),
    rr_sources = creahia::RR_GBD2023,
    epi_version = "gbd2021"
  )

  cause_outcomes_available <- hia_1 %>%
    distinct(pollutant, cause, outcome, double_counted)

  # Expected cause-outcome pairs
  expected_cause_outcomes_rr <- creahia::get_rr(creahia::RR_GBD2023) %>%
    distinct(cause) %>%
    left_join(get_cause_measure() %>% rename(outcome=measure)) %>%
    mutate(
      outcome = case_when(grepl("child", cause) & outcome == "Deaths" ~ "Deaths.child",
                          TRUE ~ outcome)
      )

  expected_cause_outcomes_crf <- creahia::get_crfs() %>%
    distinct(cause, outcome)

  expected_cause_outcomes <- bind_rows(expected_cause_outcomes_rr, expected_cause_outcomes_crf) %>%
    distinct() %>%
    arrange(cause, outcome)

  # Check that all expected cause-outcome pairs are present
  comparison <- expected_cause_outcomes %>%
    left_join(cause_outcomes_available %>% mutate(available=T),
              by=c("cause", "outcome")) %>%
    mutate(available = ifelse(is.na(available), F, available))


  testthat::expect_true(all(comparison$available), info = {
    missing <- comparison %>% filter(!available)
    paste("Missing expected cause-outcome pairs:\n",
          paste0(missing$cause, " - ", missing$outcome, collapse = "\n"))
  })
})
