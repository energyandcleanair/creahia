legacy_default_fingerprint_conc_map <- function() {
  list(
    legacy_default_baseline = list(
      fingerprint_region = data.frame(
        conc_baseline_pm25 = c(35, 55),
        conc_scenario_pm25 = c(10, 25),
        conc_baseline_no2 = c(30, 50),
        conc_scenario_no2 = c(10, 20),
        conc_baseline_o3_8h = c(70, 90),
        conc_scenario_o3_8h = c(30, 50),
        conc_baseline_so2 = c(20, 30),
        conc_scenario_so2 = c(5, 10),
        pop = c(1000, 3000)
      )
    )
  )
}

read_legacy_default_paf_fingerprint <- function() {
  readr::read_csv(
    testthat::test_path("fixtures", "legacy_default_crf_paf_fingerprint.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::arrange(.data$pollutant, .data$cause, .data$outcome)
}

resolve_historical_default_crfs <- function() {
  memberships <- .load_historical_crf_memberships() %>%
    dplyr::filter(selection_id == "legacy_profile_default")

  .resolve_historical_crf_components(memberships$component_id)
}

legacy_default_hia_epi_fixture <- function() {
  tibble::tibble(
    location_id = 1,
    estimate = c("low", "central", "high"),
    Asthma.1to18_AsthmaIncidence = c(180, 200, 220),
    Asthma.1to18_AsthmaPrevalence = c(270, 300, 330),
    Asthma.0to17_AsthmaERV = c(360, 400, 440),
    Asthma.18to99_AsthmaERV = c(450, 500, 550),
    PTB_PTB = c(540, 600, 660),
    LBW_LBW = c(630, 700, 770),
    Absences_Absences = c(720, 800, 880),
    COPD_Deaths = c(810, 900, 990),
    COPD_YLLs = c(1800, 2000, 2200),
    NCD.LRI_Deaths = c(900, 1000, 1100),
    NCD.LRI_YLLs = c(2700, 3000, 3300),
    pop = c(100000, 100000, 100000),
    country = "Fingerprint country",
    iso3 = "FPR"
  )
}
