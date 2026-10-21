test_that("explicit crfs_set stops once and lists every failing region", {
  crfs <- crfs_set(presets = "gemm_adult_deaths_components")
  crfs <- crfs[crfs$cause == "IHD", ]

  region_conc <- function(baseline, scenario) {
    data.frame(
      conc_baseline_pm25 = baseline,
      conc_scenario_pm25 = scenario,
      pop = c(1000, 2000)
    )
  }
  # IDN is within the GEMM exposure range; THA and VNM exceed it.
  conc_map <- list(scen = list(
    IDN = region_conc(c(20, 30), c(25, 35)),
    THA = region_conc(c(20, 3000), c(25, 3005)),
    VNM = region_conc(c(20, 4000), c(25, 4005))
  ))

  error <- expect_error(
    compute_hia_paf(
      conc_map = conc_map,
      species = "pm25",
      regions = NULL,
      scenarios = "scen",
      epi_version = "gbd2021",
      rr_sources = c(),
      crfs = crfs,
      diagnostic_folder = NULL
    ),
    "Registry CRF calculation failed for 2 region\\(s\\): THA, VNM"
  )

  message <- conditionMessage(error)
  expect_match(message, "No results were returned")
  expect_match(message, "region THA, gemm_pm25_ihd_adult_deaths_v1 \\(PM25/IHD/Deaths\\)")
  expect_match(message, "region VNM, gemm_pm25_ihd_adult_deaths_v1 \\(PM25/IHD/Deaths\\)")
  expect_match(message, "Exposure out of CRF range")
  expect_false(grepl("region IDN", message, fixed = TRUE))
})

test_that("registry region error summary truncates long lists", {
  crf <- tibble::tibble(
    crf_id = "x_v1", pollutant = "PM25", cause = "IHD", outcome = "Deaths"
  )
  failures <- lapply(seq_len(3), function(i) {
    list(scenario = "scen", region_id = paste0("R", i), crf = crf, message = "boom")
  })

  message <- .format_registry_region_errors(failures, max_listed = 2L)

  expect_match(message, "failed for 3 region\\(s\\): R1, R2, R3")
  expect_match(message, "region R2")
  expect_false(grepl("region R3,", message, fixed = TRUE))
  expect_match(message, "- \\.\\.\\. and 1 more")
})
