# Test get_age_weights function

test_that("get_age_weights returns correct structure with real data", {

  region_id <- "BGD"  # Bangladesh
  cause <- "LRI"
  measure <- "Deaths"
  rr_source <- "gbd2023"
  version <- "gbd2023"

  # Test with real data
  result <- get_age_weights(region_id, cause, measure, rr_source, version)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("ages", "age_weights"))
  expect_type(result$ages, "character")
  expect_s3_class(result$age_weights, "data.frame")

  # Check that we have some ages
  expect_gt(length(result$ages), 0)
  expect_gt(nrow(result$age_weights), 0)

  # Check age_weights structure
  expect_true("age" %in% names(result$age_weights))
  expect_true("val" %in% names(result$age_weights))
  expect_equal(result$age_weights$age, result$ages)
})

test_that("get_age_weights works with different RR sources", {

  region_id <- "BGD"
  measure <- "Deaths"
  version <- "gbd2023"

  # Test with different RR sources
  rr_sources <- c("original", "gbd2019", "gbd2021", "gbd2023")

  for (rr_source in rr_sources) {

    causes <- get_rr(rr_source)$cause %>% unique()
    for (cause in causes) {
      print(paste("Testing cause:", cause, "with RR source:", rr_source))
      result <- get_age_weights(region_id, cause, measure, rr_source, version)

      if (!is.null(result)) {
        expect_type(result, "list")
        expect_named(result, c("ages", "age_weights"))
        expect_gt(length(result$ages), 0)
        expect_gt(nrow(result$age_weights), 0)
      }
    }
  }
})

test_that("get_age_weights works with different measures", {

  region_id <- "BGD"
  cause <- "LRI"
  rr_source <- "gbd2023"
  version <- "gbd2023"

  measures <- c("Deaths", "YLLs", "YLDs")

  for (measure in measures) {
    result <- get_age_weights(region_id, cause, measure, rr_source, version)

    if (!is.null(result)) {
      expect_type(result, "list")
      expect_named(result, c("ages", "age_weights"))
      expect_equal(result$age_weights$measure_name[1], measure)
    }
  }
})

test_that("get_age_weights handles invalid inputs gracefully", {

  region_id <- "BGD"
  cause <- "LRI"
  measure <- "Deaths"
  rr_source <- "gbd2023"
  version <- "gbd2023"

  # Test with invalid region_id
  result <- get_age_weights("INVALID_REGION", cause, measure, rr_source, version)
  expect_null(result)

  # Test with invalid cause
  result <- get_age_weights(region_id, "INVALID_CAUSE", measure, rr_source, version)
  expect_null(result)

  # Test with invalid measure
  result <- get_age_weights(region_id, cause, "INVALID_MEASURE", rr_source, version)
  expect_null(result)
})

test_that("get_age_weights caches results correctly", {

  region_id <- "BGD"
  cause <- "LRI"
  measure <- "Deaths"
  rr_source <- "gbd2023"
  version <- "gbd2023"

  # Clear cache first
  clear_epi_count_long_cache()

  # First call should load IHME data
  result1 <- get_age_weights(region_id, cause, measure, rr_source, version)
  expect_type(result1, "list")

  # Second call should use cached IHME data (should be faster)
  result2 <- get_age_weights(region_id, cause, measure, rr_source, version)
  expect_type(result2, "list")

  # Results should be identical
  expect_equal(result1, result2)
})

test_that("get_age_weights returns NULL for missing data", {

  region_id <- "BGD"
  cause <- "LRI"
  measure <- "Deaths"
  rr_source <- "gbd2023"
  version <- "gbd2023"

  # Test with a region that doesn't exist in IHME data
  result <- get_age_weights("NONEXISTENT", cause, measure, rr_source, version)
  expect_null(result)
})

test_that("child LRI finds its under-5 age weight in every EPI version", {

  for (version in c("C40", "gbd2017", "gbd2019", "gbd2021", "gbd2023")) {
    for (measure in c("Deaths", "YLLs")) {
      expect_no_warning(
        result <- get_age_weights("BGD", CAUSE_LRICHILD, measure, RR_FUSION, version)
      )
      expect_equal(result$ages, AGE_CHILDREN, info = paste(version, measure))
      expect_gt(result$age_weights$val, 0)
    }
  }
})

test_that("GBD 2017 counts use the same labels as later GBD versions", {

  raw <- readr::read_csv(
    get_hia_path("epi/processed/epi_count_long_gbd2017.csv"),
    col_types = readr::cols()
  )

  expect_true("cause" %in% names(raw))
  expect_false("<5" %in% raw$age)
  expect_false(any(raw$cause == CAUSE_LRI & raw$age == AGE_CHILDREN))

  child <- raw %>%
    filter(location_id == get_epi_location_id("BGD"), cause == CAUSE_LRICHILD,
           age == AGE_CHILDREN, measure_name == "Deaths", estimate == "central")
  result <- get_age_weights("BGD", CAUSE_LRICHILD, "Deaths", RR_FUSION, "C40")

  expect_equal(result$age_weights$val, sum(child$val))
  expect_false(AGE_CHILDREN %in% get_age_weights("BGD", CAUSE_LRI, "Deaths", RR_FUSION, "C40")$ages)
})
