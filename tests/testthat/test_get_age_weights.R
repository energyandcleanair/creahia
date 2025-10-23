# Test get_age_weights function

test_that("get_age_weights returns correct structure with real data", {

  region_id <- "BGD"  # Bangladesh
  cause <- "NCD.LRI"
  measure <- "Deaths"
  rr_source <- "gbd2019"
  version <- "gbd2019"

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
  version <- "gbd2019"

  # Test with different RR sources
  rr_sources <- c("gbd2019", "gbd2021", "original")

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
  cause <- "NCD.LRI"
  rr_source <- "gbd2019"
  version <- "gbd2019"

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
  cause <- "NCD.LRI"
  measure <- "Deaths"
  rr_source <- "gbd2019"
  version <- "gbd2019"

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
  cause <- "NCD.LRI"
  measure <- "Deaths"
  rr_source <- "gbd2019"
  version <- "gbd2019"

  # Clear cache first
  clear_ihme_cache()

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
  cause <- "NCD.LRI"
  measure <- "Deaths"
  rr_source <- "gbd2019"
  version <- "gbd2019"

  # Test with a region that doesn't exist in IHME data
  result <- get_age_weights("NONEXISTENT", cause, measure, rr_source, version)
  expect_null(result)
})
