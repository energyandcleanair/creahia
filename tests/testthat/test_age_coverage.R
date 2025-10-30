## Tests for check_age_completeness (allows overlap) ##

test_that("check_age_completeness validates complete split age groups", {
  # Should pass: all AGE_ADULTS_SPLIT age groups present
  expect_true(
    check_age_completeness(
      c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    )
  )
})

test_that("check_age_completeness allows both aggregate and split ages", {
  # Should pass: both 25+ and split ages can coexist (IHME data pattern)
  expect_true(
    check_age_completeness(
      c("25+", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    )
  )
})

test_that("check_age_completeness detects incomplete split age groups", {
  # Should fail: missing some AGE_ADULTS_SPLIT age groups
  expect_error(
    check_age_completeness(c("25-29", "30-34", "35-39")),
    "Incomplete age coverage"
  )
})

test_that("check_age_completeness allows no split ages", {
  # Should pass: no split ages present, so no completeness check needed
  expect_true(
    check_age_completeness(c("Under 5", "5-9", "10-14", "25+"))
  )
})


## Tests for check_age_coverage_and_uniqueness (no overlap allowed) ##

test_that("check_age_coverage_and_uniqueness validates complete age groups", {
  # Should pass: all AGE_ADULTS_SPLIT age groups present
  expect_true(
    check_age_coverage_and_uniqueness(
      c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    )
  )
})

test_that("check_age_coverage_and_uniqueness detects incomplete age groups", {
  # Should fail: missing some AGE_ADULTS_SPLIT age groups
  expect_error(
    check_age_coverage_and_uniqueness(c("25-29", "30-34", "35-39")),
    "Incomplete age coverage"
  )
})

test_that("check_age_coverage_and_uniqueness detects overlap", {
  # Should fail: both aggregate and split ages present
  expect_error(
    check_age_coverage_and_uniqueness(
      c("25+", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    ),
    "Age group overlap"
  )
})

test_that("check_age_coverage_and_uniqueness allows aggregate without split", {
  # Should pass: only aggregate age group, no split ages
  expect_true(
    check_age_coverage_and_uniqueness(c("25+", "Under 5"))
  )
})

test_that("check_age_coverage_and_uniqueness allows other ages with complete split", {
  # Should pass: complete split ages plus other ages (like children)
  expect_true(
    check_age_coverage_and_uniqueness(
      c("Under 5", "5-9", "10-14", "15-19", "20-24",
        "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    )
  )
})

test_that("check_age_coverage_and_uniqueness handles NA values", {
  # Should pass: NA values are filtered out, remaining ages are complete
  expect_true(
    check_age_coverage_and_uniqueness(
      c(NA, "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
        "55-59", "60-64", "65-69", "70-74", "75-79", "80+", NA)
    )
  )
})

test_that("check_age_coverage_and_uniqueness allows no split ages at all", {
  # Should pass: no AGE_ADULTS_SPLIT ages present, so no completeness check needed
  expect_true(
    check_age_coverage_and_uniqueness(c("Under 5", "5-9", "10-14", "25+"))
  )
})


## Integration test with RR data ##

test_that("RR ages pass uniqueness check after deduplication", {
  skip_if_not(file.exists("../../inst/extdata/rr/processed/rr_gbd2019.csv"), "RR data not available")
  
  rr <- get_rr('gbd2019')
  
  # Test a cause with split ages
  ages_ihd <- unique(rr[rr$cause == "IHD", ]$age) %>%
    deduplicate_adult_ages()
  
  # Should pass: deduplicated ages should have no overlap
  expect_true(
    check_age_coverage_and_uniqueness(ages_ihd, "RR gbd2019 for IHD")
  )
  
  # Test a cause with single age (child)
  ages_lri_child <- unique(rr[rr$cause == "LRI.child", ]$age) %>%
    deduplicate_adult_ages()
  
  # Should pass: single age group
  expect_true(
    check_age_coverage_and_uniqueness(ages_lri_child, "RR gbd2019 for LRI.child")
  )
})
