# skip_on_ci("Only need to be run when creating new epi.")

testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())

# Test configuration
VERSIONS <- c('gbd2019', 'gbd2021', 'gbd2023')
KEY_COUNTRIES <- c('USA', 'CHN', 'IND', 'PHL', 'IDN', 'GBR', 'DEU', 'JPN')
ALLOWED_MISSING_ISO3S <- c("COK", "MCO", "NRU", "NIU", "PLW", "KNA", "SMR", "TKL", "TUV")
ALLOWED_LOST_KEYS <- c("birth.rate", "labor.partic")
MUST_HAVE_CAUSES <- c(CAUSE_IHD, CAUSE_STROKE, CAUSE_COPD, CAUSE_LUNGCANCER, CAUSE_DIABETES,
                      CAUSE_LRI, CAUSE_DEMENTIA, CAUSE_NCDLRI)

# Allowed missing cause/age/measure combinations by version
# Format: cause, age, measure_name, version
ALLOWED_MISSING_COMBOS_EPI_LONG <- tidyr::crossing(
  cause = "COPD",
  age = c("10-14", "5-9", "Under 5"),
  measure_name = c("Deaths", "YLLs"),
  version = c("gbd2021","gbd2023"),
  description = "COPD YLLs and Deaths for children/teenages are missing in newer gbd versions (because 0 most likely)"
)


# Helper: Load all versions of epi_rate_wide
load_epi_wide_versions <- function(versions = VERSIONS) {
  result <- lapply(versions, function(v) get_epi(v))
  names(result) <- versions
  return(result)
}

# Helper: Load all versions of epi_count_long
load_epi_long_versions <- function(versions = VERSIONS) {
  result <- lapply(versions, function(v) {
    # get_epi_count_long may not support gbd2023 yet in the recode mapping
    # If it fails, try reading the file directly
    tryCatch({
      get_epi_count_long(v)
    }, error = function(e) {
      # For gbd2023, try reading directly if file exists
      if (v == "gbd2023") {
        file_path <- get_hia_path(glue::glue("epi/processed/epi_count_long_{v}.csv"))
        if (file.exists(file_path)) {
          read_csv(file_path, col_types = cols())
        } else {
          stop(paste("gbd2023 file not found. Error:", e$message))
        }
      } else {
        stop(e)
      }
    })
  })
  names(result) <- versions
  return(result)
}

# Helper: Extract metric keys (column names) from epi_rate_wide
get_metric_keys <- function(epi_wide) {
  metadata_cols <- c("location_id", "location_level", "iso3", "estimate",
                     "country", "region", "income_group", "location_name", "pop")
  all_cols <- names(epi_wide)
  metric_keys <- setdiff(all_cols, metadata_cols)
  return(metric_keys)
}

# Helper: Extract cause/age/measure combinations from epi_count_long
get_cause_age_combos <- function(epi_long) {
  epi_long %>%
    distinct(cause, age, measure_name) %>%
    arrange(cause, age, measure_name)
}

# Helper: Compare values across versions with tolerance
compare_values_pairwise <- function(data_list, key_cols, value_col,
                                     tolerance = 0.1,
                                     filter_expr = NULL) {
  versions <- names(data_list)

  # Combine all versions
  combined <- bind_rows(
    lapply(versions, function(v) {
      df <- data_list[[v]] %>% mutate(version = v)
      if (!is.null(filter_expr)) {
        df <- df %>% filter(!!filter_expr)
      }
      return(df)
    })
  )

  # Pivot to wide format for comparison
  comparison <- combined %>%
    select(all_of(c(key_cols, value_col, "version"))) %>%
    tidyr::pivot_wider(names_from = version, values_from = all_of(value_col))

  # Get version columns
  version_cols <- intersect(versions, names(comparison))

  if (length(version_cols) < 2) {
    return(list(success = TRUE, message = "Less than 2 versions to compare"))
  }

  # Compare all pairs
  version_pairs <- utils::combn(version_cols, 2, simplify = FALSE)
  issues <- list()

  for (pair in version_pairs) {
    v1 <- pair[1]
    v2 <- pair[2]

    # Calculate relative differences
    comparison <- comparison %>%
      mutate(
        rel_diff = abs((!!sym(v2) / !!sym(v1)) - 1),
        both_present = !is.na(!!sym(v1)) & !is.na(!!sym(v2))
      )

    # Check tolerance
    exceeds_tolerance <- comparison %>%
      filter(both_present) %>%
      filter(rel_diff > tolerance)

    if (nrow(exceeds_tolerance) > 0) {
      issues[[paste0(v1, "_vs_", v2)]] <- exceeds_tolerance %>%
        slice_max(rel_diff, n = 5) %>%
        select(all_of(c(key_cols, v1, v2, "rel_diff")))
    }
  }

  return(list(success = length(issues) == 0, issues = issues, comparison = comparison))
}


plot_epi_wide_comparison <- function(epi_list, iso3="CHN"){
  bind_rows(epi_list, .id="version") %>%
    filter(location_level==3, iso3==!!iso3, estimate=="central") %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, version,
                                  country, region, income_group, location_name, pop),
                        names_to = "metric_key", values_to = "val") %>%
    ggplot(aes(x=version, y=val, fill=version)) +
    geom_col() +
    facet_wrap(~metric_key, scales="free_y")
}

# ============================================================================
# EPI RATE WIDE TESTS
# ============================================================================

test_that("EPI Rate Wide - metric keys grow over time (no keys lost)", {
  epi_list <- load_epi_wide_versions()

  # Visual inspection
  if(F){
    plot_epi_wide_comparison(epi_list, iso3="IND")
  }

  # Get metric keys for each version
  metric_keys_list <- lapply(epi_list, get_metric_keys)

  # Check that newer versions contain all keys from older versions
  # VERSIONS is ordered chronologically, so each version should have all keys from previous ones
  lost_keys <- list()
  allowed_lost_keys <- list()
  for (i in 2:length(VERSIONS)) {
    older_version <- VERSIONS[i - 1]
    newer_version <- VERSIONS[i]

    older_keys <- metric_keys_list[[older_version]]
    newer_keys <- metric_keys_list[[newer_version]]

    # Keys that exist in older version but not in newer (lost keys)
    lost <- setdiff(older_keys, newer_keys)
    if (length(lost) > 0) {
      # Separate allowed lost keys from unexpected lost keys
      allowed_lost <- intersect(lost, ALLOWED_LOST_KEYS)
      unexpected_lost <- setdiff(lost, ALLOWED_LOST_KEYS)

      if (length(unexpected_lost) > 0) {
        lost_keys[[paste0(older_version, "_to_", newer_version)]] <- data.frame(
          older_version = older_version,
          newer_version = newer_version,
          lost_keys = unexpected_lost
        )
      }

      if (length(allowed_lost) > 0) {
        allowed_lost_keys[[paste0(older_version, "_to_", newer_version)]] <- data.frame(
          older_version = older_version,
          newer_version = newer_version,
          allowed_lost_keys = allowed_lost
        )
      }
    }
  }

  # No unexpected keys should be lost
  testthat::expect_true(
    length(lost_keys) == 0,
    info = {
      paste("Unexpected keys were lost between versions:\n",
            paste(capture.output(print(lost_keys_df)), collapse = "\n"))
    }
  )

  # Also report new keys added (informational, not a failure)
  new_keys <- list()
  for (i in 2:length(VERSIONS)) {
    older_version <- VERSIONS[i - 1]
    newer_version <- VERSIONS[i]

    older_keys <- metric_keys_list[[older_version]]
    newer_keys <- metric_keys_list[[newer_version]]

    # Keys that exist in newer version but not in older (new keys)
    new <- setdiff(newer_keys, older_keys)
    if (length(new) > 0) {
      new_keys[[paste0(older_version, "_to_", newer_version)]] <- data.frame(
        older_version = older_version,
        newer_version = newer_version,
        new_keys = new
      )
    }
  }

  if (length(new_keys) > 0) {
    new_keys_df <- bind_rows(new_keys)
    message("New keys added between versions:")
    print(new_keys_df)
  }
})

test_that("EPI Rate Wide - values are similar across versions", {
  epi_list <- load_epi_wide_versions()

  # Get common metric keys (keys that exist in all versions)
  # We only compare values for keys that exist in all versions
  metric_keys_list <- lapply(epi_list, get_metric_keys)
  common_keys <- Reduce(intersect, metric_keys_list)

  # Use common keys for comparison
  metric_keys <- common_keys

  # Convert to long format for comparison
  epi_long_list <- lapply(names(epi_list), function(v) {
    epi_list[[v]] %>%
      mutate(version = v) %>%
      filter(location_level == 3, iso3 %in% KEY_COUNTRIES) %>%
      select(location_id, iso3, estimate, version, all_of(metric_keys)) %>%
      tidyr::pivot_longer(cols = all_of(metric_keys), names_to = "metric_key", values_to = "val")
  })

  combined <- bind_rows(epi_long_list) %>%
    filter(estimate == "central") %>%
    select(location_id, iso3, metric_key, version, val) %>%
      tidyr::pivot_wider(names_from = version, values_from = val)

  # Compare pairwise
  version_cols <- VERSIONS
  version_pairs <- utils::combn(version_cols, 2, simplify = FALSE)

    for (pair in version_pairs) {
        v1 <- pair[1]
        v2 <- pair[2]

    comparison_pair <- combined %>%
      filter(!is.na(!!sym(v1)) & !is.na(!!sym(v2))) %>%
      mutate(
        rel_diff = abs((!!sym(v2) / !!sym(v1)) - 1),
        abs_diff = abs(!!sym(v2) - !!sym(v1))
      )

    # Check that values are not identical (they should differ slightly)
    testthat::expect_true(
      any(comparison_pair$abs_diff > 0, na.rm = TRUE),
      info = paste("Values are identical between", v1, "and", v2)
    )

    # Check that relative differences are reasonable (most should be < 0.5 or 50%)
    # Some metrics may have larger differences, so we check median
    median_rel_diff <- median(abs(comparison_pair$rel_diff), na.rm = TRUE)
    testthat::expect_true(
      median_rel_diff < 0.2,
      info = paste("Median relative difference between", v1, "and", v2, "is", median_rel_diff)
    )

    max_rel_diff <- max(abs(comparison_pair$rel_diff), na.rm = TRUE)
    testthat::expect_true(
      max_rel_diff < 5,
      info = paste("Max relative difference between", v1, "and", v2, "is", max_rel_diff)
    )
  }
})

test_that("EPI Rate Wide - completeness checks", {
  epi_list <- load_epi_wide_versions()

  for (v in names(epi_list)) {
    epi <- epi_list[[v]]
    metric_keys <- get_metric_keys(epi)

    # Check for duplicates
    duplicates <- epi %>%
      filter(location_level == 3) %>%
      filter(!is.na(location_id)) %>%
      group_by(location_id, estimate) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 1)

    testthat::expect_equal(
      nrow(duplicates),
      0,
      info = paste("Found duplicates in", v)
    )

    # Check that each location_id (except NA) has high, low, and central estimates
    location_estimates <- epi %>%
      filter(!is.na(location_id), location_level == 3) %>%
      group_by(location_id) %>%
      summarise(
        has_central = "central" %in% estimate,
        has_low = "low" %in% estimate,
        has_high = "high" %in% estimate,
        .groups = "drop"
      ) %>%
      filter(!has_central | !has_low | !has_high)

    testthat::expect_equal(
      nrow(location_estimates),
      0,
      info = paste("Missing estimates in", v, ":\n",
                   paste(capture.output(print(location_estimates)), collapse = "\n"))
    )

    # Check that no value is NA in any of the metric columns
    na_values <- epi %>%
      filter(!is.na(location_id), location_level == 3) %>%
      select(location_id, estimate, all_of(metric_keys)) %>%
      tidyr::pivot_longer(cols = all_of(metric_keys), names_to = "metric_key", values_to = "val") %>%
      filter(is.na(val))

    testthat::expect_equal(
      nrow(na_values),
      0,
      info = paste("Found NA values in metric columns in", v, ":\n",
                   paste(capture.output(print(head(na_values, 20))), collapse = "\n"))
    )
  }
})

# ============================================================================
# EPI COUNT LONG TESTS
# ============================================================================

test_that("EPI Count Long - all versions have same cause/age/measure combinations", {

  epi_list <- load_epi_long_versions()

  # Get combinations for each version
  combos_list <- lapply(epi_list, get_cause_age_combos)

  # Find all combinations across all versions
  all_combos <- bind_rows(
    lapply(names(combos_list), function(v) {
      combos_list[[v]] %>% mutate(version = v)
    })
  ) %>%
    distinct()

  # Get all unique combinations (union across all versions)
  all_unique_combos <- all_combos %>%
    select(cause, age, measure_name) %>%
    distinct()

  # Create expected combinations: all unique combos × all versions
  expected_combos <- all_unique_combos %>%
    tidyr::crossing(version = VERSIONS)

  # Find missing combinations (expected but not present)
  missing_combos <- expected_combos %>%
    anti_join(all_combos, by = c("cause", "age", "measure_name", "version")) %>%
    # Remove allowed missing combinations
    anti_join(ALLOWED_MISSING_COMBOS_EPI_LONG, by = c("cause", "age", "measure_name", "version"))

  # Test should fail if there are any unauthorised missing combinations
  testthat::expect_equal(
    nrow(missing_combos),
    0,
    info = glue::glue("Found {nrow(missing_combos)} unauthorised missing cause/age/measure combinations")
  )

  # At least check that we have the essential causes
  for (v in names(epi_list)) {
    causes_in_version <- unique(epi_list[[v]]$cause)
    missing_causes <- setdiff(MUST_HAVE_CAUSES, causes_in_version)
    testthat::expect_equal(
      length(missing_causes),
      0,
      info = paste("Missing essential causes in", v, ":", paste(missing_causes, collapse = ", "))
    )
  }
})

test_that("EPI Count Long - values are similar across versions", {
  epi_list <- load_epi_long_versions()

  # Compare values for key countries
  combined <- bind_rows(
    lapply(names(epi_list), function(v) {
      epi_list[[v]] %>%
        mutate(version = v) %>%
        filter(location_level == 3, iso3 %in% KEY_COUNTRIES) %>%
        select(location_id, iso3, cause, age, measure_name, version, val)
    })
  ) %>%
    tidyr::pivot_wider(names_from = version, values_from = val)

  # Compare pairwise
  version_pairs <- utils::combn(VERSIONS, 2, simplify = FALSE)

      for (pair in version_pairs) {
        v1 <- pair[1]
        v2 <- pair[2]

    comparison_pair <- combined %>%
      filter(!is.na(!!sym(v1)) & !is.na(!!sym(v2))) %>%
      mutate(
        rel_diff = abs((!!sym(v2) / !!sym(v1)) - 1),
        abs_diff = abs(!!sym(v2) - !!sym(v1))
        )

        # Check that values are not identical
        testthat::expect_true(
      any(comparison_pair$abs_diff > 0, na.rm = TRUE),
      info = paste("Values are identical between", v1, "and", v2)
    )

    # Check that relative differences are reasonable
    # Exclude some known problematic combinations (e.g., COPD for young ages)
    comparison_filtered <- comparison_pair %>%
      filter(!(age %in% c("5-9", "10-14", "<5", "Under 5") & cause == "COPD"))

    if (nrow(comparison_filtered) > 0) {
      median_rel_diff <- median(comparison_filtered$rel_diff, na.rm = TRUE)
          testthat::expect_true(
        median_rel_diff < 0.5,
        info = paste("Median relative difference between", v1, "and", v2, "is", median_rel_diff)
      )
    }
  }
})

test_that("EPI Count Long - completeness checks", {
  epi_list <- load_epi_long_versions()

  # Check for duplicates
  for (v in names(epi_list)) {
    duplicates <- epi_list[[v]] %>%
      filter(location_level == 3) %>%
      group_by(location_id, cause, age, measure_name, sex_name) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 1)

    testthat::expect_equal(
      nrow(duplicates),
      0,
      info = paste("Found duplicates in", v)
    )
  }

  # Check that allowed missing countries are consistent
  for (v in names(epi_list)) {
    missing_iso3s <- epi_list[[v]] %>%
      filter(location_level == 3) %>%
      filter(is.na(val)) %>%
      pull(iso3) %>%
      unique()

    # Some combinations are expected to be missing (e.g., COPD for young ages)
    # So we check at the country level - countries that are completely missing
    completely_missing <- epi_list[[v]] %>%
    filter(location_level == 3) %>%
      group_by(iso3) %>%
      summarise(all_missing = all(is.na(val)), .groups = "drop") %>%
      filter(all_missing) %>%
    pull(iso3) %>%
    unique()

    unexpected_missing <- setdiff(completely_missing, ALLOWED_MISSING_ISO3S)
    testthat::expect_equal(
      length(unexpected_missing),
      0,
      info = paste("Unexpected completely missing countries in", v, ":",
                   paste(unexpected_missing, collapse = ", "))
    )
  }
})
