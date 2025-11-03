skip("Only need to be run when creating new epi.")

testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("GBD2023, GBD2021, GBD2019 and GBD2017 have comparable data - input", {

  versions <- c('gbd2019', 'gbd2021', 'gbd2023')

  # Fetch population data for all versions
  pops <- lapply(versions, function(v) get_epi_pop(v))
  names(pops) <- versions

  # Fetch all data for all versions
  data_list <- list(
    crude = lapply(versions, function(v) get_death_crude(version = v)),
    child = lapply(versions, function(v) get_death_child_lri(pop = pops[[v]], version = v)),
    yld = lapply(versions, function(v) get_yld(pops[[v]], version = v)),
    asthma = lapply(versions, function(v) get_asthma_prev_and_inc(pops[[v]], version = v))
  )
  names(data_list$crude) <- versions
  names(data_list$child) <- versions
  names(data_list$yld) <- versions
  names(data_list$asthma) <- versions

  # Tolerance settings for each dataset
  tolerances <- list(
    crude = 0.25,
    child = 0.7, # Changed a lot in China
    yld = 2, # OthResp and Diabetes changed a lot
    asthma = 0.4
  )

  compare <- function(data_list_by_version,
                      tolerance = 0.1,
                      dataset_name = "",
                      versions_in = versions) {

    options(dplyr.summarise.inform = FALSE)
    allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                               "PLW", "KNA", "SMR", "TKL", "TUV")

    # Fill metric_name and/or measure_name if missing
    fill_cols <- function(df) {
      if(!"metric_name" %in% names(df)) df$metric_name <- df$var
      if(!"measure_name" %in% names(df)) df$measure_name <- df$estimate
      if(!"var" %in% names(df)) df$var <- df$cause_name
      df
    }

    # Combine all versions into single dataframe
    comparison <- bind_rows(
      lapply(names(data_list_by_version), function(v) {
        data_list_by_version[[v]] %>% mutate(version = v)
      })
    ) %>%
      ungroup() %>%
      fill_cols() %>%
      select(measure_name, location_id, iso3, location_level, metric_name, estimate, var, val, version) %>%
      tidyr::pivot_wider(names_from = version, values_from = val)

    # Calculate relative differences between versions dynamically
    version_cols <- intersect(versions_in, names(comparison))
    if (length(version_cols) >= 2) {
      # Create all pairwise combinations of versions
      version_pairs <- utils::combn(version_cols, 2, simplify = FALSE)

      # Calculate relative differences for all pairs
      for (pair in version_pairs) {
        v1 <- pair[1]
        v2 <- pair[2]
        rel_diff_col <- paste0("rel_diff_", v1, "_", v2)
        comparison[[rel_diff_col]] <- (comparison[[v2]] / comparison[[v1]]) - 1
      }
    }

    # Plot comparisons
    plot_data <- comparison %>%
      filter(location_level == 3) %>%
      filter(if_all(all_of(version_cols), ~ !is.na(.))) %>%
      tidyr::pivot_longer(cols = all_of(version_cols),
                          names_to = "version", values_to = "value") %>%
      dplyr::sample_n(min(1000, nrow(.))) # Sample for plotting if too many rows

    if (nrow(plot_data) > 0) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = version, y = value, fill = version)) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_y_log10() +
        ggplot2::facet_wrap(~var, scales = "free_y") +
        ggplot2::labs(title = paste("Comparison across versions:", dataset_name)) +
        ggplot2::theme_minimal()
      print(p)
    }

    # Check for missing ISO3s
    missing_iso3s <- comparison %>%
      filter(location_level == 3) %>%
      filter(if_all(all_of(version_cols), is.na)) %>%
      pull(iso3) %>%
      unique()

    testthat::expect_in(missing_iso3s, allowed_missing_iso3s)

    # Test that numbers are relatively close but not identical
    # across versions, for selected countries
    should_be_close_iso3s <- c('USA', 'CHN', 'IND', 'PHL', 'IDN', 'GBR', 'DEU', 'JPN')
    idx_iso3 <- comparison$iso3 %in% should_be_close_iso3s & (comparison$location_level == 3)

    # Dynamically compare all version pairs
    if (length(version_cols) >= 2) {
      version_pairs <- utils::combn(version_cols, 2, simplify = FALSE)

      for (pair in version_pairs) {
        v1 <- pair[1]
        v2 <- pair[2]

        # Check that values are within tolerance
        testthat::expect_equal(
          comparison[[v1]][idx_iso3],
          comparison[[v2]][idx_iso3],
          tolerance = tolerance
        )

        # Check that values are not identical
        testthat::expect_true(
          all(comparison[[v1]][idx_iso3] != comparison[[v2]][idx_iso3], na.rm = TRUE)
        )
      }

      # Check that max relative difference per measure is within tolerance
      if (nrow(comparison) > 0) {
        rel_diff_cols <- grep("^rel_diff_", names(comparison), value = TRUE)
        if (length(rel_diff_cols) > 0) {
          max_diffs <- comparison[idx_iso3, ] %>%
            dplyr::select(all_of(rel_diff_cols)) %>%
            dplyr::summarise(dplyr::across(everything(), ~ max(abs(.x), na.rm = TRUE))) %>%
            tidyr::pivot_longer(everything(), names_to = "pair", values_to = "max_rel_diff")

          testthat::expect_true(
            all(max_diffs$max_rel_diff <= tolerance, na.rm = TRUE),
            info = paste("Max relative differences:", paste(max_diffs$pair, max_diffs$max_rel_diff, sep = "=", collapse = ", "))
          )
        }
      }
    }

    # Check years - extract expected year from version name
    # expected_years <- as.numeric(gsub("gbd", "", versions_in))
    # for (i in seq_along(versions_in)) {
    #   print(i)
    #   version_year <- expected_years[i]
    #   version_name <- versions_in[i]
    #   if ("year" %in% names(data_list_by_version[[version_name]])) {
    #     testthat::expect_true(all(data_list_by_version[[version_name]]$year == version_year))
    #   }
    # }

    # Test that it has subnational data
    # comparison %>%
    #   filter(location_level == 4) %>%
    #   filter(if_all(all_of(version_cols), ~ !is.na(.))) %>%
    #   nrow() %>%
    #   testthat::expect_gt(100)

    # Test no duplicate
    bind_rows(
      lapply(names(data_list_by_version), function(v) {
        data_list_by_version[[v]] %>% mutate(version = v)
      })
    ) %>%
      fill_cols() %>%
      group_by(measure_name, location_id, iso3, location_level, metric_name, estimate, var, version) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      pull(n) %>%
      max() %>%
      testthat::expect_equal(1)
  }

  # Run comparisons for all datasets
  for (dataset_name in names(data_list)) {
    print(dataset_name)
    compare(data_list_by_version=data_list[[dataset_name]],
            tolerance = tolerances[[dataset_name]],
            dataset_name = dataset_name)
  }
})

test_that("GBD2021, GBD2019 and GBD2017 have comparable data - output", {

  epi2017 <- get_epi('gbd2017')
  epi2019 <- get_epi('gbd2019')
  epi2021 <- get_epi('gbd2021')

  comparison <- bind_rows(
    epi2017 %>% mutate(version='gbd2017'),
    epi2019 %>% mutate(version='gbd2019'),
    epi2021 %>% mutate(version='gbd2021')
  ) %>%
    select(-c(location_name)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, country, region, income_group, version)) %>%
    tidyr::spread(version, value)

  missing_iso3s <- comparison %>%
    filter(location_level==3,
           is.na(gbd2017 + gbd2019 + gbd2021)) %>%
    pull(iso3) %>%
    unique()

  allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                             "PLW", "KNA", "SMR", "TKL", "TUV")
  testthat::expect_in(missing_iso3s, allowed_missing_iso3s)


  # Plot comparison
  # bind_rows(
  #   epi2017 %>% mutate(version='gbd2017'),
  #   epi2019 %>% mutate(version='gbd2019'),
  #   epi2021 %>% mutate(version='gbd2021')
  # ) %>%
  #   select(-c(location_name)) %>%
  #   tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, country, region, income_group, version)) %>%
  #   filter(iso3=="ZAF", location_level==3) %>%
  #   ggplot() + geom_col(aes(version, value, fill=version), position='dodge') + facet_wrap(~name, scales='free_y')

})

test_that("GBD2021, GBD2019 and GBD2017 have comparable data - IHME", {

  ihme2017 <- get_epi_count_long('gbd2017')
  ihme2019 <- get_epi_count_long('gbd2019')
  ihme2021 <- get_epi_count_long('gbd2021')

  comparison <- bind_rows(
    ihme2017 %>% mutate(version='gbd2017'),
    ihme2019 %>% mutate(version='gbd2019'),
    ihme2021 %>% mutate(version='gbd2021')
  ) %>%
    filter(location_level == 3) %>%
    select(measure_name, location_id, iso3, location_level, age, cause, sex_name, estimate, val, version) %>%
    spread(version, val)

  missing_iso3s <- comparison %>%
    filter(location_level==3,
           # COPD for low age NA in new versions
           ! age %in% c("5-9", "10-14", "<5") | cause != "COPD",
           is.na(gbd2017 + gbd2019 + gbd2021)) %>%
    pull(iso3) %>%
    unique()

  allowed_missing_iso3s <- c("COK", "MCO", "NRU", "NIU",
                             "PLW", "KNA", "SMR", "TKL", "TUV")
  testthat::expect_in(missing_iso3s, allowed_missing_iso3s)
})

test_that("GBD2021, GBD2019 and GBD2017 yield roughly similar results - Philipines", {

  library(tidyverse)
  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)

  obs_map <- terra::rast(get_test_file('example_ph_who/obs_map.tif'))
  baseline_map <- terra::rast(get_test_file('example_ph_who/zero_map.tif'))
  perturbation_map <- obs_map - baseline_map
  names(perturbation_map) <- names(obs_map)

  hia <- lapply(c(
    # 'gbd2017',
    'gbd2019',
    'gbd2021'), function(epi_version){
    creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = raster::stack(perturbation_map),
      baseline_rasters = raster::stack(baseline_map),
      administrative_iso3s = c("PHL"),
      administrative_level = 0,
      crfs_version = "C40",
      epi_version = epi_version,
      rr_sources = RR_GBD2021
    ) %>%
      mutate(epi_version = epi_version)
  }) %>%
    bind_rows()


  unique(hia$Cause)

  comparison <- hia %>%
    filter(!double_counted,
           estimate=='central') %>%
    group_by(Outcome, double_counted, epi_version) %>%
    dplyr::summarise(value=sum(number, na.rm=T))


  # Test that deaths close to 65000
  comparison_outcome <- comparison %>%
    filter(!double_counted) %>%
    tidyr::spread(Outcome, value)
  comparison_outcome$Death_target <- 65000
  testthat::expect_equal(comparison_outcome$Deaths, comparison_outcome$Death_target , tolerance = 1e-1)

  comparison_epi <- comparison %>%
    tidyr::spread(epi_version, value)

  testthat::expect_true(sum(comparison_epi$gbd2017) > 0)
  # Values should be equal on non GBD
  non_gbd <- c("exac", "PTB", "LBW", "Absences")
  idx_non_gbd <- comparison_epi$Outcome %in% non_gbd
  testthat::expect_identical(comparison_epi$gbd2017[idx_non_gbd], comparison_epi$gbd2019[idx_non_gbd])

  # Not equal but close for GBD outcomes (except Deaths.child which has similar epi data apparently, at least in PHP)
  idx_gbd <- !idx_non_gbd
  testthat::expect_equal(comparison_epi$gbd2017[idx_gbd], comparison_epi$gbd2019[idx_gbd], tolerance=1e-1)
  testthat::expect_equal(comparison_epi$gbd2017[idx_gbd], comparison_epi$gbd2021[idx_gbd], tolerance=1e-1)
  testthat::expect_true(all(comparison_epi$gbd2017[idx_gbd]!=comparison_epi$gbd2019[idx_gbd]))
  testthat::expect_true(all(comparison_epi$gbd2017[idx_gbd]!=comparison_epi$gbd2021[idx_gbd]))

})


test_that("New epi doesn't differ too much from old one", {


  version <- "gbd2019"

  new <- read_csv(glue("inst/extdata/epi/processed/epi_rate_wide_{version}.csv"))
  old <- read_csv(glue("https://raw.githubusercontent.com/energyandcleanair/creahia/7966ab8f14bda553e4cfbb1eb59dd872d3217c84/inst/extdata/epi_for_hia_{version}.csv"))

  # It should not have changed central
  comparison <- bind_rows(
    new %>% mutate(source="new"),
    old %>% mutate(source="old")
  ) %>%
    filter(!is.na(location_id)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, location_name, country, region, income_group, pop, source),
                        names_to="var", values_to="value") %>%
    select(location_id, estimate, source, var, value) %>%
  spread(source, value) %>%
    mutate(diff_rel=(new/old)-1)

  # Should be perfectly matching
  # i.d. no NA in new or old
  missing <- comparison %>%
    filter(is.na(new) != is.na(old)) %>%
    nrow()

  testthat::expect_equal(missing, 0)


  different_central <- comparison %>%
    filter(estimate=='central') %>%
    filter(abs(diff_rel) > 0.001) %>%
    nrow()

  testthat::expect_equal(different_central, 0)


  # No low > central or high < central
  inversed <- new %>%
    filter(!is.na(location_id)) %>%
    tidyr::pivot_longer(cols = -c(location_id, location_level, iso3, estimate, location_name, country, region, income_group, pop),
                        names_to="var", values_to="value") %>%
    select(location_id, estimate, var, value) %>%
    spread(estimate, value) %>%
    filter(low > central | high < central | low > high) %>%
    nrow()

  testthat::expect_equal(inversed, 0)
})
