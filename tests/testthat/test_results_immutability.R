# Unless otherwise specified, we want central estimates of health impacts numbers to remain the same
# across versions.
# This is an initial test. As new versions come up, it might be beneficial to include more cases
# and allow for estimates to vary in certain version changes.
get_fingerprint_bgd <- function(params = list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020)){

  # Get PM2.5 exposure raster over Bangladesh with resolution 0.01deg
  current_version <- as.character(packageVersion("creahia"))
  res <- 0.01
  m <- terra::rast(
    xmin=88,
    xmax=92,
    ymin=20,
    ymax=27,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  m[] <- 60

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  p1 <- -m
  p2 <- 5-m

  hia <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = list(pm25 = p1),
    baseline_rasters = list(pm25 = m),
    pop_year = params$pop_year,
    administrative_level = 1,
    administrative_res = "low",
    administrative_iso3s = "BGD",
    epi_version = params$epi_version,
    calc_causes = params$calc_causes
  )

  # Add metadata
  hia$version <- current_version
  hia$epi_version <- params$epi_version
  hia$calc_causes <- params$calc_causes
  hia$pop_year <- params$pop_year

  return(hia)
}

# Create a filename from parameters
params_to_filename <- function(params) {
  param_string <- paste(gsub(" |_|-","",tolower(params)), sep = "_", collapse = "_")
  return(param_string)
}

# Load a specific version of the package
load_package_version <- function(ref, force=FALSE) {
  if (ref == "current") {
    # Load the current version from local directory
    devtools::load_all(here::here(), quiet = TRUE)
  } else {
    # Install from GitHub with specific ref
    remotes::install_github("energyandcleanair/creahia", ref = ref, upgrade = FALSE, force=FALSE)
    creahelpers::reload_packages("creahia")
  }
}

generate_fingerprint <- function(ref,
                                 params = list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020),
                                 force = FALSE,
                                 force_current = TRUE){


  # Create the filepath
  param_string <- params_to_filename(params)
  package_root <- here::here()
  fingerprint_dir <- file.path(package_root, "inst", "testdata", "fingerprints")
  filepath <- file.path(fingerprint_dir, glue("{ref}_hia_bgd_{param_string}.csv"))

  # Check if file exists and force is FALSE
  current_force <- if(ref == "current") force_current else force
  if (file.exists(filepath) && !current_force) {
    message(glue("Fingerprint for version {ref} with params {param_string} already exists. Skipping."))
    return(NULL)
  }

  # Load the package version
  load_package_version(ref, force = current_force)

  # Get the loaded version
  version <- as.character(packageVersion("creahia"))
  message(glue("Loaded creahia version {version} for ref {ref}"))

  # Generate the fingerprint
  hia <- get_fingerprint_bgd(params = params)

  # Add ref in case it differs from version
  hia$ref <- ref

  # Save it
  dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
  write.csv(hia, filepath, row.names = FALSE)

  message(glue("Generated fingerprint for ref {ref} with params {param_string}"))
  return(filepath)
}


generate_fingerprints <- function(refs=c("0.4.1", "0.4.2", "0.4.3", "0.4.4", "0.5.0", "0.5.1", "current"),
                                 param_sets = list(
                                   list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020),
                                   list(calc_causes = "GEMM and GBD", epi_version = "gbd2019", pop_year = 2020)
                                 ),
                                 force = FALSE, force_current = TRUE){
  generated_files <- list()

  # Process each ref version
  for(ref in refs){

    # Then process all parameter sets for this version
    for(params in param_sets){
      generated_file <- generate_fingerprint(ref=ref, params=params, force=force, force_current=force_current)
      generated_files <- c(generated_files, generated_file)
    }
  }

  return(generated_files)
}


read_fingerprint <- function(filepath){
  fingerprint <- read_csv(filepath)

  # Rename columns to be compatible through different versions
  names(fingerprint) <- gsub("Outcome", "outcome", names(fingerprint))
  names(fingerprint) <- gsub("Pollutant", "pollutant", names(fingerprint))
  names(fingerprint) <- gsub("Cause", "cause", names(fingerprint))
  names(fingerprint) <- gsub("AgeGrp", "age_group", names(fingerprint))

  fingerprint <- fingerprint %>%
    mutate(outcome = recode(outcome, exac="AsthmaERV"),
           cause = recode(cause,
                          Asthma.0to17="Asthma",
                          Asthma.18to99="Asthma"
                          ))


  return(fingerprint)
}


read_fingerprints <- function(){
  # Get the package root directory
  package_root <- here::here()
  fingerprint_dir <- file.path(package_root, "inst", "testdata", "fingerprints")
  tibble(filepath=list.files(fingerprint_dir, full.names = TRUE)) %>%
    rowwise() %>%
    mutate(hia = list(read_fingerprint(filepath))) %>%
    unnest(hia)
}

# Helper function to detect missing cause/outcome pairs and identify unauthorised ones
detect_missing <- function(data, authorised_missing) {
  # Get all central estimates
  central_data <- data %>%
    filter(estimate == "central")

  # Find all possible cause/outcome combinations
  all_combinations <- central_data %>%
    select(scenario, region_id, pollutant, outcome, cause, age_group, calc_causes, epi_version) %>%
    distinct()

  # Find which combinations are missing in each version
  missing_pairs <- central_data %>%
    group_by(scenario, region_id, pollutant, outcome, cause, age_group, calc_causes, epi_version) %>%
    summarise(versions = list(unique(version)), .groups = "drop") %>%
    mutate(
      all_versions = list(unique(central_data$version)),
      missing_versions = map2(all_versions, versions, ~ setdiff(.x, .y))
    ) %>%
    unnest(missing_versions) %>%
    select(
      version = missing_versions,
      cause, outcome, calc_causes, epi_version
    ) %>%
    ungroup()

  # Join with authorised missing to identify unauthorised ones
  if (nrow(missing_pairs) > 0) {
    missing_with_auth <- missing_pairs %>%
      left_join(authorised_missing, by = c("cause", "outcome", "calc_causes", "epi_version", "version")) %>%
      mutate(
        is_authorised = !is.na(description),
        unauthorised_missing = !is_authorised
      )
  } else {
    missing_with_auth <- missing_pairs %>%
      mutate(
        is_authorised = FALSE,
        unauthorised_missing = FALSE
      )
  }

  return(missing_with_auth)
}

# Helper function to detect breaks and identify unauthorised ones
detect_breaks <- function(data, authorised_breaks) {

  # Get all central estimates, arrange by version
  central_data <- data %>%
    filter(estimate == "central") %>%
    arrange(ref)

  # Detect breaks by comparing consecutive versions
  breaks <- central_data %>%
    group_by(scenario, region_id, pollutant, outcome, cause, age_group, calc_causes, epi_version, pop_year) %>%
    arrange(ref) %>%
    mutate(
      prev_number = lag(number),
      prev_ref = lag(ref),
      is_break = !is.na(prev_number) & abs(number - prev_number) > 0.1
    ) %>%
    filter(is_break) %>%
    select(
      ref = ref, # from_ref means the ref that introduced the break
      cause, outcome, calc_causes, epi_version,
      prev_value = prev_number,
      current_value = number
    ) %>%
    ungroup()

  # Join with authorised breaks to identify unauthorised ones
  if (nrow(breaks) > 0) {
    breaks_with_auth <- breaks %>%
      left_join(authorised_breaks, by = c("cause", "ref"="from_ref")) %>%
      mutate(
        is_authorised = !is.na(description),
        unauthorised_break = !is_authorised
      )
  } else {
    breaks_with_auth <- breaks %>%
      mutate(
        is_authorised = FALSE,
        unauthorised_break = FALSE
      )
  }

  return(breaks_with_auth)
}



test_that("Estimates are compatible with previous versions", {

  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)
  library(glue)
  library(tidyr)

  readRenviron(".Renviron")

  # Define parameter sets for versions before GBD2021 support (< 0.6.0)
  param_sets_gbd2019 <- list(
    list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020),
    list(calc_causes = "GEMM and GBD", epi_version = "gbd2019", pop_year = 2020)
  )

  # Define parameter sets for versions with GBD2021 support (>= 0.6.0)
  param_sets_gbd2021 <- list(
    list(calc_causes = "GBD only", epi_version = "gbd2021", pop_year = 2020),
    list(calc_causes = "GEMM and GBD", epi_version = "gbd2021", pop_year = 2020),
    list(calc_causes = "GEMM and GBD", epi_version = "gbd2021", pop_year = 2021)
  )

  # Generate fingerprints for older versions (before GBD2021)
  generate_fingerprints(refs = c("0.4.1",
                                 "0.4.4",
                                 "0.5.0",
                                 "0.5.1"),
                        param_sets = param_sets_gbd2019,
                        force = F,
                        force_current = F)

  # Generate fingerprints for newer versions (with GBD2021)
  generate_fingerprints(refs = c("0.6.0",
                                 "0.6.1",
                                 "0.7.0",
                                 "current"),
                        param_sets = c(param_sets_gbd2021, param_sets_gbd2019),
                        force = F,
                        force_current = T)

  # Read all fingerprints
  all_fingerprints <- read_fingerprints() %>%
    # We only compare central values
    # Confidence intervals are expected to change more frequently
    filter(estimate == "central") %>%
    # Override ref with version for consistency
    mutate(version = coalesce(ref, version),
           ref = version
           )


  ####################################################
  # Test all causes/outcomes are there
  ####################################################

  # Define authorised missing cause/outcome pairs
  authorised_missing1 <- tibble(
    outcome = rep("YLLs", 10),
    cause = rep(c("COPD", "IHD", "LC", "LRI", "Stroke"), 2),
    calc_causes = rep("GEMM and GBD", 10),
    epi_version = rep("gbd2019", 10),
    version = rep(c("0.4.1", "0.4.4"), each = 5),
    description = rep("YLLs that were NA in previous versions but have values in newer versions", 10)
  )

  authorised_missing2 <- tibble(
    epi_version = "gbd2021",
    version = c("0.4.1", "0.4.4","0.5.0","0.5.1"),
    description = "No gbd 2021 for these versions"
  ) %>%
    crossing(
      all_fingerprints %>% distinct(cause, outcome, calc_causes)
    )

  # Detect missing cause/outcome pairs and identify unauthorised ones
  missing_analysis <- detect_missing(all_fingerprints,
                                    authorised_missing = bind_rows(authorised_missing1, authorised_missing2))

  # Show which missing pairs were detected
  if (nrow(missing_analysis) > 0) {
    cat("\nDetected missing cause/outcome pairs:\n")
    print(missing_analysis %>%
          select(version, cause, outcome, calc_causes, epi_version, is_authorised, unauthorised_missing))

    # Show authorised vs unauthorised missing
    authorised_count <- sum(missing_analysis$is_authorised)
    unauthorised_count <- sum(missing_analysis$unauthorised_missing)

    cat(glue("\nMissing summary: {authorised_count} authorised, {unauthorised_count} unauthorised\n"))
  } else {
    cat("\nNo missing cause/outcome pairs detected - all versions have consistent pairs\n")
  }

  # Test should fail if there are any unauthorised missing pairs
  unauthorised_missing <- missing_analysis %>% filter(unauthorised_missing)

  testthat::expect_equal(nrow(unauthorised_missing), 0,
                         info = glue("Found {nrow(unauthorised_missing)} unauthorised missing cause/outcome pairs"))



  ####################################################
  # Test all estimates are identical (detect breaks)
  ####################################################

  # Define authorised breaks where values are expected to change between specific versions
  authorised_breaks1 <- tibble(
    cause = c("PTB", "LBW", "Absences"),
    from_ref = "0.5.1",
    description = "Epidemiological data update at version 0.5.1"
  )

  authorised_breaks2 <- tibble(
    from_ref = "0.6.1",
    description = "Small changes (less than 1%). Updated population projection data."
  ) %>%
    crossing(
      all_fingerprints %>% distinct(cause)
    )

  # Detect all breaks and identify unauthorised ones
  breaks_analysis <- detect_breaks(all_fingerprints, authorised_breaks = bind_rows(authorised_breaks1, authorised_breaks2))

  # Show which breaks were detected
  if (nrow(breaks_analysis) > 0) {
    cat("\nDetected breaks:\n")
    print(breaks_analysis %>%
          select(ref, cause, outcome, calc_causes,
                 prev_value, current_value, is_authorised, unauthorised_break))

    # Show authorised vs unauthorised breaks
    authorised_count <- sum(breaks_analysis$is_authorised)
    unauthorised_count <- sum(breaks_analysis$unauthorised_break)

    cat(glue("\nBreak summary: {authorised_count} authorised, {unauthorised_count} unauthorised\n"))
  } else {
    cat("\nNo breaks detected - all values are consistent across versions\n")
  }

  # Test should fail if there are any unauthorised breaks
  unauthorised_breaks <- breaks_analysis %>% filter(unauthorised_break)

  testthat::expect_equal(nrow(unauthorised_breaks), 0,
                         info = glue("Found {nrow(unauthorised_breaks)} unauthorised breaks in central values"))


  # Show sample data for debugging
  if(F){
    all_fingerprints %>%
      # filter(version=="0.4.1") %>%

      select(-c(filepath, pop, version)) %>%
      filter(region_name=="Dhaka") %>%
      select(-c(estimate, region_id, region_name, scenario, iso3)) %>%
      mutate(number = round(number, 1)) %>%
      group_by(cause, outcome, age_group, calc_causes, epi_version) %>%
      mutate(ok = n_distinct(round(number)) == 1) %>%
      spread(ref, number) %>%
      View()
  }
})
