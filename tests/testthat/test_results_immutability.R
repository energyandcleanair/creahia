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

# Install a specific version of the package
install_package_version <- function(ref, force=FALSE) {
  if (ref == "current") {
    # Install the current version from local directory
    remotes::install_local(".", upgrade = FALSE, force=force)
  } else {
    # Install from GitHub with specific ref
    remotes::install_github("energyandcleanair/creahia", ref = ref, upgrade = FALSE, force=FALSE)
  }
  creahelpers::reload_packages("creahia")
}

generate_fingerprint <- function(ref, params = list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020),
                               force = FALSE){


  # Create the filepath
  param_string <- params_to_filename(params)
  filepath <- glue("tests/data/versions/{ref}_hia_bgd_{param_string}.csv")

  # Check if file exists and force is FALSE
  if (file.exists(filepath) && !force && ref != "current") {
    message(glue("Fingerprint for version {ref} with params {param_string} already exists. Skipping."))
    return(NULL)
  }

  # Install the package version
  install_package_version(ref)

  # Get the installed version
  # version <- as.character(packageVersion("creahia"))

  # Generate the fingerprint
  hia <- get_fingerprint_bgd(params = params)

  # Save it
  dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
  write.csv(hia, filepath, row.names = FALSE)

  message(glue("Generated fingerprint for ref {ref} with params {param_string}"))
  return(filepath)
}

generate_fingerprints <- function(refs=c("0.4.1", "0.4.2", "0.4.3", "0.4.4", "current"),
                                 param_sets = list(
                                   list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020),
                                   list(calc_causes = "GEMM and GBD", epi_version = "gbd2019", pop_year = 2020)
                                 ),
                                 force = FALSE){
  generated_files <- list()

  # Process each ref version
  for(ref in refs){


    # Then process all parameter sets for this version
    for(params in param_sets){

      # Always force regeneration for current version
      current_force <- if(ref == "current") TRUE else force

      # Create the filepath
      param_string <- params_to_filename(params)
      filepath <- glue("tests/data/versions/{ref}_hia_bgd_{param_string}.csv")

      # Check if file exists and force is FALSE
      if (file.exists(filepath) && !current_force && ref != "current") {
        message(glue("Fingerprint for version {ref} with params {param_string} already exists. Skipping."))
        next
      }

      # Install the package version once per ref
      install_package_version(ref, force = current_force)

      # Generate the fingerprint
      hia <- get_fingerprint_bgd(params = params)

      # Save it
      dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
      write.csv(hia, filepath, row.names = FALSE)

      message(glue("Generated fingerprint for ref {ref} with params {param_string}"))
      generated_files <- c(generated_files, filepath)
    }
  }

  return(generated_files)
}

read_fingerprints <- function(){
  tibble(filepath=list.files("tests/data/versions", full.names = TRUE)) %>%
    rowwise() %>%
    mutate(hia = list(read_csv(filepath))) %>%
    select(-filepath) %>%
    unnest(hia)
}



test_that("Estimates are compatible with previous versions", {
  library(terra)
  library(creahelpers)
  library(dplyr)
  library(creahia)
  library(creaexposure)
  library(glue)
  library(tidyr)

  # Define parameter sets to test
  param_sets <- list(
    list(calc_causes = "GBD only", epi_version = "gbd2019", pop_year = 2020),
    list(calc_causes = "GEMM and GBD", epi_version = "gbd2019", pop_year = 2020)
  )

  generate_fingerprints(refs = c("0.4.1", "0.4.2", "0.4.3", "0.4.4", "current"), param_sets = param_sets, force = FALSE)

  # Read all fingerprints
  all_fingerprints <- read_fingerprints() %>%
    # We only compare central values
    # Confidence intervals are expected to change more frequently
    filter(estimate == "central")


  ####################################################
  # Test all causes/outcomes are there
  ####################################################

  # Define known exceptions where differences are expected
  known_missing_exceptions <- list(
    # YLLs that were NA in previous versions but have values in newer versions
    tibble(
      Outcome = "YLLs",
      Cause = c("COPD", "IHD", "LC", "LRI", "Stroke"),
      calc_causes = "GEMM and GBD",
      version_condition = "version < '0.5.0'",
    )
  )

  # First check: ensure all versions have the same cause/outcome pairs after removing exceptions
  missing_pairs <- all_fingerprints %>%
    group_by(scenario, region_id, Pollutant, Outcome, Cause, AgeGrp, calc_causes, epi_version) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n < length(unique(all_fingerprints$version))) %>%
    distinct(calc_causes, epi_version, Cause, Outcome, n)

  # Filter out known exceptions from missing_pairs
  for (exception in known_missing_exceptions) {
    join_cols <- setdiff(names(exception), "version_condition")
    version_condition <- exception$version_condition[1]
    missing_pairs <- anti_join(missing_pairs,
                              exception %>% select(-version_condition),
                              by = join_cols)
  }

  testthat::expect_equal(nrow(missing_pairs), 0,
                     info = glue("Some versions are missing cause/outcome pairs"))



  ####################################################
  # Test all estimates are identical
  ####################################################
  filtered_fingerprints <- all_fingerprints

  # Second check: ensure values are consistent across versions
  different <- filtered_fingerprints %>%
    group_by(scenario, region_id, Pollutant, Outcome, Cause, AgeGrp, calc_causes, epi_version) %>%
    # Round to 1 decimal place to ignore tiny issues
    mutate(number = round(number, 1)) %>%
    summarise(unique = n_distinct(number), .groups = "drop") %>%
    filter(unique > 1) %>%
    distinct(calc_causes, epi_version, Cause, Outcome)

  testthat::expect_equal(nrow(different), 0,
                         info = glue("Different central values"))
})
