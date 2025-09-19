#' Debug GIS Directory Contents
#'
#' @return
#' @export
debug_gis_directory <- function() {
  cat("\n=== DEBUGGING GIS DIRECTORY ===\n")
  
  # Check GIS_DIR environment variable
  gis_dir <- Sys.getenv("GIS_DIR")
  cat("GIS_DIR environment variable:", gis_dir, "\n")
  
  # Check creahelpers::get_gis_dir()
  if (requireNamespace("creahelpers", quietly = TRUE)) {
    tryCatch({
      gis_dir_func <- creahelpers::get_gis_dir()
      cat("creahelpers::get_gis_dir():", gis_dir_func, "\n")
    }, error = function(e) {
      cat("Error calling creahelpers::get_gis_dir():", e$message, "\n")
    })
  }
  
  # Check if GIS directory exists
  if (dir.exists(gis_dir)) {
    cat("GIS directory exists:", gis_dir, "\n")
    
    # List top-level contents
    cat("Top-level contents:\n")
    top_files <- list.files(gis_dir, full.names = FALSE)
    for (f in top_files) {
      cat("  -", f, "\n")
    }
    
    # Check boundaries directory
    boundaries_dir <- file.path(gis_dir, "boundaries")
    if (dir.exists(boundaries_dir)) {
      cat("\nBoundaries directory contents:\n")
      boundaries_files <- list.files(boundaries_dir, recursive = TRUE, full.names = FALSE)
      for (f in boundaries_files) {
        cat("  - boundaries/", f, "\n")
      }
    } else {
      cat("Boundaries directory does not exist:", boundaries_dir, "\n")
    }
    
    # Check population directory
    population_dir <- file.path(gis_dir, "population")
    if (dir.exists(population_dir)) {
      cat("\nPopulation directory contents:\n")
      population_files <- list.files(population_dir, full.names = FALSE)
      for (f in population_files) {
        cat("  - population/", f, "\n")
      }
    } else {
      cat("Population directory does not exist:", population_dir, "\n")
    }
    
  } else {
    cat("GIS directory does not exist:", gis_dir, "\n")
  }
  
  cat("=== END GIS DIRECTORY DEBUG ===\n\n")
}

#' Debug get_adm function
#'
#' @return
#' @export
debug_get_adm <- function() {
  cat("\n=== DEBUGGING get_adm FUNCTION ===\n")
  
  # Check if creahelpers is available
  if (!requireNamespace("creahelpers", quietly = TRUE)) {
    cat("creahelpers package not available\n")
    return()
  }
  
  # Check if creahia masks get_adm
  cat("Checking function masking:\n")
  if (exists("get_adm", envir = asNamespace("creahia"))) {
    cat("  - creahia::get_adm exists\n")
  }
  if (exists("get_adm", envir = asNamespace("creahelpers"))) {
    cat("  - creahelpers::get_adm exists\n")
  }
  
  # Try to call creahelpers::get_adm with debug info
  cat("\nTesting creahelpers::get_adm with level=0, res='low', iso2s='BD':\n")
  tryCatch({
    result <- creahelpers::get_adm(level = 0, res = "low", iso2s = "BD")
    cat("  - Success! Got", nrow(result), "rows\n")
    cat("  - Columns:", paste(names(result), collapse = ", "), "\n")
  }, error = function(e) {
    cat("  - Error:", e$message, "\n")
    cat("  - Full error:\n")
    print(e)
  })
  
  # Try to call with different parameters
  cat("\nTesting creahelpers::get_adm with level=0, res='full', iso2s='BD':\n")
  tryCatch({
    result <- creahelpers::get_adm(level = 0, res = "full", iso2s = "BD")
    cat("  - Success! Got", nrow(result), "rows\n")
  }, error = function(e) {
    cat("  - Error:", e$message, "\n")
  })
  
  cat("=== END get_adm DEBUG ===\n\n")
}

#' Debug helper that runs before tests
#'
#' @return
#' @export
debug_test_environment <- function() {
  cat("\n" , rep("=", 60), "\n")
  cat("STARTING TEST DEBUG SESSION\n")
  cat(rep("=", 60), "\n")
  
  debug_gis_directory()
  debug_get_adm()
  
  cat(rep("=", 60), "\n")
  cat("END TEST DEBUG SESSION\n")
  cat(rep("=", 60), "\n\n")
}
