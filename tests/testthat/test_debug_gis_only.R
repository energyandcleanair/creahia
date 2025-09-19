test_that("Debug GIS directory and get_adm function", {
  
  # Run our debugging helpers
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("RUNNING DEBUG-ONLY TEST TO INSPECT GIS DIRECTORY\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Debug the test environment
  debug_test_environment()
  
  # Try calling get_adm with debugging
  cat("\n", paste(rep("-", 40), collapse=""), "\n")
  cat("TESTING creahelpers::get_adm function\n")
  cat(paste(rep("-", 40), collapse=""), "\n")
  
  tryCatch({
    cat("Attempting to call creahelpers::get_adm(level=0, res='low', iso2s='BD')...\n")
    result <- creahelpers::get_adm(level = 0, res = "low", iso2s = "BD")
    cat("SUCCESS! Got", nrow(result), "administrative boundaries for Bangladesh\n")
    cat("Columns:", paste(colnames(result), collapse = ", "), "\n")
  }, error = function(e) {
    cat("ERROR in creahelpers::get_adm():\n")
    cat("Message:", e$message, "\n")
    cat("Call:", deparse(e$call), "\n")
    
    # Try to get more details about the error
    if (grepl("full_res_file", e$message)) {
      cat("\nThis is the 'full_res_file' error we're trying to debug!\n")
    }
  })
  
  # Test with different parameters
  cat("\n", paste(rep("-", 40), collapse=""), "\n")
  cat("TESTING different get_adm parameters\n")
  cat(paste(rep("-", 40), collapse=""), "\n")
  
  test_params <- list(
    list(level = 0, res = "low", iso2s = "BD"),
    list(level = 0, res = "full", iso2s = "BD"),
    list(level = 1, res = "low", iso2s = "BD")
  )
  
  for (i in seq_along(test_params)) {
    params <- test_params[[i]]
    cat("Test", i, ": level=", params$level, ", res='", params$res, "', iso2s='", params$iso2s, "'\n", sep="")
    
    tryCatch({
      result <- creahelpers::get_adm(level = params$level, res = params$res, iso2s = params$iso2s)
      cat("  SUCCESS! Got", nrow(result), "boundaries\n")
    }, error = function(e) {
      cat("  ERROR:", e$message, "\n")
    })
  }
  
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("END OF DEBUG-ONLY TEST\n")
  cat(paste(rep("=", 60), collapse=""), "\n\n")
  
  # Always pass this test - we're just debugging
  expect_true(TRUE)
})
