get_examples_dir <- function(){
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    testthat::skip("Source-checkout examples are validated outside R CMD check")
  }

  dirs <- c("../../../examples", "../../examples", "../examples", "examples")
  dir <- dirs[which(dir.exists(dirs))]
  if(length(dir) == 0){
    stop("Could not find examples directory")
  }
  return(dir[1])
}

get_test_file <- function(filename){

  dir <- get_examples_dir()
  filepath <- file.path(dir, filename)
  if(!file.exists(filepath)){
    dir.create(dirname(filepath), F, T)
    url <- glue("https://storage.googleapis.com/crea-public/data/test/creahia/{filename}")
    download.file(url, filepath)
  }
  return(filepath)
}

get_testdata_file <- function(...) {
  installed_path <- system.file("testdata", ..., package = "creahia")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  testthat::test_path("..", "..", "inst", "testdata", ...)
}

copy_epi_data_update <- function(){
  # Copy data/epi_update
  dir_dest <- "data/epi_update"
  dir_org <- file.path("../../", dir_dest)
  dir.create(dir_dest, showWarnings = F, recursive = T)
  list.files(dir_org, "*.csv") %>%
    lapply(function(x){file.copy(file.path(dir_org, x), file.path(dir_dest, x))})

}


