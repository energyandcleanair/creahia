get_examples_dir <- function(){
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
