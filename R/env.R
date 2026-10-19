# Package level environment
creahia.env <- new.env()

set_env <- function(variable, value){
  creahia.env[[variable]] <- value
}

get_env <- function(variable){
  creahia.env[[variable]]
}

#' List of package environment variables
#' and their correspondence with system environemnt
#'
#' @return
#' @export
#'
#' @examples
list_env_vars <- function(){
  list(
    gis_dir="GIS_DIR" #'gis_dir' is the variable name in package environment.
    #"GIS_DIR" is the system environment variable it will be initiated with if no variable with this name is found
  )
}


#' Initiate environment, that is, feeding creahia.env from
#'  1- Global R environment
#'  2- System environment
#'
#' @return
#' @export
#'
#' @examples
init_env <- function(){

  suppressWarnings(try(readRenviron(".Renviron"), silent = TRUE))
  suppressWarnings(try(dotenv::load_dot_env(), silent = TRUE))

  vars <- list_env_vars()

  print("Initiating environment")
  for(n in names(vars)){

    # First check R environment
    if(exists(n)){
      v.value <- get(n)
    }else{
      # Otherwise system environment
      v.value <- Sys.getenv(vars[[n]])
    }

    if(is.null(v.value) || v.value==""){
      warning(sprintf("%s not set. Please set it using `creahia::set_env(%s,'your_value').
                      You can also set the %s system environment variable once and for all`", n, n, vars[[n]]))
    }else{
      set_env(n, v.value)
      message(crayon::green(paste0(n,": ", v.value)))
    }
  }
}

.onLoad <- function(libname, pkgname) {
  init_env()
}

