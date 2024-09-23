#' Convert geoschem netcdf file to raster (tif)
#'
#' @param nc_file
#' @param var_name
#' @param lat_name
#' @param lon_name
#'
#' @return
#' @export
#'
#' @examples
rasterise_geoschem_nc <- function(nc_file, var_name, lat_name = 'lat', lon_name = 'lon') {
  nc <- nc_open(nc_file)

  if(!var_name %in% names(nc$var)){
    stop(glue('Variable "{var_name}" not found in the netcdf file.'))
  }

  lat <- enforce_regular_dim(nc, lat_name)
  lon <- enforce_regular_dim(nc, lon_name)

  x <- terra::rast(xmin = lon %>% min - (diff(lon)[[1]] / 2),
                   xmax = lon %>% max + (diff(lon)[[1]] / 2),
                   ymin = lat %>% min - (diff(lat)[[1]] / 2),
                   ymax = lat %>% max + (diff(lat)[[1]] / 2),
                   ncols = length(lon), nrows = length(lat))

  values(x) <- get_nc_values(nc = nc, var_name = var_name,
                             ncols = length(lon), nrows = length(lat),
                             lat_name = lat_name, lon_name = lon_name)

  plot(x)
  return(x)
}


enforce_regular_dim <- function(nc, dim_name){
  dim <- ncvar_get(nc, dim_name)

  dim_diff <- diff(dim)

  for(i in 1:5){
    if(!all(dim_diff == dim_diff[1])){
      print(glue('removing {i}th edges'))
      dim <- try_trim_edges(dim)
      dim_diff <- diff(dim)
    } else {
      break
    }
  }

  if(!all(dim_diff == dim_diff[1])){
    stop(paste('Lat/lon dimension of the grid is not regular even after removing up to 5 edges.',
               'Rasterising requires a regular grid. Please check the data.'))
  } else {
    dim
  }
}


try_trim_edges <- function(array){
  array <- array[c(-1, -length(array))]
}


get_nc_values <- function(nc, var_name, ncols, nrows, lat_name, lon_name){
  var <- ncvar_get(nc, var_name)

  ori_lat <- ncvar_get(nc, lat_name)
  ori_lon <- ncvar_get(nc, lon_name)

  # check if dimensions match
  if(!all(dim(var) == c(length(ori_lat), length(ori_lon)))){
    values <- t(var)
  } else {
    values <- var
  }

  # remove values if edges were trimmed
  if(length(ori_lat) != nrows){
    n_remove <- (length(ori_lat) - nrows) / 2
    values <- values[-c(1:n_remove, (length(ori_lat) - n_remove + 1): length(ori_lat)), ]
  }

  if(length(ori_lon) != ncols){
    n_remove <- (length(ori_lon) - ncols) / 2
    values <- values[, -c(1:n_remove, (length(ori_lon) - n_remove + 1): length(ori_lon))]
  }

  # flip values on x axis
  values[nrow(values):1,]
}
