get_rr <- function(version){
  read_csv(get_hia_path(glue("rr_{version}.csv")), col_types = cols())
}
