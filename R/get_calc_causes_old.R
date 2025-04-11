#' Old way of doing things. Kept to test that the new way is retro-compatible
#'
#' @param calc_causes
#' @param gbd_causes
#'
#' @returns
#' @export
#'
#' @examples
get_calc_causes_old <- function(calc_causes = 'GEMM and GBD',
                                gbd_causes = 'default',
                                filter = NULL) {



  get_calc_causes_from_set <- function(causes_set, filter=NULL){

    if(causes_set == 'GEMM and GBD') {
      # define short names
      names_causes <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

      causes_out <- c(paste0(c('NCD.LRI', 'LRI.child'), '_YLLs'),
                      paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
                      paste0(c(names_causes, 'LRI.child', 'Diabetes'), '_Deaths')) %>%
        unique
    }

    if(causes_set == 'GEMM only') {
      # define short names
      names_causes <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

      causes_out <- c(paste0(c('NCD.LRI'), '_YLLs'),
                      paste0(c('Stroke', 'COPD'), '_YLDs'),
                      paste0(c(names_causes), '_Deaths')) %>%
        unique
    }

    if(causes_set == 'GBD only') {
      # define short names
      names_causes <- c('IHD', 'Stroke', 'COPD', 'LC', 'LRI')

      causes_out <- c(paste0(c(names_causes, 'LRI.child'), '_YLLs'),
                      paste0(c('Stroke', 'Diabetes', 'COPD'), '_YLDs'),
                      paste0(c(names_causes, 'LRI.child', 'Diabetes'), '_Deaths')) %>%
        unique
    }

    if(!is.null(filter)) {
      causes_out <- causes_out %>% grep(filter, ., value = T)
    }

    return(causes_out)
  }


  if(gbd_causes[1] == 'default' & calc_causes[1] == CALC_GBD) gbd_causes <- 'all'

  if(grepl('GEMM|GBD', calc_causes[1])) calc_causes <- get_calc_causes_from_set(calc_causes[1])
  if(calc_causes[1] == 'none') calc_causes <- character(0)
  calc_causes_wo_outcome <- calc_causes %>% gsub('_.*', '', .) %>%
    unique()

  if(gbd_causes[1] == 'default'){
    gemm <- get_gemm()
    gbd_causes <- calc_causes_wo_outcome %>% subset(!(. %in% unique(gemm$cause)))
  }


  get_gbd_rr_old <- function(version="original", gbd_causes=c('LRI.child', 'Diabetes')){

    gbd_rr <- read_csv(get_hia_path(glue("rr_{version}.csv")), col_types = cols())
    if(length(gbd_causes) == 0) gbd_causes <- 'none'
    if(gbd_causes[1] != 'all'){
      print("Filtering")
      print(unique(gbd_rr$cause))
      gbd_rr <- gbd_rr %>% dplyr::filter(cause %in% gbd_causes)
    }

    return(gbd_rr)
  }

  gbd_rr <- get_gbd_rr_old(version=RR_ORIGINAL, gbd_causes=gbd_causes)

  gbd_causes <- gbd_rr$cause %>% unique %>%
    subset(. %in% calc_causes_wo_outcome)
  gemm_causes <- calc_causes_wo_outcome %>% subset(!(. %in% gbd_causes))


  return(list(gbd_causes = gbd_causes,
              gemm_causes = gemm_causes,
              calc_causes_wo_outcome = calc_causes_wo_outcome,
              calc_causes = calc_causes))
}
