generate_rrs <- function(){

  causes_to_keep <- c(
    CAUSE_NCDLRI,
    CAUSE_LRI,
    CAUSE_COPD,
    CAUSE_LBW,
    CAUSE_IHD,
    CAUSE_STROKE,
    CAUSE_LUNGCANCER,
    CAUSE_DIABETES,
    CAUSE_LRICHILD,
    CAUSE_DEMENTIA
  )


  sources <- c(
    RR_ORIGINAL,
    RR_GBD2019,
    RR_FUSION,
    RR_GEMM,
    RR_GBD2021,
    RR_GBD2023
  )


  rrs <- lapply(sources, function(source){

    print(glue("Generating RR for {source}"))
    raw <- if(source == RR_ORIGINAL){
      generate_rr_original()
    } else if(source == RR_GBD2019){
      generate_rr_gbd2019()
    } else if(source == RR_GBD2021){
      generate_rr_gbd2021()
    } else if(source == RR_GBD2023){
      generate_rr_gbd2023()
    } else if(source == RR_FUSION){
      generate_rr_fusion()
    } else if(source == RR_GEMM){
      generate_rr_gemm()
    } else {
      stop('Unknown version')
    }

    raw %>%
      filter(cause %in% causes_to_keep) %>%
      check_rr() %>%
      mutate(source=source) %>%
      write_csv(file.path('inst','extdata', paste0('rr_', source, '.csv')))

  }) %>%
    bind_rows()


  # Visual check
  # ggplot(rrs %>%
  #          filter(exposure < 200) %>%
  #          filter(age=='25+') %>%
  #          pivot_longer(cols=c(low, central, high), names_to='rr_type', values_to='rr') %>%
  #          filter(rr_type=='central',
  #                 source %in% c("fusion", "gbd2021", "gbd2023")
  #                 ),
  #        aes(exposure, rr, col=source,
  #            # linetype = rr_type
  #            )) +
  #   geom_line() +
  #   facet_wrap(cause~age, scales='free_y')
  #
  #
  #
  # # Plot marginal rrs
  # rrs %>%
  #   filter(exposure < 200, source %in% c("fusion", "gemm")) %>%
  #   filter(age=='25+') %>%
  #   group_by(source, cause, age) %>%
  #   arrange(exposure) %>%
  #   mutate(res=1 - 1/central) %>%
  #   ggplot(aes(exposure, central, col=source)) + geom_line() +
  #   facet_wrap(~cause)

}




#' Check format and data
#'
#' @returns
#' @export
#'
#' @examples
check_rr <- function(rr){

  # No NA age
  stopifnot(all(!is.na(rr$age)))


  # Cause short is known
  unknown_causes <- rr$cause[!rr$cause %in% c(
    CAUSE_LRI,
    CAUSE_COPD,
    CAUSE_LBW,
    CAUSE_IHD,
    CAUSE_STROKE,
    CAUSE_LUNGCANCER,
    CAUSE_DIABETES,
    CAUSE_LRICHILD,
    CAUSE_NCDLRI,
    CAUSE_PTB,
    CAUSE_DEMENTIA
  )]
  if(length(unknown_causes) > 0){
    stop('Unknown causes: ', paste(unique(unknown_causes), collapse=', '))
  }

  # No NA cause
  stopifnot(all(!is.na(rr$cause)))

  # Colnames
  stopifnot(setequal(colnames(rr), c('cause', 'age', 'exposure', 'low', 'central', 'high')))

  # Exposure 0-300
  stopifnot(min(rr$exposure) == 0)
  stopifnot(max(rr$exposure) == 300)

  # Check ages are known
  stopifnot(all(rr$age %in% c(
    AGE_CHILDREN,
    AGE_ADULTS,
    AGE_ADULTS_SPLIT
  )))


  rr
}




recode_gbd_causes <- function(cause, stop_on_unknown = TRUE){
  result <- recode(tolower(cause),
         lri = CAUSE_LRI,
         lower_respiratory_infections = CAUSE_LRI,
         t2_dm = CAUSE_DIABETES,
         diabetes = CAUSE_DIABETES,
         cvd_ihd = CAUSE_IHD,
         ihd = CAUSE_IHD,
         ischemic_heart_disease = CAUSE_IHD,
         ischaemic_heart_disease = CAUSE_IHD,
         cvd_stroke = CAUSE_STROKE,
         stroke = CAUSE_STROKE,
         neo_lung = CAUSE_LUNGCANCER,
         lung_cancer = CAUSE_LUNGCANCER,
         lc = CAUSE_LUNGCANCER,
         `lung cancer` = CAUSE_LUNGCANCER,
         resp_copd = CAUSE_COPD,
         copd = CAUSE_COPD,
         lbw = CAUSE_LBW,
         ptb = CAUSE_PTB,
         ncd.lri = CAUSE_NCDLRI,
         `ncd&lri` = CAUSE_NCDLRI,
         dementia = CAUSE_DEMENTIA,
         cv = CAUSE_CV,
        .default = NA_character_
  )

  # Check for NA values and raise error if found
  if(stop_on_unknown && any(is.na(result))) {
    unknown_causes <- cause[is.na(result)]
    stop("Unknown cause(s) encountered: ", paste(unique(unknown_causes), collapse=", "))
  }

  return(result)
}


recode_age <- function(age){
  case_when(
    as.character(age) == '25+' ~ AGE_ADULTS,
    as.character(age) == '0-4' ~ AGE_CHILDREN,
    as.character(age) == '<5' ~ AGE_CHILDREN,
    as.character(age) == 'Under 5' ~ AGE_CHILDREN,
    as.character(age) == "9999" ~ AGE_ADULTS,
    as.character(age) %in% AGE_ADULTS_SPLIT ~ as.character(age),
    # Avoiding unnecessary conversion warnings
    suppressWarnings(as.numeric(age)) == 80 ~ '80+',
    suppressWarnings(as.numeric(age)) < 80 ~ paste0(age, '-', suppressWarnings(as.numeric(age)) + 4),
    TRUE ~ NA_character_)
}



add_lri_child <- function(rr){
  rr %>% filter(cause == CAUSE_LRI) %>%
    mutate(cause = CAUSE_LRICHILD,
           age = AGE_CHILDREN) %>%
    bind_rows(rr)
}

