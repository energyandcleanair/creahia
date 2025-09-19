generate_rr_gemm <- function(region="inc_China"){

  gemm <- get_gemm() %>%
    filter(region==!!region)

  rr <- lapply(seq(0, 300, 0.1), function(z){
    generate_rr_gemm_at_z(gemm, z)
  }) %>%
    bind_rows()


  # Filter causes to match those of original IER for now
  rr <- rr %>%
    mutate(cause=recode_gbd_causes(cause, stop_on_unknown = FALSE)) %>%
    mutate(age=recode_age(age)) %>%
    filter(!is.na(age))

  # Convert LRI to LRIChild when Under 5
  # rr <- rr %>%
  #   mutate(cause=case_when(cause == "LRI" &
  #                                  age == "0-4" ~ CAUSE_LRICHILD,
  #                                TRUE ~ cause))

  rr <- rr %>%
    add_lri_child()

  # Format
  rr <- rr %>%
    select(cause,
           exposure,
           age,
           central,
           low,
           high)

  rr
}


generate_rr_gemm_at_z <- function(gemm, z){
  gemm %>%
    spread(param, value) %>%
    mutate(
      z = z,
      z_corr = pmax(0, z-2.4),
      g = log(1 + z_corr / a) / (1 + exp((u-z_corr) / p)),
      low = g * (t - 2 * se),
      central = g * t,
      high = g * (t + 2 * se)
    ) %>%
    mutate_at(vars(low, central, high), exp) %>%
    select(exposure=z, age, cause, low, central, high)
}


get_gemm <- function() {
  print("Getting GEMM")

  # read GEMM function fit parameters
  infile <- get_hia_path('GEMM Calculator (PNAS)_ab.xlsx')
  gemm.china <- suppressMessages(read_xlsx(infile, sheet = 'GEMM fit parameters',
                                           skip = 8, n_max = 14))
  gemm.exchina <- suppressMessages(read_xlsx(infile, sheet = 'GEMM fit parameters',
                                             skip = 29, n_max = 14))

  gemm <- bind_rows(gemm.china %>% mutate(region = 'inc_China'),
                    gemm.exchina %>% mutate(region = 'ex_China'))

  # eliminate empty rows and columns
  gemm <- gemm[rowSums(!is.na(gemm)) > 1, colSums(!is.na(gemm)) > 0]
  # write.csv(gemm, 'gemm fit parameters.csv') # CHECK necessary?

  # read names of causes of death
  causes <- suppressMessages(read_xlsx(infile, sheet = 'GEMM fit parameters',
                                       skip = 6, n_max = 1, col_names = F)) %>%
    unlist %>%
    subset(!is.na(.))


  # define short names
  names(causes) <- c('NCD.LRI', 'IHD', 'Stroke', 'COPD', 'LC', 'LRI')

  # remove duplicated age columns
  names(gemm)[1] <- 'age'
  gemm <- gemm %>% sel(-contains('Age'), age)

  # give parameter columns names; t = theta, se = standard error of theta,
  # a = alpha, u = mu, p = pi
  newnames <- names(causes) %>% sapply(paste, c('t', 'se', 'a', 'u', 'p'), sep = '_') %>%
    as.vector()
  names(gemm)[seq_along(newnames)] <- newnames

  gemm <- gemm %>% gather(cause, value, -region, -age) %>%
    tidyr::separate(cause, c('cause', 'param'), '_')
  gemm$age[gemm$age == '30-35'] <- '30-34'
  # setwd(origwd)

  return(gemm)
}
