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
    # dplyr::filter(age == .age, cause == .cause, region == .region) %>%
    spread(param, value) %>%
    mutate(
      z = z,
      z_corr = pmax(0, z-2.4, na.rm = T),
      g = log(1 + z_corr / a) / (1 + exp((u-z_corr) / p)),
      low = g - 2 * se,
      central = g,
      high = g + 2 * se
    ) %>%
    mutate_at(vars(low, central, high), exp) %>%
    select(exposure=z, age, cause, low, central, high)
}
