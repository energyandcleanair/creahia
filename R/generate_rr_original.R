generate_rr_original <- function(){

  # Sent to Lauri by email
  read_csv(get_hia_path('rr/raw/ier/ier_computed_table.csv'), col_types = cols()) %>%
    # there's a weird one where rr_upper < rr_mean
    mutate(
      low = rr_lower,
      central = pmin(rr_mean, rr_upper),
      high = pmax(rr_mean, rr_upper)
    ) %>%
    select(-c(rr_lower, rr_mean, rr_upper)) %>%
    mutate(cause = recode_rr_causes(cause)) %>%
    group_by(cause) %>%
    group_modify(function(df, key) {
      # if the cause is LRI, we need to add the child risk function
     if(all(df$age == 99))
       df$age <- AGE_ADULTS

     df$age <- as.character(df$age)
     return(df)
    }) %>%
    mutate(age = recode_age(age)) %>%
    dplyr::filter(exposure <= 300, !is.na(age)) %>%
    select(cause, age, exposure, low, central, high) %>%
    # add the LRI risk function to be used for children
    add_lri_child()
}
