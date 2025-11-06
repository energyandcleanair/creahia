gather_epi <- function(df) {
  df %>%
    dplyr::rename(central = val, low = lower, high = upper) %>%
    tidyr::gather(estimate, val, central, low, high) %>%
    dplyr::mutate(measure_name = measure_name %>% gsub(' .*', '', .))
}


ihme_getrate <- function(df, pop) {
  df %>%
    left_join(pop %>% sel(location_id, pop = val), by = "location_id") %>%
    mutate(val = val / pop * 1e5) %>% sel(-pop) %>%
    ungroup %>%
    distinct
}
