



orderrows <- function(df) {
  df %>% apply(1, sort) %>% t -> rr.out
  colnames(rr.out) <- c('low', 'central', 'high')
  rr.out
}

