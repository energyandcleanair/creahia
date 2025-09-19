investigate_rr_rhos <- function(){


  dir <- "~/Downloads/IHME_GBD_2019_PM_RISK_DRAWS"
  files <- list.files(dir)

  f <- "IHME_GBD_2019_PM_RISK_DRAWS_LBW_Y2021M01D06.csv"

  raw <- read_csv(file.path(dir, f))

  # Select first 1,001 columns
  raw <- raw %>% select(1:1001)

  # Make it a matrix, with first column being matrix rownames
  raw <- as.matrix(raw)
  rownames(raw) <- raw[,1]
  raw <- raw[,-1]

  # For each pair of rows, calculate the correlation
  n <- nrow(raw)
  rhos <- matrix(NA, nrow = n, ncol = n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      rhos[i,j] <- cor(raw[i,], raw[j,])
    }
  }
  rownames(rhos) <- rownames(raw)
  colnames(rhos) <- rownames(raw)

  #

  # Plot map of rhos using exposure levels contained in rownames
  library(ggplot2)
  library(tidyverse)
  df <- rhos %>%
    as_tibble() %>%
    mutate(x=as.numeric(rownames(rhos))) %>%
    pivot_longer(-x, names_to = "y", values_to = "value") %>%
    mutate(
      y = as.numeric(gsub("V", "", y))
    )


  hist <- df %>%
    filter(y > x,
           y < 100,
           ) %>%
    mutate(diff = y-x)

  hist %>% filter(diff==5) %>% pull(value) %>% hist()

  # Plot smooth with confidence interval
  plt <- ggplot(hist, aes(x=diff, y=value)) +
    geom_point(col='grey70') +
    geom_smooth(method="gam") +
    theme_classic() +
    labs(caption=f)

  plt

  # Interpolate
  library(akima)

  # Define grid resolution
  grid_points <- 100

  # Perform bilinear interpolation
  interp_data <- interp(
    x = df$x,
    y = df$y,
    z = replace_na(df$value, 0),
    xo = seq(min(df$x), 99, length.out = grid_points),
    yo = seq(min(df$y), 99, length.out = grid_points),
    linear = TRUE
  )

  # Convert to ggplot-friendly format
  interp_df <- expand.grid(x = interp_data$x, y = interp_data$y)
  interp_df$value <- as.vector(interp_data$z)
  # Replace 0 with NA
  interp_df$value[interp_df$value == 0] <- NA

  plt <- ggplot(interp_df %>% filter(x<y), aes(x, y, z=value)) +
    geom_raster(aes(fill = value)) +
    scale_fill_distiller(palette = "RdYlBu", limits=c(0,1)) +
    theme_classic() +
    labs(caption=f)

  ggsave("rho.png", plot=plt)




  }
