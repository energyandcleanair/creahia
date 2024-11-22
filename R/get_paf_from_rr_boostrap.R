#' Calculate Population Attributable Fraction (PAF) Using Bootstrapping
#'
#' This function computes the Population Attributable Fraction (PAF) for each pixel by
#' propagating uncertainties from baseline and permuted Risk Ratios (RR) using a
#' bootstrap method. It accounts for multiple age groups with associated weights and
#' population sizes.
#'
#' @param rr_base A 3D numeric array of baseline Risk Ratios with dimensions \[pixel, estimate, agegroup\].
#'   The `estimate` dimension should include `"low"`, `"central"`, and `"high"` values representing the confidence interval bounds.
#' @param rr_perm A 3D numeric array of permuted Risk Ratios with the same dimensions and structure as `rr_base`.
#' @param age_weights A numeric vector of length equal to the number of age groups, representing the weights for each age group.
#' @param pop A numeric vector of population sizes for each pixel. Length should match the number of pixels in `rr_base` and `rr_perm`.
#' @param n_boot An integer specifying the number of bootstrap iterations to perform. Default is `1000`.
#' @param ci_level A numeric value between 0 and 1 indicating the confidence level for the PAF estimates. Default is `0.95` for a 90% confidence interval.
#' @param seed An integer value to set the random seed for reproducibility of bootstrap results. Default is `123`.
#'
#' @return A named numeric vector containing the population-weighted PAF estimates:
#'   \item{low}{Lower bound of the confidence interval for PAF.}
#'   \item{central}{Median estimate of PAF.}
#'   \item{high}{Upper bound of the confidence interval for PAF.}
#'
#' @details
#' The function assumes that Risk Ratios (RR) are log-normally distributed. It transforms the RR estimates to the log scale,
#' calculates standard errors based on the provided confidence intervals, and performs bootstrapping to sample RR values.
#' The PAF is then calculated for each bootstrap iteration, aggregated across age groups using the specified weights, and
#' the confidence intervals are derived from the bootstrap distribution.
get_paf_from_rr_boostrap <- function(rr_base, rr_perm, age_weights, pop, n_boot = 1000, ci_level = 0.95, seed = 123){

  # Set seed for reproducibility
  set.seed(seed)

  # Extract dimensions
  n_pixels <- dim(rr_base)[1]
  n_agegroups <- dim(rr_base)[3]


  # Extract log RR and compute standard errors
  # Initialize matrices
  log_rr_base_central <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_base_low <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_base_high <- matrix(0, nrow = n_pixels, ncol = n_agegroups)

  log_rr_perm_central <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_perm_low <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_perm_high <- matrix(0, nrow = n_pixels, ncol = n_agegroups)

  # Fill the matrices
  #TODO vectorize this
  for (ag in 1:n_agegroups) {
    log_rr_base_central[, ag] <- log(rr_base[, "central", ag])
    log_rr_base_low[, ag] <- log(rr_base[, "low", ag])
    log_rr_base_high[, ag] <- log(rr_base[, "high", ag])

    log_rr_perm_central[, ag] <- log(rr_perm[, "central", ag])
    log_rr_perm_low[, ag] <- log(rr_perm[, "low", ag])
    log_rr_perm_high[, ag] <- log(rr_perm[, "high", ag])
  }

  # Compute standard errors based on CI
  # Assuming symmetric CI on log scale: log(RR_central) ± z * SE = log(RR_low), log(RR_high)
  z_score <- qnorm(1 - (1 - ci_level) / 2) # For two-tailed CI (e.g., 1.645 for 90% CI)
  SE_base <- (log_rr_base_high - log_rr_base_low) / (2 * z_score)
  SE_perm <- (log_rr_perm_high - log_rr_perm_low) / (2 * z_score)

  # Normalize age_weights to sum to 1
  age_weights_norm <- age_weights / sum(age_weights)

  library(foreach)
  library(doParallel)

  num_cores <- detectCores() - 1

  # Register the parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  paf_boot <- foreach(b = 1:n_boot, .combine = cbind, .packages = 'matrixStats') %dopar% {
    # Sample RR_base and RR_perm for all pixels and agegroups
    rr_base_sample_log <- log_rr_base_central + matrix(rnorm(n_pixels * n_agegroups, mean = 0, sd = 1) * SE_base,
                                                       nrow = n_pixels, ncol = n_agegroups)
    rr_perm_sample_log <- log_rr_perm_central + matrix(rnorm(n_pixels * n_agegroups, mean = 0, sd = 1) * SE_perm,
                                                       nrow = n_pixels, ncol = n_agegroups)

    # Exponentiate to get RR samples
    rr_base_sample <- exp(rr_base_sample_log)
    rr_perm_sample <- exp(rr_perm_sample_log)

    # Compute PAF: (RR_perm / RR_base) - 1
    paf_sample <- (rr_perm_sample / rr_base_sample) - 1
    paf_sample[!is.finite(paf_sample)] <- NA

    # Apply age weights and aggregate PAF across agegroups
    paf_weighted <- sweep(paf_sample, 2, age_weights_norm, FUN = "*")
    paf_pixel <- rowSums(paf_weighted, na.rm = TRUE)

    return(paf_pixel)
  }

  stopCluster(cl)


  # Reshape back to [n_pixels, n_boot]
  # paf_boot <- matrix(paf_pixel_boot, nrow = n_pixels, ncol = n_boot, byrow = TRUE)

  # Compute confidence intervals
  lower_prob <- (1 - ci_level) / 2
  upper_prob <- 1 - lower_prob


  paf_low_central_high <- matrixStats::rowQuantiles(paf_boot, probs = c(lower_prob, 0.5, upper_prob), na.rm = TRUE)

  paf_low_central_high %>%
    # Weight average by population
    apply(2, function(x) weighted.mean(x, w = pop, na.rm = TRUE)) %>%
    `names<-`(c("low", "central", "high"))
}
