#' Calculate Population Attributable Fraction (PAF) Using the Delta Method
#'
#' This function computes the Population Attributable Fraction (PAF) for each pixel by
#' propagating uncertainties from baseline and permuted Risk Ratios (RR) using the Delta Method.
#' It accounts for multiple age groups with associated weights and population sizes.
#'
#' @param rr_base A 3D numeric array of baseline Risk Ratios with dimensions \[pixel, estimate, agegroup\].
#'   The `estimate` dimension should include `"low"`, `"central"`, and `"high"` values representing the confidence interval bounds.
#' @param rr_perm A 3D numeric array of permuted Risk Ratios with the same dimensions and structure as `rr_base`.
#' @param age_weights A numeric vector of length equal to the number of age groups, representing the weights for each age group.
#' @param pop A numeric vector of population sizes for each pixel. Length should match the number of pixels in `rr_base` and `rr_perm`.
#' @param ci_level A numeric value between 0 and 1 indicating the confidence level for the PAF estimates. Default is `0.95` for a 90% confidence interval.
#' @param seed An integer value to set the random seed for reproducibility of results. Default is `123`.
#'
#' @return A named numeric vector containing the population-weighted PAF estimates:
#'   \item{low}{Lower bound of the confidence interval for PAF.}
#'   \item{central}{Median estimate of PAF.}
#'   \item{high}{Upper bound of the confidence interval for PAF.}
#'
#' @details
#' The function uses the Delta Method to approximate the variance of the PAF based on the variances
#' of the log-transformed Risk Ratios (RR). It assumes that the log(RR) are normally distributed.
#'
#' See doc/paf_uncertainty.md for more details
#'
#'
#'
#' LAURI 2025-01-13:  The delta methods needs to be fixed before deploying -Lauri
#  issues:
#Rows 105-106 (new rows 119-120) apply the standard error of the absolute RRs to the difference between the perm and base RRs
#I guess this has to do with the comment "Assuming independence between RR_base and RR_perm". This assumption is definitely not valid -- in most cases they are very closely correlated.
#This leads to extremely wide confidence intervals -- in the case that I'm looking at, RR_base - RR_perm is mostly in the order of 1e-7 or less, whereas qnorm(upper_prob) * SE_paf is to the tune of 0.1 - 0.3, so you're getting confidence intervals that are like 7+ orders of magnitude, not the about +/- 50% we would expect
#the signs are also wrong -- qnorm(upper_prob) * SE_paf is >0 so in this case when paf_central has both positive and negative values, the negative ones are adjusted in the wrong direction
#row 76 (new row 90) should be paf_central <- 1 - (exp(log_rr_base_central) / exp(log_rr_perm_central))
#'
#'
#' @export
get_paf_from_rr_delta <- function(rr_base, rr_perm, age_weights, pop, ci_level = 0.95, seed = 123){

  # rr_base and rr_perm are 3D arrays: [pixel, estimate, agegroup]
  # estimates are "low", "central", "high"
  # age_weights is a vector of length n_agegroups
  # pop is a vector of length n_pixels
  # ci_level: desired confidence level (e.g., 0.95 for 90% CI)

  age_weights <- unlist(unname(age_weights))

  # Extract dimensions
  n_pixels <- dim(rr_base)[1]
  n_agegroups <- dim(rr_base)[3]

  # Extract log RR and compute standard errors
  log_rr_base_central <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_base_low <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_base_high <- matrix(0, nrow = n_pixels, ncol = n_agegroups)

  log_rr_perm_central <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_perm_low <- matrix(0, nrow = n_pixels, ncol = n_agegroups)
  log_rr_perm_high <- matrix(0, nrow = n_pixels, ncol = n_agegroups)

  # Fill the matrices
  #TODO vectorize all this
  for (ag in 1:n_agegroups) {
    log_rr_base_central[, ag] <- log(rr_base[, "central", ag])
    log_rr_base_low[, ag] <- log(rr_base[, "low", ag])
    log_rr_base_high[, ag] <- log(rr_base[, "high", ag])

    log_rr_perm_central[, ag] <- log(rr_perm[, "central", ag])
    log_rr_perm_low[, ag] <- log(rr_perm[, "low", ag])
    log_rr_perm_high[, ag] <- log(rr_perm[, "high", ag])
  }

  # Compute standard errors based on CI
  z_score <- qnorm(1 - (1 - ci_level) / 2) # e.g., 1.645 for 90% CI
  SE_base <- (log_rr_base_high - log_rr_base_low) / (2 * z_score)
  SE_perm <- (log_rr_perm_high - log_rr_perm_low) / (2 * z_score)

  # Normalize age_weights to sum to 1
  age_weights_norm <- age_weights / sum(age_weights)

  # Calculate PAF using the Delta Method
  # PAF = (RR_perm / RR_base) - 1
  # log(RR_perm / RR_base) = log(RR_perm) - log(RR_base)
  # Approximate variance using Delta Method

  # Compute the central estimate of PAF
  paf_central <- (exp(log_rr_perm_central) / exp(log_rr_base_central)) - 1

  # Compute the variance of log(RR_perm) and log(RR_base)
  var_log_rr_base <- SE_base^2
  var_log_rr_perm <- SE_perm^2

  # Assuming independence between RR_base and RR_perm
  X <- log_rr_perm_central
  var_X <- var_log_rr_perm

  Y <- log_rr_base_central
  var_Y <- var_log_rr_base

  # Compute the variance of PAF
  # 1-st order
  var_paf_1 <- exp(2 * (X - Y)) * (var_X + var_Y)

  # 2-nd order
  var_paf_2 <- exp(2 * (X - Y)) * (exp(var_X + var_Y) - 1 - var_X - var_Y)

  var_paf <- var_paf_1 + var_paf_2

  # Compute standard error of PAF
  SE_paf <- sqrt(var_paf)

  # Calculate confidence intervals
  lower_prob <- (1 - ci_level) / 2
  upper_prob <- 1 - lower_prob

  paf_low <- paf_central - qnorm(upper_prob) * SE_paf
  paf_high <- paf_central + qnorm(upper_prob) * SE_paf


  # In certain cases, where the perturbation is small, the PAF estimates can be of the opposite sign
  # which is not possible. In such cases, we set the PAF that is the opposite sign of central estimate to 0.
  # This could affect the confidence interval calculations at later stage. We'll need to ensure we're taking
  # the largest side of the confidence interval when computing standard deviation.
  paf_low[paf_low * paf_central < 0] <- 0
  paf_high[paf_high * paf_central < 0] <- 0


  # Weight by age group, then by population
  paf_low <- rowSums(paf_low %*% age_weights_norm, na.rm = TRUE)
  paf_central <- rowSums(paf_central %*% age_weights_norm, na.rm = TRUE)
  paf_high <- rowSums(paf_high %*% age_weights_norm, na.rm = TRUE)

  paf <- cbind(low=paf_low, central=paf_central, high=paf_high) %>%
    sweep(1, pop, "*") %>%
    colSums(na.rm = TRUE) / sum(pop)

  # Some sanity checks
  if(all(sort(paf) != paf)){
    stop("PAF estimates are not properly ordered.")
  }

  # If not all of the same sign, stop
  if(n_distinct(setdiff(sign(paf),0)) > 1){
    stop("PAF estimates are not all of the same sign.")
  }

  return(paf)
}
