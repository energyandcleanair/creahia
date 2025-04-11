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
#' @export
get_paf_from_rr_lauri <- function(rr_base, rr_perm, age_weights, pop, ci_level = 0.95, seed = 123,
                                  cause=NA, measure=NA){

  # rr_base and rr_perm are 3D arrays: [pixel, estimate, agegroup]
  # estimates are "low", "central", "high"
  # age_weights is a vector of length n_agegroups
  # pop is a vector of length n_pixels
  # ci_level: desired confidence level (e.g., 0.95 for 90% CI)

  age_weights <- unlist(unname(age_weights))

  # Normalize age_weights to sum to 1
  age_weights_norm <- age_weights / sum(age_weights)

  paf <- rr_perm / rr_base - 1


  if(length(dim(paf))==3) {
    paf_low <- paf[,'low',]
    paf_central <- paf[,'central',]
    paf_high <- paf[,'high',]

    rr_low <- rr_base[,'low',]
    rr_central <- rr_base[,'central',]
    rr_high <- rr_base[,'high',]
  } else {
    paf_low <- paf['low',]
    paf_central <- paf['central',]
    paf_high <- paf['high',]

    rr_low <- rr_base['low',]
    rr_central <- rr_base['central',]
    rr_high <- rr_base['high',]
  }


  # Weight by age group, then by population
  if(length(age_weights_norm)>1) {
    paf_low <- rowSums(paf_low %*% age_weights_norm, na.rm = TRUE)
    paf_central <- rowSums(paf_central %*% age_weights_norm, na.rm = TRUE)
    paf_high <- rowSums(paf_high %*% age_weights_norm, na.rm = TRUE)

    rr_low <- rowSums(rr_low %*% age_weights_norm, na.rm = TRUE)
    rr_central <- rowSums(rr_central %*% age_weights_norm, na.rm = TRUE)
    rr_high <- rowSums(rr_high %*% age_weights_norm, na.rm = TRUE)
  }

  if(all(pop==0)) pop[] <- 1

  paf <- cbind(low=paf_low, central=paf_central, high=paf_high) %>%
    sweep(1, pop, "*") %>%
    colSums(na.rm = TRUE) / sum(pop)

  # Sanity checks
  # If not all of the same sign or not ordered, stop
  if(
    # Not ordered (either way)
    (any(paf[order(paf)] != paf) & any(paf[order(-paf)] != paf))
    # Or different sign
    | n_distinct(setdiff(sign(paf),0)) > 1){
    warning("PAF estimates are not properly ordered or of the same sign for ", cause, " ", measure)
    logger::log_warn("PAF estimates are not properly ordered or of the same sign for ", cause, " ", measure)
  }

  return(paf)
}
