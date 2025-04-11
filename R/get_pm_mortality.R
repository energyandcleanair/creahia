#' A function to estimate PM mortality by multiplying Population Attributable Fractions (PAFs) by epidemiological data
#' and estimating associated confidence intervals.
#'
#' @param paf_scenario
#' @param epi_loc
#'
#' @return
#' @export
#'
#' @examples
get_pm_mortality <- function(paf_scenario, epi_loc){

  z_epi <- qnorm(1 - (1 - 0.95) / 2) # 95% confidence interval: 1.96
  z_paf <- qnorm(1 - (1 - 0.95) / 2) # 95% confidence interval
  z_mort <- qnorm(1 - (1 - 0.95) / 2) # 95% confidence interval

  available_causes <- intersect(unique(paf_scenario$var), names(epi_loc))

  epi_long <- epi_loc %>%
    filter(estimate %in% c("low", "central", "high")) %>%
    select(iso3, region_id, estimate, pop, all_of(available_causes)) %>%
    pivot_longer(
      cols = all_of(available_causes),
      names_to = "var",
      values_to = "metric_value"
    ) %>%
    pivot_wider(
      names_from = estimate,
      values_from = metric_value,
      names_prefix = "E_"
    )

  # Merge paf_scenario with epi_long on var and region_id = iso3
  merged_df <- paf_scenario %>%
    rename_at(c("low", "central", "high"), ~ paste0("P_", .)) %>%
    inner_join(epi_long, by = c("var", "region_id"))

  # Check for missing values after merge
  if(any(is.na(merged_df$E_central))) {
    warning("Some PAF entries could not be matched with GBD data. Check 'var' and epidemiological metric names.")
  }

  pm_mort <- merged_df %>%

    # Calculate variances from confidence intervals
    # using Delta method
    mutate(

      # Standard Error for PAF and GBD metrics

      # In PAF, we have included a saturation at 0, meaning the confidence interval might be artificially truncated
      # We ensure we are using the largest side of the confidence interval when computing standard deviation.
      SE_PAF = pmax(P_high - P_central, P_central - P_low) / (z_paf),

      # In GBD metrics, we do not have a saturation at 0, so we can use the standard formula
      SE_E = (E_high - E_low) / (2 * z_epi),

      # Variance of PAF and GBD metrics
      Var_PAF = SE_PAF^2,
      Var_E = SE_E^2,


      # Variance of Attributable Burden using Delta Method
      Var_A = (E_central^2) * Var_PAF + (P_central^2) * Var_E,

      # Standard Error
      SE_A = sqrt(Var_A),

      # Confidence Intervals
      # Attributable Burden
      central = P_central * E_central,
      low = P_low * E_low,
      high = P_high * E_high
      #low = central - z_mort * SE_A,
      #high = central + z_mort * SE_A

    )  %>%
    mutate(var = paste0(var, '_PM25')) %>%
    select(region_id, pop, var, central, low, high) %>%

    # In certain cases, where the perturbation is small, the mortality estimates can be of the opposite sign
    # which is not possible. In such cases, we set the mortality that is the opposite sign of central estimate to 0.
    mutate(
      low = ifelse(low * central < 0, 0, low),
      high = ifelse(high * central < 0, 0, high)
    ) %>%

    # Pivot
    pivot_longer(
      cols = c(central, low, high),
      names_to = "estimate",
      values_to = "value"
    ) %>%

    # Scale
    mutate(value = value / 1e5 * pop) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    as.data.frame()



  return(pm_mort)
}
