#' A function to estimate PM mortality by multiplying Population Attributable Fractions (PAFs) by epidemiological data
#' and estimating associated confidence intervals.
#'
#' @param paf_scenario
#' @param epi_loc
#' @param calc_causes
#'
#' @return
#' @export
#'
#' @examples
get_pm_mortality <- function(paf_scenario, epi_loc, calc_causes){

  paf_wide <- paf_scenario %>%
    gather(estimate, val, low, central, high) %>%
    mutate(var = paste0('paf_', var)) %>%
    spread(var, val)

  paf_wide <- epi_loc %>% left_join(paf_wide)

  pm_mort <- paf_wide %>% sel(region_id, estimate)

  available_causes <- intersect(unique(paf[[scenario]]$var), names(epi_loc))

  for(cs in intersect(available_causes, calc_causes)) {
    pm_mort[[cs]] <- paf_wide[[cs]] / 1e5 * paf_wide[[paste0('paf_', cs)]] * paf_wide$pop
  }

  names(pm_mort)[sapply(pm_mort, is.numeric)] <- names(pm_mort)[sapply(pm_mort, is.numeric)] %>%
    paste0('_PM25')

  z_epi <- qnorm(1 - (1 - 0.95) / 2) # 95% confidence interval: 1.96
  z_paf <- qnorm(1 - (1 - 0.95) / 2) # 95% confidence interval
  z_mort <- qnorm(1 - (1 - 0.95) / 2) # 95% confidence interval


  # GPT
  available_causes <- intersect(unique(paf[[scenario]]$var), names(epi_loc))


  epi_long <- epi_loc %>%
    filter(estimate %in% c("low", "central", "high")) %>%
    select(iso3, estimate, pop, all_of(available_causes)) %>%
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
    inner_join(epi_long, by = c("var", "region_id" = "iso3"))

  # Check for missing values after merge
  if(any(is.na(merged_df$E_central))) {
    warning("Some PAF entries could not be matched with GBD data. Check 'var' and epidemiological metric names.")
  }

  pm_mort <- merged_df %>%

    # Calculate variances from confidence intervals
    # using Delta method
    mutate(
      # Standard Error for PAF and GBD metrics
      SE_PAF = (high - low) / (2 * z_paf),
      SE_E = (E_high - E_low) / (2 * z_epi),

      # Variance of PAF and GBD metrics
      Var_PAF = SE_PAF^2,
      Var_E = SE_E^2,

      # Attributable Burden
      Attributable_Burden = central * E_central,

      # Variance of Attributable Burden using Delta Method
      Var_A = (E_central^2) * Var_PAF + (central^2) * Var_E,

      # Standard Error
      SE_A = sqrt(Var_A),

      # Confidence Intervals
      CI_low = Attributable_Burden - z_mort * SE_A,
      CI_high = Attributable_Burden + z_mort * SE_A,

      old_low = low * E_low,
      old_high = high * E_high
    )  %>%
    mutate(var = paste0(var, '_PM25')) %>%
    select(region_id, pop, var, central=Attributable_Burden, low=CI_low, high=CI_high) %>%
    # central, low, high to a estimate column
    pivot_longer(
      cols = c(central, low, high),
      names_to = "estimate",
      values_to = "value"
    ) %>%
    # scale
    mutate(value = value / 1e5 * pop) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    as.data.frame()

  return(pm_mort)
}
