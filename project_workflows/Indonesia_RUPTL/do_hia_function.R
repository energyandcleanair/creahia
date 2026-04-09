do_hia <- function(emis, hia_per_t_file=NULL, project_dir="G:/IndonesiaIESR") {
  if(is.null(hia_per_t_file)) {
    output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
    hia_per_t_file <- file.path(output_dir, 'hia_per_t.RDS')
  }

  hia_per_t <- readRDS(hia_per_t_file)

  emis %>% ungroup %>%
    rename(emitted_species=pollutant) %>%
    group_by(cluster, year) %>%
    group_modify(function(df, group) {
      message(group)
      df %>%
        inner_join(hia_per_t %>% filter(cluster==group$cluster, year==group$year)) %>%
        group_by(CFPP.name, MW, utilization, scenario, Outcome, Cause, Pollutant, double_counted, estimate, unit) %>%
        mutate(across(c(number, cost_mn_currentUSD), ~.x*emissions_t)) %>%
        summarise(across(c(number, cost_mn_currentUSD), sum))
    }) %>%
    mutate(across(c(number_per_TWh=number, cost_mn_currentUSD_per_TWh=cost_mn_currentUSD), ~.x/(MW*8760*utilization)*1e6))
}
