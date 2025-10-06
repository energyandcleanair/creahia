output_dir <- "G:/IndonesiaIESR/HIA"

malut_plants <- read_csv(file.path(output_dir, 'malut_plants_to_icel.csv'))

fut <- read_csv(file.path(output_dir, 'all_plants_cumulative.csv'))
cur <- read_csv(file.path(output_dir, 'all_plants_results_2022.csv'))

cur %>% filter(unit_name %in% malut_plants$Plant) %>% write_csv(file.path(output_dir, 'malut_results_2022.csv'))
fut %>% filter(unit_name %in% malut_plants$Plant) %>% write_csv(file.path(output_dir, 'malut_results_cumulative.csv'))

fut %>% filter(unit_name %in% malut_plants$Plant, !double_counted) %>%
  mutate(across(starts_with('number'), ~ifelse(grepl('Death', outcome), .x, 0))) %>%
  group_by(scenario) %>%
  summarise(across(matches('number|cost_mn_currentUSD'), ~sum(.x, na.rm=T)))

