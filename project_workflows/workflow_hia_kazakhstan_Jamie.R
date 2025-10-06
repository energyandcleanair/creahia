# import libraries ----
library(raster)
library(sf)
library(readxl)
library(zoo)
library(magrittr)
library(lubridate)
library(glue)

library(creahia)
library(creapuff)
require(rcrea)
require(creahelpers)


# setup variables ----

project_dir="H:/kazakhstan"
gis_dir <- "H:/gis"

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files



pollutants_to_process <- c('NO2', 'PM2.5', 'PM10', 'SO2') # e.g. c('NO2', 'PM2.5', ...)
scenarios_to_process = c('allstack') # make sure the file names match the scenarios

# economic valuation variables
targetyears <- 2023


# 01: Prepare CALMET results ----
# CALMET parameters
calmet_result <- readRDS(file.path(input_dir, "calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# list CALMET csvs and convert to TIFs
calpuff_files <- creapuff::get_calpuff_files(ext = ".csv", gasunit = 'ug',
                                             dir = input_dir, hg_scaling = 1e-3)
grids <- creapuff::get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res = 1)
grid_raster <- grids$gridR

calpuff_files %>%
  filter(scenario %in% scenarios_to_process,
         speciesName %in% pollutants_to_process,
         period == 'annual') %>%
  as.data.frame() %>%
  make_tifs(grids = grids, overwrite = T)


# 02: Get base concentration levels ----
conc_base <- creahia::wrappers.get_conc_baseline(species = unique(calpuff_files$species),
                                                 grid_raster = grid_raster,
                                                 no2_targetyear = 2020,
                                                 pm25_to_pm10_ratio = .7)


# 03: Create support maps (e.g. countries, provinces, cities) ----
shp <- creahelpers::get_adm(level = 2, res = 'low')
regions <- creahia::get_adm(grid_raster, shp = shp, admin_level = 2)

calpuff_files_all <- creapuff::get_calpuff_files(ext = ".tif", gasunit = 'ug',
                                                 dir = input_dir, hg_scaling = 1e-3)

causes_to_include <- creahia::get_calc_causes(filter = 'Death|YLD')


# 04: HIA calculations ----
creahia::wrappers.compute_hia_two_images.character(scenarios = scenarios_to_process,
                                         perturbation_rasters_table = calpuff_files_all,
                                         baseline_rasters_table = conc_base,
                                         grid_raster = grid_raster,
                                         regions = regions,
                                         scale_base_year = 2019, # Population base year : reference year of INPUT data, for total epidemiological and total population
                                         scale_target_year = 2022, # Population target year
                                         crfs_version = "C40",
                                         epi_version = "C40",
                                         return_concentrations = T,
                                         gbd_causes = 'default',
                                         calc_causes = causes_to_include)


#read HIA data
runs <- calpuff_files_all$scenario %>% unique
hia <- runs %>% lapply(function(scen) readRDS(file.path(project_dir, 'HIA', paste0('hia_GBD__',scen,'.RDS')))$hia) %>% bind_rows


#select admin regions to include
#adm <- creahelpers::get_adm(level = 2, res='coarse')
#adm <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_coarse.RDS'))


hia_totals <- hia %>%
  left_join(shp@data %>% dplyr::select(region_id = GID_2, starts_with('NAME'))) %>%
  group_by(across(c(starts_with('NAME'), outcome, pollutant, cause, age_group, iso3,
                    scenario, estimate, double_counted))) %>%
  summarise(across(number, sum)) %>%
  filter(pollutant != 'PM25' | cause != 'AllCause') %>%
  mutate(number = number * case_when(pollutant != 'NO2' | cause != 'AllCause' ~ 1,
                                     estimate == 'central' ~ 1/2,
                                     estimate == 'low' ~ 1/2,
                                     estimate == 'high' ~ 2/3))




#hia_totals  %>% group_by(scenario, pollutant) %>% filter(!double_counted, outcome=='Deaths', estimate=='central') %>%
#  #summarise(across(number, sum, na.rm=T))
#  summarise(across(number, ~sum(.x, na.rm=T), na.rm=T))


# 06: Compute and extract economic costs --------------------------------------------------------
# TODO : change name scale_target_year -> pop_target_year

targetyears = c(seq(1996,2022,1))

hia_cost <- get_hia_cost(hia=hia, valuation_version="viscusi")


#usd_to_lcu=15447
usd_to_lcu=461.15

hia_cost %>%
  distinct(outcome, valuation_world_2017, valuation_current_usd, iso3, reference) %>%
  na.omit %>%
  add_long_names() %>%
  dplyr::select(-outcome, outcome = outcome_long) %>%
  mutate(across(where(is.numeric), function(x) x %>% signif(4) %>% scales::comma(accuracy = 1))) %>%
  relocate(outcome) %>%
  relocate(reference, .after = everything()) %>%
  write_csv(file.path(output_dir, 'valuations.csv'))

hia_fut <- get_econ_forecast(hia_cost, forecast_years = targetyears, reference_year = 2019, use_gdp_scaling = TRUE)

hia_fut %>%
  left_join(hia_cost %>% distinct(outcome, cause, pollutant, double_counted)) %>%
  group_by(scenario, outcome) %>%
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T)))

years = list('allstack' = 1996:2022)

################################################################################################

# 1996_2022: Scale impacts based on steel production, normalised to year 2023
Steel_Production = read_xlsx(file.path(output_dir, 'Temirtau_Proxies_for_AqQ.xlsx'), sheet='Clean')
hia_fut <- merge(hia_fut, Steel_Production, by = "year", all.x = TRUE)
hia_fut$number <- hia_fut$normalised * hia_fut$number
hia_fut$cost_mn_currentUSD <- hia_fut$normalised * hia_fut$cost_mn_currentUSD
hia_fut$cost_mn_currentLCU <- hia_fut$normalised * hia_fut$cost_mn_currentLCU

# 2023: copy 2022, re-scale
hia_fut_2023 <- hia_fut %>% filter(year == 2022)
steel_SF     <- Steel_Production %>% filter(year == 2022)
steel_SF      <- steel_SF$normalised

# divided by X,
GDP_SF = 1.011

hia_fut_2023$number             <- hia_fut_2023$number / steel_SF
hia_fut_2023$cost_mn_currentUSD <- hia_fut_2023$cost_mn_currentUSD * GDP_SF / steel_SF
hia_fut_2023$cost_mn_currentLCU <- hia_fut_2023$cost_mn_currentLCU * GDP_SF / steel_SF
hia_fut_2023 <- hia_fut_2023 %>%
  mutate(year = ifelse(year == '2022', '2023', year))


hia_fut <- rbind(hia_fut, hia_fut_2023)



# cumulative  integrated over time and space
for(x in names(years)){
  hia_fut %>% filter(scenario == x) %>%
    filter(year %in% years[[x]], pollutant != 'NO2' | cause != 'AllCause') %>%
    group_by(scenario, estimate, outcome, cause, pollutant) %>%
    summarise(across(c(number, cost_mn_currentUSD),  ~sum(.x, na.rm=T))) %>%
    left_join(hia %>% distinct(scenario, outcome, cause, pollutant, double_counted)) %>%
    mutate(double_counted = ifelse(pollutant=='NO2', F, double_counted)) %>%
    add_long_names %>% ungroup %>% select(-outcome, -cause) %>% rename(cause=cause_long, outcome=outcome_long) %>%
    pivot_longer(c(number, cost_mn_currentUSD)) %>%
    spread(estimate, value) %>%
    arrange(desc(name), scenario, outcome!='deaths', double_counted) %>%
    select(scenario, outcome, cause, pollutant,  central, low, high, variable=name, double_counted) %>%
    write_csv(file.path(output_dir, glue::glue('{x}_Cumulative.csv')))
}



# cumulative  integrated over time and space
for(x in names(years)){
  hia_fut %>% filter(scenario == x) %>%
    filter(year %in% years[[x]], pollutant != 'NO2' | cause != 'AllCause') %>%
    group_by(scenario, estimate, outcome, cause, pollutant, year) %>%
    summarise(across(c(number, cost_mn_currentUSD),  ~sum(.x, na.rm=T))) %>%
    left_join(hia %>% distinct(scenario, outcome, cause, pollutant, double_counted)) %>%
    mutate(double_counted = ifelse(pollutant=='NO2', F, double_counted)) %>%
    add_long_names %>% ungroup %>% select(-outcome, -cause) %>% rename(cause=cause_long, outcome=outcome_long) %>%
    pivot_longer(c(number, cost_mn_currentUSD)) %>%
    spread(estimate, value) %>%
    arrange(desc(name), scenario, outcome!='deaths', double_counted) %>%
    select(scenario, outcome, cause, pollutant,  central, low, high, variable=name, double_counted, year) %>%
    write_csv(file.path(output_dir, glue::glue('{x}_Yearly.csv')))
}

##########################################

yearly = read_csv(file.path(output_dir, 'allstack_Yearly.csv'))

Deaths = yearly %>%
  filter(outcome == 'deaths' & variable == 'number' & double_counted == 'FALSE') %>%
  group_by(year) %>%
  summarise(across(c(central),  ~sum(.x, na.rm=T))) %>%
  rename("Deaths" = "central")

Cost = yearly %>%
  filter(variable == 'cost_mn_currentUSD' & double_counted == 'FALSE') %>%
  group_by(year) %>%
  summarise(across(c(central),  ~sum(.x, na.rm=T))) %>%
  rename("Costs" = "central")

yearly_deaths_costs <- merge(Deaths,Cost,by="year")

yearly_deaths_costs$costs_per_death <- yearly_deaths_costs$Costs  / yearly_deaths_costs$Deaths

Cumulative_deaths <- sum(yearly_deaths_costs$Deaths)
Cumulative_costs <- sum(yearly_deaths_costs$Costs)
Cumulative_costs_per_death = Cumulative_deaths / Cumulative_costs
print (Cumulative_deaths)
print (Cumulative_costs)
print (Cumulative_costs_per_death)


yearly_deaths_costs$year <- as.Date(paste0(yearly_deaths_costs$year, "-01-01"))



# Create a ggplot object with separate subplots for 'costs', 'deaths', and 'costs_per_death'

# Plot for 'deaths'
plot_deaths <- ggplot(yearly_deaths_costs, aes(x = year, y = Deaths)) +
  geom_line() +
  labs(title = "Annual deaths due to air pollution from Temirtau",
       x = "Years",
       y = "Deaths") +
  theme_minimal() +
  ylim(0, max(yearly_deaths_costs$Deaths, na.rm = TRUE)) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")
plot(plot_deaths)
ggsave("H:/kazakhstan/HIA/Deaths_Timeseries.png", plot_deaths, width = 8, height = 6)

# Plot for 'costs'
plot_costs <- ggplot(yearly_deaths_costs, aes(x = year, y = Costs)) +
  geom_line() +
  labs(title = "Annual health damages due to air pollution from Temirtau",
       x = "Years",
       y = "USD million") +
  theme_minimal() +
  ylim(0, max(yearly_deaths_costs$Costs, na.rm = TRUE)) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")
plot(plot_costs)
ggsave("H:/kazakhstan/HIA/Costs_Timeseries.png", plot_costs, width = 8, height = 6)

# Plot for 'costs'
plot_costs <- ggplot(yearly_deaths_costs, aes(x = year, y = costs_per_death)) +
  geom_line() +
  labs(title = "Annual health damages due to air pollution from Temirtau",
       x = "Years",
       y = "USD million / death") +
  theme_minimal() +
  ylim(0, max(yearly_deaths_costs$costs_per_death, na.rm = TRUE)) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")
plot(plot_costs)
ggsave("H:/kazakhstan/HIA/Costs_per_death_Timeseries.png", plot_costs, width = 8, height = 6)


















