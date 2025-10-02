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
  creapuff::make_tifs(grids = grids, overwrite = T)


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
creahia::wrappers.compute_hia_two_images(scenarios = scenarios_to_process,
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

# read the HIA data
hia <- runs %>% lapply(function(scen) readRDS(file.path(project_dir, 'hia',
                                                        glue('hia_GBD__{scen}.RDS')))$hia) %>%
  bind_rows

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


# 05: Compute and extract economic cost ----
hia_cost <- creahia::get_hia_cost(hia = hia_totals, valuation_version = "viscusi",
                                  current_year = 2019)

# export valuations.csv to output_dir
hia_cost %>%
  distinct(outcome, valuation_world_2017, valuation_current_usd, iso3, reference) %>%
  na.omit %>%
  add_long_names() %>%
  dplyr::select(-outcome, outcome = outcome_long) %>%
  mutate(across(where(is.numeric), function(x) x %>% signif(4) %>% scales::comma(accuracy = 1))) %>%
  relocate(outcome) %>%
  relocate(reference, .after = everything()) %>%
  write_csv(file.path(output_dir, 'valuations.csv'))

# calculate future cost
hia_fut <- hia_cost %>% creahia::get_econ_forecast(forecast_years = targetyears, reference_year = 2022)

# calculate HIA totals and its summary; export hia_results.csv to output_dir
hia_totals <- hia_fut %>% add_long_names() %>%
  group_by(outcome = outcome_long, cause = Cause_long, pollutant,
           double_counted, scenario, estimate) %>%
  mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>%
  rename(cost_bn_currentLCU = cost_mn_currentLCU) %>%
  summarise(across(c(number, starts_with('cost')), sum, na.rm = T))

hia_totals %>% filter(!double_counted) %>%
  group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
  pivot_longer(where(is.numeric), names_to = 'outcome', values_to = 'number') %>%
  bind_rows(hia_totals %>% filter(!double_counted)) %>%
  dplyr::select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
  write_csv(file.path(output_dir, 'hia_results.csv'))


# Plotting code below ----








