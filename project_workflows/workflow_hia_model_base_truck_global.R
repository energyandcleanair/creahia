# import libraries ----
library(raster)
library(sf)
library(readxl)
library(zoo)
library(magrittr)
library(lubridate)
library(glue)
library(pbapply)
library(terra)
library(glue)
library(ncdf4)

library(creahia)
library(creapuff)
require(rcrea)
require(creahelpers)

# setup variables ---------------------------------------------------------

project_dir="H:/Truck_global"
gis_dir <- "H:/gis"
#gis_dir <- get_gis_dir() # CREA GIS files

input_dir <- file.path(project_dir, "sample input/sample model inputs") # TODO changet


output_dir <- file.path(project_dir, "outputs") # TODO change
dir.create(output_dir, showWarnings = F) # where to write all output files

pollutants_to_process <- c('O3_8h', 'NO2', 'PM25')
scenarios_to_process <- c('AF_OC',
                          'AS_OC_ME_AF_EAS',
                          'AS_OC_SA_ME_AF_EAS',
                          'AS_OC',
                          'AS',
                          'EU',
                          'ME_AF',
                          'NA_MX',
                          'NA',
                          'SA') # TODO change this

# project specific variables
bands_to_process <- list('PM25' = 'PM25', 
                         'O3_8h' = 'O3', 
                         'NO2' = 'NO2') # TODO change

# economic valuation variables
# targetyears <- c(2040, 2050) # TODO change with intended year
targeryears <- 2019

# 01: Prepare CALMET results ----------------------------------------------
# convert all required .nc to .tif

## convert baseline files ----
# preferably the baseline files are in a separate folder
baseinput <- file.path(input_dir,"base_unit_merge")
nc_baseline <- list.files(baseinput,full.names = T) # TODO change dir, make sure nc file is yearly



sapply(nc_baseline, function(file){

  output_folder <- file.path('outputs', 'tifs','baseline')
  dir.create(output_folder, showWarnings = F, recursive = T)
  
  pbsapply(bands_to_process, function(band){
    scenario <- 'baseline'
    poll <- names(which(bands_to_process == band))
    tryCatch(
      {
        raster <- rasterise_geoschem_nc(file, band, 
                                        lat_name = 'lat', lon_name = 'lon')
        raster %>% writeRaster(file.path(output_folder, glue('{scenario}_{poll}.tif')), 
                               overwrite = T)
      }, error = function(e){
        message(glue('Band {band} not found in {file}, continuing...'))
      }
    )
  })
})

# convert other files 
# TODO
sen_inputs <- file.path(input_dir,"diff")
nc_files <- list.files(sen_inputs, full.names = T)

sapply(nc_files, function(file){
  output_folder <- file.path('outputs', 'tifs','sens') # TODO preferably the base files are in seperate folder
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  pbsapply(bands_to_process, function(band){
    scenario <- sub(".*Truck_NOx_(.*)\\.nc$", "\\1", file) # TODO change depending on file name
    poll <- names(which(bands_to_process == band))
    tryCatch(
      {
        raster <- rasterise_geoschem_nc(file, band, 
                                        lat_name = 'lat', lon_name = 'lon')
        raster %>% writeRaster(file.path(output_folder, glue('{scenario}.{poll}.tif')), 
                               overwrite = T)
      }, error = function(e){
        message(glue('Band {band} not found in {file}, continuing...'))
      }
    )
  })
})

# TODO
perturbation_rasters <- tibble(path = list.files(file.path(output_dir, 'tifs','sens'), full.names = T)) %>% 
  mutate(filename = tools::file_path_sans_ext(basename(path)),
         scenario = filename %>% str_split_i('\\.', 1), # TODO make sure this match `scenarios_to_process`
         species = filename %>% str_split_i('\\.', 2) %>% tolower() %>% if_else(. == 'o3', 'o3_8h', .)) # TODO make sure this match conc_base
         


# grid raster for the domain
grid_raster <- terra::rast((list.files(file.path('outputs', 'tifs','baseline'), 
                                       full.names = T))[[1]])


# 02: Get base concentration levels ---------------------------------------
conc_base <- creahia::get_conc_baseline_manual(species = pollutants_to_process %>% tolower(),
                                               folder = file.path('outputs', 'tifs','baseline'))


# 03: Create support maps -------------------------------------------------
shp <- creahelpers::get_adm(level = 0, res = 'low',version = '36')
regions <- creahia::get_model_adm(grid_raster, shp = shp, admin_level = 0)

causes_to_include <- creahia::get_calc_causes(filter = 'Death|YLD')

#causes_to_include <- creahia::get_calc_causes(causes_set = "GEMM and GBD", filter = NULL)




# 04: HIA calculations ----------------------------------------------------
creahia::wrappers.compute_hia_two_images(scenarios = scenarios_to_process,
                                         perturbation_rasters_table = perturbation_rasters,
                                         baseline_rasters_table = conc_base,
                                         grid_raster = grid_raster,
                                         regions = regions,
#                                         scale_base_year = 2015, # Population base year : reference year of INPUT data, for total epidemiological and total population
#                                         scale_target_year = 2019, # Population target year
                                         pop_year = 2019,   # target year]
                                         crfs_version = "C40",
                                         epi_version = "C40",
                                         return_concentrations = T,
                                         gbd_causes = 'default',
                                         calc_causes = causes_to_include,
                                         output_folder = output_dir,
                                         custom_glue = paste0(glue("hia_GBD2019"),
                                                              "_{scen}.RDS"))

hia <- scenarios_to_process %>% lapply(function(scen){
  readRDS(file.path(output_dir, 'hia_old2', glue('hia_GBD2019_{scen}.RDS')))$hia # TODO change to fit result RDS
}) %>%
  bind_rows

hia_totals <- hia %>%
  left_join(shp@data %>% dplyr::select(region_id = GID_0, # TODO change to admin level e.g. GID_2
                                       starts_with('NAME'))) %>%
  group_by(across(c(starts_with('NAME'), Outcome, Pollutant, Cause, AgeGrp, iso3,
                    scenario, estimate, double_counted))) %>%
  summarise(across(number, sum)) %>%
#  filter(Pollutant != 'PM25' | Cause != 'AllCause') %>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause' ~ 1,
#  mutate(number = number * case_when(Pollutant != pollutants_to_process | Cause != 'AllCause' ~ 1,
                                       estimate == 'central' ~ 1/2,
                                     estimate == 'low' ~ 1/2,
                                     estimate == 'high' ~ 2/3))


# 05: Compute and extract economic cost ----
hia_cost <- creahia::get_hia_cost(hia = hia_totals, valuation_version = "viscusi",
                                  current_year = 2019) # GDP data currently 2017

# export valuations.csv to output_dir
hia_cost %>%
  distinct(Outcome, valuation_world_2017, valuation_current_usd, iso3, reference) %>%
  na.omit %>%
  add_long_names() %>%
  dplyr::select(-Outcome, Outcome = Outcome_long) %>%
  mutate(across(where(is.numeric), function(x) x %>% signif(4) %>% scales::comma(accuracy = 1))) %>%
  relocate(Outcome) %>%
  relocate(reference, .after = everything()) %>%
  write_csv(file.path(output_dir, 'valuations.csv'))

# calculate future cost
targeryears <- "2019"

hia_fut <- hia_cost %>% creahia::get_econ_forecast(years = targeryears, pop_targetyr = 2019)

# calculate HIA totals and its summary; export hia_results.csv to output_dir
hia_totals <- hia_fut %>% add_long_names() %>%
  group_by(Outcome = Outcome_long, Cause = Cause_long, Pollutant,
           double_counted, scenario, estimate) %>%
  mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>%
  rename(cost_bn_currentLCU = cost_mn_currentLCU) %>%
  summarise(across(c(number, starts_with('cost')), sum, na.rm = T))

hia_totals %>% filter(!double_counted) %>%
  group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
  pivot_longer(where(is.numeric), names_to = 'Outcome', values_to = 'number') %>%
  bind_rows(hia_totals %>% filter(!double_counted)) %>%
  dplyr::select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
  write_csv(file.path(output_dir, 'hia_results.csv'))


# Sum hia by scenarios

scenarios_to_process <- c('AF_OC',
                          'AS_OC_ME_AF_EAS',
                          'AS_OC_SA_ME_AF_EAS',
                          'AS_OC',
                          'AS',
                          'EU',
                          'ME_AF',
                          'NA_MX',
                          'NA',
                          'SA') # TODO change this


for (ccase in (scenarios_to_process)){
  # calculate HIA totals and its summary; export hia_results.csv to output_dir
  hia_totals <- hia_fut %>%
    filter(scenario == ccase) %>%
    add_long_names() %>%
    group_by(Outcome = Outcome_long, Cause = Cause_long, Pollutant, iso3,
             double_counted, scenario, estimate) %>%
    mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>%
    rename(cost_bn_currentLCU = cost_mn_currentLCU) %>%
    summarise(across(c(number, starts_with('cost')), sum, na.rm = T))
  
  hia_totals %>% filter(!double_counted) %>%
    group_by(scenario, estimate) %>%
    summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
    pivot_longer(where(is.numeric), names_to = 'Outcome', values_to = 'number') %>%
    bind_rows(hia_totals %>% filter(!double_counted)) %>%
    dplyr::select(-starts_with('cost')) %>%
    filter(!is.na(estimate)) %>%
    pivot_wider(names_from = estimate, values_from = number) %>%
    write_csv(file.path(output_dir, paste0('hia_results_',ccase,'.csv')))

}

## Continental 

NORA <- c('CAN','USA','MEX')
SOUA <- c('ARG', 'BOL', 'BRA', 'CHL', 'COL', 'ECU', 'GUY', 'PER', 'PRY', 'SUR', 'URY', 'VEN')
OCEA <- c('AUS', 'FJI', 'KIR', 'MHL', 'FSM', 'NRU', 'NZL', 'PLW', 'PNG', 'SLB', 'TON', 'TUV', 'VUT', 'WSM')
ASIA <- c('AFG', 'ARM', 'AZE', 'BHR', 'BGD', 'BTN', 'BRN', 'KHM', 'CHN', 'CYP', 'GEO', 'IND', 'IDN', 'IRN', 'IRQ', 'ISR', 'JPN', 'JOR', 'KAZ', 'KWT', 'KGZ', 'LAO', 'LBN', 'MYS', 'MDV', 'MNG', 'MMR', 'NPL', 'PRK', 'OMN', 'PAK', 'PHL', 'QAT', 'SAU', 'SGP', 'KOR', 'LKA', 'SYR', 'TWN', 'TJK', 'THA', 'TLS', 'TUR', 'TKM', 'ARE', 'UZB', 'VNM', 'YEM')
AFRI <- c('DZA', 'AGO', 'BEN', 'BWA', 'BFA', 'BDI', 'CPV', 'CMR', 'CAF', 'TCD', 'COM', 'COG', 'CIV', 'COD', 'DJI', 'EGY', 'GNQ', 'ERI', 'SWZ', 'ETH', 'GAB', 'GMB', 'GHA', 'GIN', 'GNB', 'KEN', 'LSO', 'LBR', 'LBY', 'MDG', 'MWI', 'MLI', 'MRT', 'MUS', 'MAR', 'MOZ', 'NAM', 'NER', 'NGA', 'RWA', 'STP', 'SEN', 'SYC', 'SLE', 'SOM', 'ZAF', 'SSD', 'SDN', 'TZA', 'TGO', 'TUN', 'UGA', 'ZMB', 'ZWE')
EUROP <- c('ALB', 'AND', 'AUT', 'BLR', 'BEL', 'BIH', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'ISL', 'IRL', 'ITA', 'KAZ', 'LVA', 'LIE', 'LTU', 'LUX', 'MLT', 'MDA', 'MCO', 'MNE', 'NLD', 'MKD', 'NOR', 'POL', 'PRT', 'ROU', 'RUS', 'SMR', 'SRB', 'SVK', 'SVN', 'ESP', 'SWE', 'CHE', 'TUR', 'UKR', 'GBR', 'VAT')


scenarios_to_process <- c('AF_OC',
                          'AS_OC_ME_AF_EAS',
                          'AS_OC_SA_ME_AF_EAS',
                          'AS_OC',
                          'AS',
                          'EU',
                          'ME_AF',
                          'NA_MX',
                          'NA',
                          'SA') # TODO change this

regs <- EUROP

for (ccase in (scenarios_to_process)){
  # calculate HIA totals and its summary; export hia_results.csv to output_dir
  hia_totals <- hia_fut %>%
    filter(scenario == ccase) %>%
    filter(iso3 %in% regs) %>%
    add_long_names() %>%
    group_by(Outcome = Outcome_long, Cause = Cause_long, Pollutant, iso3,
             double_counted, scenario, estimate) %>%
    mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>%
    rename(cost_bn_currentLCU = cost_mn_currentLCU) %>%
    summarise(across(c(number, starts_with('cost')), sum, na.rm = T))
  
  hia_totals %>% filter(!double_counted) %>%
    group_by(scenario, estimate) %>%
    summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
    pivot_longer(where(is.numeric), names_to = 'Outcome', values_to = 'number') %>%
    bind_rows(hia_totals %>% filter(!double_counted)) %>%
    dplyr::select(-starts_with('cost')) %>%
    filter(!is.na(estimate)) %>%
    pivot_wider(names_from = estimate, values_from = number) %>%
    write_csv(file.path(output_dir, paste0('EU_hia_results_',ccase,'.csv')))
  
}











