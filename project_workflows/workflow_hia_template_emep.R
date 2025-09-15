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
library(tidyverse)

library(creahia)
#library(creapuff)
library(rcrea)
library(creahelpers)
require(rcrea)
require(creahelpers)
library(ncdf4)



# setup variables ---------------------------------------------------------


sim_year <- 2015
project_dir="H:/dieselgate_eu"

gis_dir <- "H:/gis"
#gis_dir <- get_gis_dir() # CREA GIS files

input_dir <- file.path(project_dir, "input/diff.fullrun/",sim_year)

output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files


pollutants_to_process <- c( 'O3_8H','PM25','NO2')
scenarios_to_process <- c(
  'Belgium',
  'Denmark',
  'EU',
  'France',
  'Germany',
  'Greece',
  'Ireland',
  'Italy',
  'Luxembourg',
  'Netherlands',
  'Portugal',
  'Spain',
  'Sweden',
  'UK',
  'Finland',
  'Austria'
) 

  
  
  
  
#  'diff.2015.base_Finland_fullrun.nc',
#  'diff.2015.base_Austrira_fullrun.nc'
#) %>% `names<-`(paste0('GR', 1:2))  # make sure the file names match the scenarios

# project specific variables
#bands_to_process <- c("SURF_ppb_O3")
bands_to_process <- c('SURF_ug_PMFINE', 
                      'SURF_ppb_O3', 
                      'SURF_ug_NO2')

# economic valuation variables
targetyears <- 2015 # TODO change with intended year


# 01: Prepare CALMET results ----------------------------------------------
# convert all required .nc to .tif


nc_files <- list.files(input_dir, full.names = T) 

##Modify ozone unit to ug/m3 to match with hia package, Do not use after modified.

#for (nc_file in nc_files){
  # Open the NetCDF file
#  nc_data <- nc_open(nc_file, write = TRUE)
  
  # Define the name of the variable to modify and rename
#  old_variable_name <- "SURF_ppb_O3"  # Replace with the actual variable name
#  new_variable_name <- "SURF_ug_O3"  # Replace with the new variable name
#  var_data <- ncvar_get(nc_data, old_variable_name)
  
  # Modify the variable data (example: increase all values by 10)
#  modified_var_data <- var_data * 1.96
  
  # Write the modified data back to the variable in the NetCDF file
#  ncvar_put(nc_data, old_variable_name, modified_var_data)
  
 
  
  # Close the NetCDF file
#  nc_close(nc_data)
  
  # Print a message indicating the file was processed
#  cat("Modified variable in file:", nc_file, "\n")
#}


output_folder <- file.path(output_dir,'output', 'tifs')

sapply(nc_files, function(file){
  hia_nc <- rast(file)
  bands_to_process <- c('SURF_ppb_O3')
  

  
  # requirements for HIA calculation
  # 1. dataframe with:
  ### path: path to the tif
  ### species: pollutant to process
  ### scenario: scenario to process
  
  pbsapply(bands_to_process, function(band) {
#   scenario <- file %>% str_split_i('_', 2) %>% str_split_i('.', 1)
    scenario <- file %>% str_split_i('_', 3) 
    poll <- band %>% str_split_i('_', 3) %>% if_else(. == 'PMFINE', 'PM25', if_else(. == 'O3', 'O3_8H', .),.)
    hia_nc[band] %>% mean() %>% 
    writeRaster(file.path(output_folder, glue('{scenario}_{poll}.{sim_year}.tif')), overwrite = TRUE)
  })
})

perturbation_rasters <- tibble(path = list.files(output_folder, pattern = "\\.tif$",full.names = T)) %>% 
#perturbation_rasters <- tibble(path = list.files(output_folder, full.names = T)) %>%
#perturbation_rasters <- tibble(path = list.files(file.path(output_folder, 'tifs'), full.names = T)) %>%
  mutate(filename = tools::file_path_sans_ext(basename(path)),
         scenario = filename %>% str_split_i('_', 1),
#         pattern_tem = filename %>% str_split_i('_', 2),
         year =   sim_year,
  #filename %>% str_split_i('_', 2) %>% str_split_i("\\.",2),
         species = filename %>% str_split_i('_', 2) %>% str_split_i("\\.",1) %>% if_else(. == 'O3', 'O3_8H', .)%>%tolower())

# grid raster for the domain
grid_raster <- terra::rast((list.files(file.path(output_folder), 
                                       full.names = T))[[1]])


# 02: Get base concentration levels ---------------------------------------
#pollutants_to_process <- c('O3')
conc_base <- creahia::wrappers.get_conc_baseline(species = pollutants_to_process %>% tolower(),
                                                 grid_raster = raster(grid_raster),
                                                 no2_targetyear = 2018,
                                                 pm25_to_pm10_ratio = .7)




# 03: Create support maps -------------------------------------------------
shp <- creahelpers::get_adm(level = 2, res = 'low', version = '36') #after Danny's correction
#shp <- creahelpers::get_adm(level = 2, res = 'low')
regions <- creahia::get_model_adm(grid_raster, shp = shp, admin_level = 2)

causes_to_include <- creahia::get_calc_causes(filter = 'Death|YLD')
#causes_to_include <- creahia::get_calc_causes(causes_set = "GEMM and GBD", filter = NULL)

# 04: HIA calculations ----------------------------------------------------
#pollutants_to_process <- c('O3_8H')
#scenarios_to_process <- "Austria"

creahia::wrappers.compute_hia_two_images(scenarios = scenarios_to_process,
                                         perturbation_rasters_table = perturbation_rasters,
                                         baseline_rasters_table = conc_base,
                                         grid_raster = grid_raster,
                                         regions = regions,
                                         scale_base_year = 2015, # Population base year : reference year of INPUT data, for total epidemiological and total population
                                         scale_target_year = 2019, # Population target year
                                         crfs_version = "C40",
                                         epi_version = "C40",
                                         return_concentrations = T,
                                         gbd_causes = 'default',
                                         calc_causes = causes_to_include,
                                         output_folder = output_dir,
                                         custom_glue = paste0(glue("hia_GBD2019"),
                                                              "_{scen}.RDS"))

# read the HIA data and summarise the total
#hia_totals <- creahia::get_hia_totals(scenarios = scenarios_to_process,
#                             shp = shp, output_folder = output_dir)

# read the HIA data
#runs <- scenarios_to_process
runs <- scenarios_to_process
# EU states and UK. 
im_regs <- c('AUT', 'BEL', 'BGR', 'HRV', 'CZE','DNK', 'EST', 'FIN', 'FRA',
             'DEU','GRC','HUN', 'IRL', 'ITA','LVA', 'LTU', 'LUX','MLT', 'NLD',
             'POL','PRT','ROU','SVK','SVN','ESP','SWE','GBR')
hia <- runs %>% lapply(function(scen) readRDS(file.path(project_dir, 'HIA/hia',
                                                        glue('hia_GBD2019_{scen}.RDS')))$hia) %>%
  bind_rows




hia_x <- hia%>%filter(iso3 %in% im_regs)



hia_totals <- hia_x %>%
  left_join(shp@data %>% dplyr::select(region_id = GID_2, starts_with('NAME'))) %>%
  group_by(across(c(starts_with('NAME'), Outcome, Pollutant, Cause, AgeGrp, iso3,
                    scenario, estimate, double_counted))) %>%
  summarise(across(number, sum)) %>%
  #  filter(Pollutant != pollutants_to_process | Cause != 'AllCause') %>%
  #  filter(Pollutant != 'pollutants_to_process' | Cause != 'GEMM and GBD') %>% 
  #  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause' ~ 1,
  mutate(number = number * case_when(Pollutant != 'pollutants_to_process' | Cause != 'AllCause' ~ 1,
#  mutate(number = number * case_when(Pollutant != 'pollutants_to_process' | Cause != 'GEMM and GBD' ~ 1,
                                     estimate == 'central' ~ 1/2,
                                     estimate == 'low' ~ 1/2,
                                     estimate == 'high' ~ 2/3))




## test
hia_totals_c1 <- hia_totals%>%filter(NAME_2 %in% c('Greater London','Paris'))


hia_totals_london <-hia_totals_c1%>%filter(NAME_2 %in% c('Greater London'))%>%filter(scenario %in% c('UK'))
hia_totals_paris<-hia_totals%>%filter(NAME_1 %in% c('ÃŽle-de-France'))%>%filter(scenario %in% c('France'))
#im_regs1 <- c('FRA','GBR')

#for (regs in im_regs1){

#hia_totals_x <- hia_totals%>%filter(iso3 == regs)
hia_totals_x <- hia_totals_paris

hia_cost<- creahia::get_hia_cost(hia = hia_totals_x, valuation_version = "viscusi",
                                  current_year = 2015)

#hia_cost <- hia_cost %>%
#  filter(NAME_2 == "Greater London")

hia_cost %>%
  distinct(Outcome, valuation_world_2017, valuation_current_usd, iso3, reference) %>%
  na.omit %>%
  add_long_names() %>%
  dplyr::select(-Outcome, Outcome = Outcome_long) %>%
  mutate(across(where(is.numeric), function(x) x %>% signif(4) %>% scales::comma(accuracy = 1))) %>%
  relocate(Outcome) %>%
  relocate(reference, .after = everything()) %>%
  write_csv(file.path(output_dir, paste0('valuations.',regs,'csv')))


for (targetyears in seq(2009,2040)){

hia_fut <- hia_cost %>% creahia::get_econ_forecast(forecast_years = targetyears, reference_year = 2019, use_gdp_scaling = TRUE)
hia_fut_totals <- hia_fut %>% add_long_names() %>%
  group_by(Outcome = Outcome_long, Cause = Cause_long, Pollutant,
           double_counted, scenario, estimate, iso3, NAME_2) %>%
  mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>%
  rename(cost_bn_currentLCU = cost_mn_currentLCU) %>%
  summarise(across(c(number, starts_with('cost')), sum, na.rm = T))


temp_name <- paste0("hia_fut_totals_city_ÃŽle-de-France_",targetyears)
assign(temp_name,hia_fut_totals)

}

#im_regs <- c("GBR")
#im_regs <- c('AUT', 'BEL', 'BGR', 'HRV', 'CZE','DNK', 'EST', 'FIN', 'FRA',
#             'DEU','GRC','HUN', 'IRL', 'ITA','LVA', 'LTU', 'LUX','MLT', 'NLD',
#             'POL','PRT','ROU','SVK','SVN','ESP','SWE','GBR')


emis_inv <- read.csv("H:/dieselgate_eu/input/Cumulative_Years_Country_Kaiyu.csv")

emis_inv <- emis_inv%>%
  mutate(country_clean = ifelse(country_clean == 'EU12/13','EU',country_clean))


for (targetyears in seq(2009,2040)){

#  hia_fut_totals <- get(paste0("hia_fut_totals_",regs,"_",targetyears))%>%
  hia_fut_totals <- get(paste0("hia_fut_totals_",targetyears))%>%
      mutate(number = ifelse(Pollutant == "O3_8h",number * 1.96, number),
           cost_mn_currentUSD = ifelse(Pollutant == "O3_8h",cost_mn_currentUSD * 1.96, cost_mn_currentUSD),
           cost_bn_currentLCU = ifelse(Pollutant == "O3_8h",cost_bn_currentLCU * 1.96, cost_bn_currentLCU))

    for (scale_scenario in scenarios_to_process){
    sf_a <-emis_inv %>%
      filter(cal_year == 2015) %>%
      filter(country_clean == scale_scenario) %>%
      pull(annual_emiss_Excess)
    
    sf_b <- emis_inv %>%
      filter(cal_year == targetyears) %>%
      filter(country_clean == scale_scenario) %>%
      pull(annual_emiss_Excess)
    
    sf = sf_b/sf_a
    hia_fut_totals <-  hia_fut_totals%>%
      mutate(
        number = if_else(scenario == scale_scenario, number*sf,number),
        cost_mn_currentUSD = if_else(scenario == scale_scenario, cost_mn_currentUSD*sf,cost_mn_currentUSD),
        cost_bn_currentLCU = if_else(scenario == scale_scenario, cost_bn_currentLCU*sf,cost_bn_currentLCU)
      )
    }
      


  
#  hia_fut_totals_out <- hia_fut_totals%>%
#    filter(iso3 == regs)

  hia_fut_totals_out <- hia_fut_totals
  
  hia_fut_totals_out <- hia_fut_totals_out%>%
    group_by(Outcome,Cause,Pollutant,double_counted,scenario,estimate,iso3)%>%
    summarize(
      across(where(is.numeric), sum, na.rm = TRUE)) # Sum numeric columns


  hia_fut_totals_out<-hia_fut_totals_out %>% filter(!double_counted) %>%
  group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
  pivot_longer(where(is.numeric), names_to = 'Outcome', values_to = 'number') %>%
  bind_rows(hia_fut_totals_out %>% filter(!double_counted)) %>%
  dplyr::select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
    mutate(
      year = targetyears,
      target_reg = regs
    )%>%
  write_csv(file.path(output_dir,"temp",paste0('hia_results_',regs,"_",targetyears,'.csv')))
  
}
#}



##for UK and France, write out the county level


for (targetyears in seq(2009,2040)){
  
  hia_fut_totals <- get(paste0("hia_fut_totals_city_ÃŽle-de-France_",targetyears))%>%
#    filter(scenario%in%c("UK","France"))%>%
    mutate(number = ifelse(Pollutant == "O3_8h",number * 1.96, number),
           cost_mn_currentUSD = ifelse(Pollutant == "O3_8h",cost_mn_currentUSD * 1.96, cost_mn_currentUSD),
           cost_bn_currentLCU = ifelse(Pollutant == "O3_8h",cost_bn_currentLCU * 1.96, cost_bn_currentLCU))
  ## mod ozone since unit diff (ppb to ug/m3), factor in 1.96 
  
  
  for (scale_scenario in c("UK","France")){
    sf_a <-emis_inv %>%
      filter(cal_year == 2015) %>%
      filter(country_clean == scale_scenario) %>%
      pull(annual_emiss_Excess)
    
    sf_b <- emis_inv %>%
      filter(cal_year == targetyears) %>%
      filter(country_clean == scale_scenario) %>%
      pull(annual_emiss_Excess)
    
    sf = sf_b/sf_a
    hia_fut_totals <-  hia_fut_totals%>%
      mutate(
        number = if_else(scenario == scale_scenario, number*sf,number),
        cost_mn_currentUSD = if_else(scenario == scale_scenario, cost_mn_currentUSD*sf,cost_mn_currentUSD),
        cost_bn_currentLCU = if_else(scenario == scale_scenario, cost_bn_currentLCU*sf,cost_bn_currentLCU)
      )
  }
  
#  for (regs in c("FRA","GBR")){
    
    
    hia_fut_totals_out <- hia_fut_totals
    #%>%
    #  filter(iso3 %in% regs)
  
    
    hia_fut_totals_out <- hia_fut_totals_out%>%
      group_by(Outcome,Cause,Pollutant,double_counted,scenario,estimate,iso3,NAME_2)%>%
      summarize(
        across(where(is.numeric), sum, na.rm = TRUE)) # Sum numeric columns
    
    
    hia_fut_totals_out<-hia_fut_totals_out %>% filter(!double_counted) %>%
      group_by(scenario, estimate) %>%
      summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
      pivot_longer(where(is.numeric), names_to = 'Outcome', values_to = 'number') %>%
      bind_rows(hia_fut_totals_out %>% filter(!double_counted)) %>%
      dplyr::select(-starts_with('cost')) %>%
      filter(!is.na(estimate)) %>%
      pivot_wider(names_from = estimate, values_from = number) %>%
      mutate(
        year = targetyears,
        target_reg = regs
      )%>%
      write_csv(file.path(output_dir,"temp",paste0('hia_results_city_ÃŽle-de-France',targetyears,'.csv')))
    
  }
#}







# post-organized the data


##Merge csv files
csv_files <- list.files(file.path(output_dir,"temp"), pattern = "\\.csv$", full.names = TRUE)
list_of_dfs <- lapply(csv_files, read.csv)
#print(list_of_dfs)
merged_df <- bind_rows(list_of_dfs)%>%
  write_csv(file.path(output_dir,paste0('hia_results_all_o3updated_mod2.csv')))


##Merge csv files
csv_files <- list.files(file.path(output_dir,"temp"), pattern = "hia_results_city_", full.names = TRUE)
list_of_dfs <- lapply(csv_files, read.csv)
#print(list_of_dfs)
merged_df <- bind_rows(list_of_dfs)%>%
  write_csv(file.path(output_dir,paste0('hia_results_city_all_sus.csv')))


##Merge csv files for london only
csv_files <- list.files(file.path(output_dir,"temp"), pattern = "hia_results_city_London", full.names = TRUE)
list_of_dfs <- lapply(csv_files, read.csv)
#print(list_of_dfs)
merged_df <- bind_rows(list_of_dfs)%>%
  write_csv(file.path(output_dir,paste0('hia_results_city_sus_London_only_mod2.csv')))

##Merge csv files for paris only
csv_files <- list.files(file.path(output_dir,"temp"), pattern = "hia_results_city_Paris", full.names = TRUE)
list_of_dfs <- lapply(csv_files, read.csv)
#print(list_of_dfs)
merged_df <- bind_rows(list_of_dfs)%>%
  write_csv(file.path(output_dir,paste0('hia_results_city_sus_Paris_only_mod2.csv')))

##Merge csv files for ÃŽle-de-France only
csv_files <- list.files(file.path(output_dir,"temp"), pattern = "hia_results_city_ÃŽle-de-France", full.names = TRUE)
list_of_dfs <- lapply(csv_files, read.csv)
#print(list_of_dfs)
merged_df <- bind_rows(list_of_dfs)%>%
  write_csv(file.path(output_dir,paste0('hia_results_city_sus_ÃŽle-de-France_only_mod2.csv')))




###scale data from sus emission to real-world emission


emis_inv <- read.csv("H:/dieselgate_eu/input/Cumulative_Years_Country_Kaiyu.csv")
emis_inv <- emis_inv%>%
  mutate(country_clean = ifelse(country_clean == 'EU12/13','EU',country_clean))


  df <- read.csv(file.path(output_dir,paste0('hia_results_city_sus_ÃŽle-de-France_only_mod2.csv')))
  for (targetyears in seq(2009,2040)){
    for (scale_scenario in scenarios_to_process){
      sf_a <-emis_inv %>%
        filter(cal_year == targetyears) %>%
        filter(country_clean == scale_scenario) %>%
        pull(annual_emiss_Real)
      
      sf_b <- emis_inv %>%
        filter(cal_year == targetyears) %>%
        filter(country_clean == scale_scenario) %>%
        pull(annual_emiss_Excess)
      
      sf = sf_a/sf_b 
  
      df <- df %>%
        mutate(central = ifelse(scenario == scale_scenario & year == targetyears,central * sf, central),
               high = ifelse(scenario == scale_scenario & year == targetyears,high * sf, high),
               low = ifelse(scenario == scale_scenario & year == targetyears,low * sf, low))
  
    }
}
  
  write.csv(df,file.path(output_dir,paste0('hia_results_city_real_ÃŽle-de-France_only_mod2.csv')))
  
  
  
  df <- read.csv(file.path(output_dir,paste0('hia_results_city_all_sus_o3updated.csv')))
  for (targetyears in seq(2009,2040)){
    for (scale_scenario in scenarios_to_process){
      sf_a <-emis_inv %>%
        filter(cal_year == targetyears) %>%
        filter(country_clean == scale_scenario) %>%
        pull(annual_emiss_Real)
      
      sf_b <- emis_inv %>%
        filter(cal_year == targetyears) %>%
        filter(country_clean == scale_scenario) %>%
        pull(annual_emiss_Excess)
      
      sf = sf_a/sf_b 
      
      df <- df %>%
        mutate(central = ifelse(scenario == scale_scenario & year == targetyears,central * sf, central),
               high = ifelse(scenario == scale_scenario & year == targetyears,high * sf, high),
               low = ifelse(scenario == scale_scenario & year == targetyears,low * sf, low))
      
    }
  }
  
  write.csv(df,file.path(output_dir,paste0('hia_results_city_all_real_o3updated.csv')))
  
  
  
### Test isolate NO2 death for different causes
  
  hia_fut_totals_test <- hia_fut_totals%>%
    filter(Pollutant %in% c("NO2"))%>%
    filter(Outcome %in% c("deaths"))%>%
    filter(iso3 %in% c("FRA"))
  
   hia_fut_totals_test <- hia_fut_totals_test%>%
    group_by(Outcome,Cause,Pollutant,double_counted,scenario,estimate,iso3)%>%
    summarize(
      across(where(is.numeric), sum, na.rm = TRUE)) # Sum numeric columns
  
  
  hia_fut_totals_test_out<-hia_fut_totals_test %>% filter(!double_counted) %>%
    group_by(scenario, estimate) %>%
    summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
    pivot_longer(where(is.numeric), names_to = 'Outcome', values_to = 'number') %>%
    bind_rows(hia_fut_totals_test %>% filter(!double_counted)) %>%
    dplyr::select(-starts_with('cost')) %>%
    filter(!is.na(estimate)) %>%
    pivot_wider(names_from = estimate, values_from = number) 
  
  #%>%
    
   # write_csv(file.path(output_dir,"temp",paste0('hia_results_city_ÃŽle-de-France',targetyears,'.csv')))

  
