# library(remotes)
# remotes::install_github("energyandcleanair/creahia")
# devtools::install_github('energyandcleanair/creahia')
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creahia)
library(creapuff)
# For development only
library(raster)
library(plyr)
library(readxl)
library(zoo)
library(magrittr)


project_dir="I:/koreasteel"       # calpuff_external_data-2 persistent disk (project data)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files

gis_dir <- "F:/gis"                    # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


pollutants_to_process=c('NO2', 'PM2.5', 'PM10', 'SO2')

# Load CALMET parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM


calpuff_files_all <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>%
  mutate(scenario_description = case_when(scenario=="gwan"~'POSCO Gwangyang steel plant',
                                          scenario=="hyundai"~'Hyundai steel plant',
                                          scenario=="krsteel"~'All integrated steel plants',
                                          scenario=="pohang"~'POSCO Pohang steel plant'))

sources <- read_csv(file.path(project_dir, 'emissions/emissions inputs.csv'))

runs <- calpuff_files_all$scenario %>% unique

for (scenario_prefix in runs) {

  # HIA ###########################################################################################

  # 01: Get coal additional concentrations from CALPUFF -------------------------------------------
  calpuff_files <- calpuff_files_all %>% filter(scenario==scenario_prefix)

  scenario=scenario_prefix
  scenario_description=calpuff_files$scenario_description %>% unique

  # ================================ Define grids =================================================
  grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

  # ============================ Select pollutants ================================================
  calpuff_files %>%
    filter(period=='annual' | !is.na(threshold)) %>%
    filter(speciesName %in% pollutants_to_process) %>% make_tifs(grids = grids)

  # =============================== Get Perturbation Raster ========================================
  conc_perturbation <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)  %>%
    filter(scenario==scenario_prefix, period=='annual', speciesName %in% pollutants_to_process)

  conc_perturbation$conc_perturbation <- lapply(conc_perturbation$path, raster)
  names(conc_perturbation$conc_perturbation)=conc_perturbation$species

  grid_raster = conc_perturbation$conc_perturbation[[1]] %>% raster


  # 02: Get base concentration levels -------------------------------------------------------------
  conc_base <- get_conc_baseline(species=unique(calpuff_files$species), grid_raster=grid_raster, no2_targetyear = 2020) # 2020 # Target year of model simulations (CALPUFF and WRF)


  # 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
  regions <- get_adm(grid_raster, admin_level=2, res="low")

  # Input parameters:
  #
  # admin_level=2 -> Highest degree of res.
  # admin_level=1 -> Main regions.
  # admin_level=0 -> States
  #
  # res=null/"full" -> Highest res of GADM file
  # res="low"       -> Medium res
  # res="coarse"    -> Lowest res
  #
  # iso3s=c("IDN", "KHM", "LAO", "VNM", "THA")

  pollutants_for_hia = intersect(conc_perturbation$species, conc_base$species)


  # 04: HIA Calculations:
  # TODO : add no2_targetyear as input parameter in the wrapper, to calculate conc_base in the wrapper?
  # TODO : change name no2_targetyear -> no2_target_year
  # TODO : change name scale_base_year -> pop_base_year
  # TODO : change value for scale_base_year from 2020 -> 2019, as default in wrapper
  # TODO : change name scale_target_year -> pop_target_year
  hia <-  wrappers.compute_hia_two_images(conc_perturbation$conc_perturbation[pollutants_for_hia],       # perturbation_rasters=raster::stack(perturbation_map)
                                          baseline_rasters=conc_base$conc_baseline[pollutants_for_hia],  # baseline_rasters=raster::stack(who_map)
                                          regions=regions,
                                          # administrative_level=0,    # Overridden by : regions --> get_adm
                                          # administrative_res="full", # Overridden by : regions --> get_adm
                                          # administrative_iso3s=NULL, # Overridden by : regions --> get_adm
                                          scenario_name=scenario_prefix,
                                          scale_base_year=2019,        # Population base year : reference year of INPUT data, for total epidemiological and total population
                                          scale_target_year=2021,      # 2025 # Population target year (same as no2_targetyear?)
                                          crfs_version="C40",      # crfs_version="C40"
                                          epi_version="C40",       # epi_version="C40"
                                          valuation_version="default") # valuation_version="viscusi"


  saveRDS(hia, file.path(output_dir, paste0('hia','_',scenario_prefix,'.RDS')))
}

hia <- runs %>% lapply(function(scen) readRDS(file.path(output_dir, paste0('hia','_',scen,'.RDS')))) %>% bind_rows

calpuff_files_all %>% distinct(scenario, scenario_description) %>%
  left_join(hia, .) -> hia



# 06: Compute and extract economic costs --------------------------------------------------------
# TODO : change name scale_target_year -> pop_target_year


targetyears = c(2021, seq(2025,2050,5))

econ_costs <- hia %>% dplyr::select(-any_of('Deaths_Total')) %>%
  group_by(region_id, region_name, iso3, scenario, scenario_description, estimate) %>%
  summarise_if(is.numeric, sum, na.rm=T) %>%
  compute_econ_costs(results_dir=output_dir,
                     pop_targetyr=2021,  # 2025 # Same as scale_target_year
                     projection_years=targetyears,
                     iso3s_of_interest=NULL,
                     valuation_version="default")

econ_costs$cost_forecast %>% filter(year==2021) %>% write_excel_csv(file.path(output_dir, 'hia results by admin 2 area.csv'))
econ_costs %>% saveRDS(file.path(output_dir, 'econ_costs.RDS'))

#scale by pathway
#SO2 -85%, NOx -29%, PM -6% https://www.eea.europa.eu/publications/carbon-capture-and-storage/download
#weighted by Zhou coefficients: 0.461546678-1

econ_costs <- readRDS(file.path(output_dir, 'econ_costs.RDS'))
proj <- read_xlsx(file.path(project_dir, 'emissions/Steel Production by Production Route.xlsx'))

proj %>% pivot_longer(matches('[0-9]{4}'), names_to='year', values_to='steel_output') %>%
  dplyr::mutate(across(year, as.numeric),
                route=case_when(route=='Blast Furnace'~'BF',
                                grepl('Furnace with hydrogen', route)~'BF-H2',
                                grepl('Furnace CCS', route)~'BF-CCS',
                                T~route),
                scaling_pm = case_when(route=='BF'~1,
                                       route=='BF-CCS'~0.461546678,
                                       route=='BF-H2'~.7, T~0) *  steel_output,
                scaling_no2 = case_when(route=='BF'~1,
                                        route=='BF-CCS'~0.71,
                                        route=='BF-H2'~.7, T~0) * steel_output) %>%
  group_by(pathway, year) %>%
  dplyr::summarise(across(starts_with('scaling_'), sum)) %>%
  group_by(pathway) %>%
  dplyr::mutate(across(starts_with('scaling_'), function(x) x/x[year==2020]),
                year=ifelse(year==2020, 2021, year)) ->
  scaling

scaling %>% ggplot(aes(year, scaling_no2, col=pathway)) + geom_line()

econ_costs$cost_forecast %>% full_join(scaling)

econ_costs$cost_forecast %>%
  dplyr::group_by(across(c(scenario, estimate, iso3, matches('Outcome|Cause'), Pollutant, year))) %>%
  dplyr::summarise(across(c(number, cost.mnUSD), sum, na.rm=T)) %>%
  write_excel_csv(file.path(output_dir, 'hia results by country and year.csv'))




econ_costs$cost_forecast %>%
  filter(!is.na(year)) %>%
  dplyr::group_by(across(c(scenario, estimate, iso3, matches('Outcome|Cause|region_'), Pollutant))) %>%
  dplyr::mutate(groupnumber=cur_group_id()) -> indata

indata %>%
  group_modify(function(df, ...) {
    message(df$groupnumber[1])
    df %>% dplyr::summarise(across(c(number, cost.mnUSD),
                            function(x) {
                              x.out=NA
                              if(all(!is.na(x)) & length(x)>1) x.out=approx(df$year, x, 2022:2050)$y %>% sum()
                              return(x.out)
                            }))
    }) -> hia_cumu


hia_cumu %>% write_excel_csv(file.path(output_dir, 'hia results by admin 2 area, 2022-2050 cumulative.csv'))
hia_cumu %>%
  dplyr::group_by(across(c(scenario, estimate, iso3, matches('Outcome|Cause'), Pollutant))) %>%
  dplyr::summarise(across(c(number, cost.mnUSD), sum, na.rm=T)) %>%
  write_excel_csv(file.path(output_dir, 'hia results by country and year, 2022-2050 cumulative.csv'))

