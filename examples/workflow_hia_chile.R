# library(remotes)
# remotes::install_github("energyandcleanair/creahia")
# devtools::reload(pkgload::inst("creahia"))
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


# Parameters ###################################################################

# ============================= Project specific ===============================
project_dir="G:/projects/chile"        # calpuff_external_data persistent disk (project data)
# project_dir <- "F:/TAPM/Sekong/results/"
output_dir <- file.path(project_dir,"calpuff_suite") # Where to write all generated files

# ================================ General =====================================
gis_dir <- "F:/gis"                    # The folder where we store general GIS data

# setwd(get_env('gis_dir'))
# system('gsutil rsync -r gs://crea-data/gis .')  # Run this command in local gis folder


# HIA ########################################################################

# Load CALMET parameters
calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# 01: Get coal additional concentrations from CALPUFF --------------------------------------
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
# calpuff_files$scenario = "reference_scenario"
# calpuff_files$scenario[calpuff_files$scenario=='sekong'] <- 'sekong_a'
# scenarios = unique(calpuff_files$scenario)

# Select only chileall
calpuff_files %<>% slice(grep('chileall', calpuff_files$name)) # %>% make_tifs(grids = grids)

# get_grid_from_calpuff.inp <- function(scenarios=NULL,
#                                       dir="C:/CALPUFF/CALPUFF_v7.2.1_L150618",
#                                       filename_suffix="_CALPUFF_7.0.inp",
#                                       file_paths=file.path(dir, paste0(scenarios, filename_suffix)),
#                                       params_to_read = c('IUTMZN','UTMHEM','XORIGKM','YORIGKM','DGRIDKM','NX','NY'),
#                                       ...) {
#   if(is.null(scenarios)) scenarios <- file_path %>% basename %>% gsub(filename_suffix, '', .)
#   file_paths %>%
#     lapply(creapuff::readPuffInp, ...) %>%
#     lapply('[', params_to_read) %>%
#     lapply(data.frame) %>% bind_rows %>% tibble(scenario=scenarios, .)
# }
#
# gridparams <- get_grid_from_calpuff.inp(scenarios[1:3]) %>% distinct(across(contains('UTM')))




# grids <- get_grids_calpuff(calpuff_files,
#                            utm_zone = as.numeric(gridparams$IUTMZN),
#                            utm_hem = gridparams$UTMHEM,
#                            map_res=1)

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

# calpuff_files %>%
#   # filter(period=='annual' | !is.na(threshold)) %>%  # Threshold : why ?!
#   filter(period=='annual') %>%
#   filter( speciesName=='NO2' | speciesName=='PM2.5') %>%  # For now, just NO2 and PM2.5 species
#   make_tifs(grids = grids) -> conc_perturbation

calpuff_files %>%
  # filter(period=='annual' | !is.na(threshold)) %>%  # Threshold : why ?!
  filter(period=='annual') %>%
  filter( speciesName=='NO2' | speciesName=='PM2.5'  | speciesName=='SO2') %>% make_tifs(grids = grids)  # For now, just NO2 and PM2.5 species


conc_perturbation <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)  %>%
  # filter(period=='annual' | !is.na(threshold)) %>%  # Threshold : why ?!
  filter(period=='annual') %>%
  filter( speciesName=='NO2' | speciesName=='PM2.5'  | speciesName=='SO2') %>%
  dplyr::select(species, path, scenario) %>%
  tibble()

conc_perturbation$conc_perturbation <- lapply(conc_perturbation$path, raster)

# Select scenario
#scenario <- "chileall"  # Loop on scenarios ?
#conc_perturbation <- conc_perturbation %>% filter(scenario==!!scenario)

species = unique(conc_perturbation$species)

grid_raster = conc_perturbation$conc_perturbation[[1]] %>% raster
# Test with a smaller domain
# grid_raster  %>% extent() %>% add(-1200) -> new_extent  # Test
# grid_raster %<>% crop(new_extent)  # Test


# 02: Get base concentration levels --------------------------------------------------------
conc_base <- get_conc_baseline(species=species, grid_raster=grid_raster, no2_targetyear = NULL)
conc_base$conc_baseline$no2 %<>% multiply_by(1.2)


# 03: Combine and flatten: one row per scenario --------------------------------------------
# conc_perturbation_ <-  conc_perturbation %>% select(species, scenario, conc_perturbation)
concs <- combine_concs(conc_perturbation, conc_base) %>% flatten_concs() %>% add_pop(grid_raster)  # Added: grid_raster


# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
adm <- get_adm(grid_raster, admin_level=2, res="low")
#adm %<>% subset(country_id=='PHL') %>% head(10)
cities <- get_map_cities(grid_raster)

# 05: Extract concentrations ---------------------------------------------------------------
conc_adm <- extract_concs_at_regions(concs, adm, species)
saveRDS(conc_adm, file.path(project_dir, "conc_adm.RDS"))


# ******************************************************************************
# If you start from here
conc_adm <- readRDS(file.path(project_dir, "conc_adm.RDS"))
scenarios <- names(conc_adm)


# 06: Get HIA data, in case you want to modify them.  ---------------------------------------
crfs <- get_crfs()
epi <- get_epi()
gemm <- get_gemm()
ihme <- get_ihme()
gbd_rr <- get_gbd_rr()
calc_causes <- get_calc_causes()


# 07: HIA Calculations: PAF -----------------------------------------------------------------
require(doParallel)
require(future)
plan(multisession(workers=detectCores()))

# browser()

paf <- conc_adm %>% compute_hia_paf(calc_causes=calc_causes, gemm=gemm, gbd_rr=gbd_rr, ihme=ihme)  #, conc_map=conc_adm)  # Added: conc_adm
saveRDS(paf, file.path(project_dir, 'paf.RDS'))
# save.image()

# 08: HIA Calculations: EPI+PAF -------------------------------------------------------------
hia <- compute_hia_epi(species, paf, conc_map=conc_adm, epi=epi, regions=adm)
saveRDS(hia, file.path(project_dir, 'hia.RDS'))

hia <- readRDS(file.path(project_dir, 'hia.RDS'))

# 09: Scale with population growth
hia <- scale_hia_pop(hia, base_year=2015, target_year=2025)

# hia %<>% filter(scenario != 'sekong') %>%
#   group_by(across(c(where(is.character), -scenario))) %>%
#   summarise_if(is.numeric, sum) %>% mutate(scenario='sekong') %>%
#   bind_rows(hia %>% filter(scenario != 'sekong'))

hia %>% totalise_hia() %>% make_hia_table() %>% arrange(scenario) %>%
  write_csv(file.path(project_dir, 'hia_by_admin_area.csv'))

hia %>% totalise_hia(.groups=NULL) %>% make_hia_table() %>% arrange(scenario) %>%
  write_csv(file.path(project_dir, 'hia_totals.csv'))

# 10: Compute and extract economic costs
econ_costs <- hia %>% sel(-any_of('Deaths_Total')) %>%
  group_by(iso3, scenario, estimate) %>% summarise_if(is.numeric, sum, na.rm=T) %>%
  compute_econ_costs(results_dir=project_dir, projection_years=2025:2054, pop_targetyr=2025)

econ_costs$cost_forecast %>%
  group_by(estimate, Outcome_long, Cause_long, Pollutant, scenario, iso3) %>%
  summarise_if(is.numeric, sum) ->
  hia_cumu

hia_cumu %>% write_csv(file.path(project_dir, 'hia_cumulative.csv'))
