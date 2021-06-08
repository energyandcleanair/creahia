gis_dir <- "~/GIS/"

devtools::install_github('energyandcleanair/creahia')

library(creahia)
library(creapuff)
creahia::set_env('gis_dir',gis_dir)

# For development only
library(raster)
library(plyr)
library(tidyverse)
library(readxl)
library(zoo)
library(magrittr)

setwd(get_env('gis_dir'))
#system('gsutil rsync -r . gs://crea-data/gis')
project_dir <- "F:/TAPM/Sekong/results/"



# 01: Get coal additional concentrations from CALPUFF --------------------------------------
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=project_dir, hg_scaling=1e-3)
calpuff_files$scenario[calpuff_files$scenario=='sekong'] <- 'sekong_a'
scenarios = unique(calpuff_files$scenario)

get_grid_from_calpuff.inp <- function(scenarios=NULL,
                                      dir="C:/CALPUFF/CALPUFF_v7.2.1_L150618",
                                      filename_suffix="_CALPUFF_7.0.inp",
                                      file_paths=file.path(dir, paste0(scenarios, filename_suffix)),
                                      params_to_read = c('IUTMZN','UTMHEM','XORIGKM','YORIGKM','DGRIDKM','NX','NY'),
                                      ...) {
  if(is.null(scenarios)) scenarios <- file_path %>% basename %>% gsub(filename_suffix, '', .)
  file_paths %>%
    lapply(creapuff::readPuffInp, ...) %>%
    lapply('[', params_to_read) %>%
    lapply(data.frame) %>% bind_rows %>% tibble(scenario=scenarios, .)
}

gridparams <- get_grid_from_calpuff.inp(scenarios[1:3]) %>% distinct(across(contains('UTM')))

grids <- get_grids_calpuff(calpuff_files,
                           utm_zone = as.numeric(gridparams$IUTMZN),
                           utm_hem = gridparams$UTMHEM,
                           map_res=1)

calpuff_files %>%
  filter(period=='annual' | !is.na(threshold)) %>%
  make_tifs(grids = grids) -> conc_coal_only

species = unique(conc_coal_only$species)
grid_raster = conc_coal_only$conc_coal_only[[1]] %>% raster


# 02: Get base concentration levels --------------------------------------------------------
conc_base <- get_conc_base(species=species, grid_raster=grid_raster, no2_targetyear = NULL)
conc_base$conc_base$no2 %<>% multiply_by(1.2)
# 03: Combine and flatten: one row per scenario --------------------------------------------
concs <- combine_concs(conc_coal_only, conc_base) %>% flatten_concs() %>% add_pop()


# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
adm <- get_map_adm(grid_raster, admin_level=2, res="low")
#adm %<>% subset(country_id=='PHL') %>% head(10)
cities <- get_map_cities(grid_raster)

# 05: Extract concentrations ---------------------------------------------------------------
conc_adm <- extract_concs_at_map(concs, adm)
# conc_cities <- extract_concs_at_map(concs, cities)

saveRDS(conc_adm, file.path(project_dir, "conc_adm.RDS"))
# saveRDS(conc_cities, file.path(project_dir, "conc_cities.RDS"))

# If you start from here
conc_adm <- readRDS(file.path(project_dir, "conc_adm.RDS"))
scenarios <- names(conc_adm)


# 06: Get HIA data, in case you want to modify them.  ---------------------------------------
crfs <- get_crfs()
epi <- get_epi()
gemm <- get_gemm()
ihme <- get_ihme()
gbd <- get_gbd()
calc_causes <- get_calc_causes()
valuation <- get_valuation()
gdp <- get_gdp()


# 07: HIA Calculations: PAF -----------------------------------------------------------------
require(doParallel)
require(future)
plan(multisession(workers=detectCores()))

paf <- conc_adm %>% compute_hia_paf(calc_causes=calc_causes, gemm=gemm, gbd=gbd, ihme=ihme)
saveRDS(paf, file.path(project_dir, 'paf.RDS'))
# save.image()

# 08: HIA Calculations: EPI+PAF -------------------------------------------------------------
hia <- compute_hia_epi(species, paf, conc_map=conc_adm, epi=epi, regions=adm)
saveRDS(hia, file.path(project_dir, 'hia.RDS'))

hia <- readRDS(file.path(project_dir, 'hia.RDS'))

# 09: Scale with population growth
hia <- scale_hia_pop(hia, base_year=2015, target_year=2025)

hia %<>% filter(scenario != 'sekong') %>%
  group_by(across(c(where(is.character), -scenario))) %>%
  summarise_if(is.numeric, sum) %>% mutate(scenario='sekong') %>%
  bind_rows(hia %>% filter(scenario != 'sekong'))

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
