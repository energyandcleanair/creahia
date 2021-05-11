gis_dir <- "~/GIS/"

devtools::install_github('energyandcleanair/creahia')

library(creahia)
creahia::set_env('gis_dir',gis_dir)

# For development only
library(creahelpers)
library(raster)
library(plyr)
library(readxl)
library(zoo)
library(magrittr)

setwd(get_env('gis_dir'))
system('gsutil rsync -r . gs://crea-data/gis')
system('gsutil cp -r gs://crea-data/gis/* ./')
project_dir <- "F:/TAPM/Phils/case_results"



# 01: Get coal additional concentrations from CALPUFF --------------------------------------
calpuff_files <- getPuffCSVs(ext=".csv", gasunit = 'ug', dir=project_dir)

calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>%
  get_conc_calpuff(utm_zone = 51, utm_hem = 'N', map_res =1) -> conc_coal_only

species = unique(conc_coal_only$species)
scenarios = unique(conc_coal_only$scenario)
grid_raster = conc_coal_only$conc_coal_only[[1]] %>% raster


# 02: Get base concentration levels --------------------------------------------------------
conc_base <- get_conc_base(species=species, grid_raster=grid_raster, no2_targetyear = NULL)
conc_base$conc_base$no2 %<>% multiply_by(1.2)
# 03: Combine and flatten: one row per scenario --------------------------------------------
concs <- combine_concs(conc_coal_only, conc_base) %>% flatten_concs() %>% add_pop()


# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
adm <- get_map_adm(grid_raster, admin_level=2, res="low")
adm %<>% subset(country_id=='PHL') %>% head(10)
cities <- get_map_cities(grid_raster)

# 05: Extract concentrations ---------------------------------------------------------------
conc_adm <- extract_concs_at_map(concs, adm)
# conc_cities <- extract_concs_at_map(concs, cities)

# saveRDS(conc_adm, file.path(project_dir, "conc_adm.RDS"))
# saveRDS(conc_cities, file.path(project_dir, "conc_cities.RDS"))

# If you start from here
# conc_adm <- readRDS(file.path(project_dir, "conc_adm.RDS"))
# scenarios <- names(conc_adm)


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

paf <- compute_hia_paf(conc_adm, scenarios=scenarios, calc_causes=calc_causes, gemm=gemm, gbd=gbd, ihme=ihme)
# saveRDS(paf, file.path(project_dir, 'paf.RDS'))
# save.image()

# 08: HIA Calculations: EPI+PAF -------------------------------------------------------------
hia <- compute_hia_epi(species, paf, conc_map=conc_adm, epi=epi, regions=adm)
# saveRDS(hia, file.path(project_dir, 'hia.RDS'))

# 09: Scale with population growth
hia <- scale_hia_pop(hia, base_year=2015, target_year=2019)

hia_table <- hia %>% make_hia_table()
hia_table %>% write_csv(file.path(project_dir, 'hia_table.csv'))

# 10: Compute and extract economic costs
econ_costs <- compute_econ_costs(hia, results_dir=project_dir)

