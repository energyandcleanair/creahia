# library(remotes)
# remotes::install_github("energyandcleanair/creahia")

library(creahia)

# library(creahelpers)
# library(creapuff)
# library(raster)
# library(plyr)
# library(readxl)
# library(zoo)
# library(magrittr)

project_dir <- "examples/example_ph" # Assuming your working directory is the repository folder
gis_dir <- "/Volumes/ext1/gis/"


# 01: Get coal additional concentrations from CALPUFF --------------------------------------
conc_additional <- creahia::get_conc_calpuff(calpuff_dir=project_dir, utm_zone=51, utm_hem='N', map_res=1)

species = unique(conc_additional$species)
scenarios = unique(conc_additional$scenario)
grid_raster = conc_additional$conc_perturbation[[1]] %>% raster


# 02: Get base concentration levels --------------------------------------------------------
conc_baseline <- creahia::get_conc_baseline(species=species, grid_raster=grid_raster)


# 03: Combine and flatten: one row per scenario --------------------------------------------
concs <- creahia::combine_concs(conc_additional, conc_baseline) %>% flatten_concs() %>% add_pop(grid_raster)


# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
regions <- creahia::get_adm(grid_raster, admin_level=2)
regions <- regions[1:5,] # For example purposes, we only keep a subset


# 05: Extract concentrations ---------------------------------------------------------------
conc_regions <- creahia::extract_concs_at_regions(concs, regions, species)

# Saving point: if you want to restart from here later on
# saveRDS(conc_adm, file.path(project_dir, "conc_adm.RDS"))
# conc_adm <- readRDS(file.path(project_dir, "conc_adm.RDS"))
# scenarios <- names(conc_adm)

# 06: Get HIA data, in case you want to modify them.  ---------------------------------------
# These are the values used by default
# crfs <- get_crfs()
# epi <- get_epi()
# gemm <- get_gemm()
# ihme <- get_ihme()
# gbd_rr <- get_gbd_rr()
# calc_causes <- get_calc_causes()
# valuation <- get_valuation()
# gdp <- get_gdp()

# 07: HIA Calculations: PAF -----------------------------------------------------------------
hia <- compute_hia(conc_map=conc_regions,
                   species=species,
                   regions=regions)


# 09: Scale with population growth
hia <- scale_hia_pop(hia, base_year=2015, target_year=2019)

# 10: Compute and extract economic costs
econ_costs <- compute_econ_costs(hia, results_dir=project_dir)

# 11: Export results
hia_table <- hia %>% totalise_hia() %>% make_hia_table()
write_csv(hia_table, file.path(project_dir, 'hia_table.csv'))



