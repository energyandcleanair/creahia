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
#system('gsutil rsync -r . gs://crea-data/gis')
project_dir <- "F:/TAPM/Phils/case_results"
load(file.path(project_dir, '.RData'))


# 01: Get coal additional concentrations from CALPUFF --------------------------------------
calpuff_files <- creapuff::get_calpuff_files(ext=".csv", gasunit = 'ug', dir=project_dir)

calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>%
  get_conc_calpuff(utm_zone = 51, utm_hem = 'N', map_res =1) -> conc_additional

species = unique(conc_additional$species)
scenarios = unique(conc_additional$scenario)
grid_raster = conc_additional$conc_additional[[1]] %>% raster


# 02: Get base concentration levels --------------------------------------------------------
conc_base <- get_conc_baseline(species=species, grid_raster=grid_raster, no2_targetyear = NULL)
conc_base$conc_base$no2 %<>% multiply_by(1.2)
# 03: Combine and flatten: one row per scenario --------------------------------------------
concs <- combine_concs(conc_additional, conc_base) %>% flatten_concs() %>% add_pop()


# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
adm <- get_adm(grid_raster, admin_level=2, res="low")
#adm %<>% subset(country_id=='PHL') %>% head(10)
cities <- get_map_cities(grid_raster)

# 05: Extract concentrations ---------------------------------------------------------------
conc_adm <- extract_concs_at_regions(concs, adm, species)

saveRDS(conc_adm, file.path(project_dir, "conc_adm.RDS"))

# If you start from here
# conc_adm <- readRDS(file.path(project_dir, "conc_adm.RDS"))
# scenarios <- names(conc_adm)


# 06: Get HIA data, in case you want to modify them.  ---------------------------------------
crfs <- get_crfs()
epi <- get_epi()
gemm <- get_gemm()
ihme <- get_ihme()
gbd_rr <- get_gbd_rr()
calc_causes <- get_calc_causes()
valuation <- get_valuation()
gdp <- get_gdp()


# 07: HIA Calculations: PAF -----------------------------------------------------------------
require(doParallel)
require(future)
plan(multisession(workers=detectCores()))

paf <- compute_hia_paf(conc_adm, scenarios=scenarios, calc_causes=calc_causes, gemm=gemm, gbd_rr=gbd_rr, ihme=ihme)
saveRDS(paf, file.path(project_dir, 'paf.RDS'))
# save.image()

# 08: HIA Calculations: EPI+PAF -------------------------------------------------------------
hia <- compute_hia_epi(species, paf, conc_map=conc_adm, epi=epi, regions=adm)
saveRDS(hia, file.path(project_dir, 'hia.RDS'))
hia = readRDS(file.path(project_dir, 'hia.RDS'))

# 09: Scale with population growth
hia <- scale_hia_pop(hia, base_year=2015, target_year=2019)

hia %>% totalise_hia() %>% make_hia_table() %>% arrange(scenario) %>%
  write_csv(file.path(project_dir, 'hia_by_admin_area.csv'))

hia %>% totalise_hia(.groups=NULL) %>% make_hia_table() %>% arrange(scenario) %>%
  write_csv(file.path(project_dir, 'hia_totals.csv'))

# 10: Compute and extract economic costs
econ_costs <- hia %>% sel(-any_of('Deaths_Total')) %>%
  group_by(iso3, scenario, estimate) %>% summarise_if(is.numeric, sum, na.rm=T) %>%
  compute_econ_costs(results_dir=project_dir, projection_years=2019:2055)

COD = c(atimonan = 2025, kamangas = 2021, pagbilao = 1996,
        quezonpo = 2000, sbplquez = 2019, smcibaba = 2021,
        tagkaway=2024) %>% data.frame(scenario=names(.), COD=.)

econ_costs$cost_forecast %>% left_join(COD) %>%
  filter(year>=2021, year>=COD, year<COD+30) %>%
  group_by(estimate, Outcome_long, Cause_long, Pollutant, scenario) %>%
  summarise_if(is.numeric, sum) %>% sel(-COD) ->
  hia_cumu

hia_cumu %>% write_csv(file.path(project_dir, 'hia_cumulative.csv'))
