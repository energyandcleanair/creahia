library(remotes)

remotes::install_github("energyandcleanair/creahia")
library(creahia)
library(raster)
library(magrittr)

results_dir <- "example_hsbc"
gis_dir <- "/Volumes/ext1/gis/"


# 01: Get coal additional concentrations from raster directly -------------------------------
conc_coal_only <- tibble(scenario="scenario1",
                         species="pm25",
                         conc_coal_only=list(raster::raster("../studies/202105_hsbc_coal/results/coal_pm25.grd")))

species = unique(conc_coal_only$species)
scenarios = unique(conc_coal_only$scenario)
grid_raster = conc_coal_only$conc_coal_only[[1]] %>% raster

# 02: Get base concentration levels --------------------------------------------------------
conc_base <- creahia::get_conc_base(species=species, grid_raster=grid_raster)

# 03: Combine and flatten: one row per scenario --------------------------------------------
concs <- creahia::combine_concs(conc_coal_only, conc_base) %>% flatten_concs() %>% add_pop()

# 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
adm <- creahia::get_map_adm(grid_raster, admin_level=0, res="coarse", iso3s=c("CHN", "PAK", "IND", "BGD"))
# adm <- adm[adm$country_id %in% c("CHN", "PAK", "IND", "BGD"),] # FOR DEBUG

# 05: Extract concentrations ---------------------------------------------------------------
conc_adm <- extract_concs_at_map(concs, adm)
c <- conc_adm$scenario1$CHN
weighted.mean(c[2,],c[3,])

save.image()

# 06: Compute hia --------------------------------------------------------------------------
hia <- creahia::compute_hia(conc_map=conc_adm, species=species, regions=adm,
                   scale_base_year=2015, scale_target_year=2025)

hia_table <- hia %>% make_hia_table() %T>% write_csv(file.path(results_dir, 'hia_table.csv'))


# 07: Compute and export economic costs ----------------------------------------------------
econ_costs <- compute_econ_costs(hia, results_dir=results_dir)


# 08: Compare with Lauri's results ----------------------------------------
hia <- read.csv("example_hsbc/hia_table.csv") %>% mutate(source="CREAHIA")
hia.lauri <- read.csv("example_hsbc/lauri/health impacts table by ADM 0 area.csv") %>%
  mutate(source="Lauri",
         region_id=GID_0) %>%
  rename(central=HSBC_central,
         high=HSBC_high,
         low=HSBC_low)

bind_rows(hia, hia.lauri) %>%
  filter(region_id %in% c("IND","CHN","PAK", "BGD")) %>%
  ggplot() +
  geom_bar(stat="identity", aes(region_id, central, fill=source), position="dodge") +
  facet_wrap(~Outcome, scales="free_y")
