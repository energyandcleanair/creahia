library(terra)
library(creahelpers)
library(dplyr)
library(creahia)

obs_map <- terra::rast('examples/example_ph_who/obs_map.tif')
baseline_map <- terra::rast('examples/example_ph_who/zero_map.tif')
perturbation_map <- obs_map - baseline_map
names(perturbation_map) <- names(obs_map)

hia_gbd2017 <- creahia::wrappers.compute_hia_two_images.default(
  perturbation_rasters = raster::stack(perturbation_map),
  baseline_rasters = raster::stack(baseline_map),
  administrative_iso3s = c("PHL"),
  administrative_level = 0,
  crfs_version = "C40",
  epi_version = "C40",
  valuation_version = "viscusi"
)


hia_gbd2019 <- creahia::wrappers.compute_hia_two_images.default(
  perturbation_rasters = raster::stack(perturbation_map),
  baseline_rasters = raster::stack(baseline_map),
  administrative_iso3s = c("PHL"),
  administrative_level = 0,
  crfs_version = "C40",
  epi_version = "gbd2019",
  valuation_version = "viscusi"
)


bind_rows(
  hia_gbd2017 %>% mutate(version='gbd2017'),
  hia_gbd2019 %>% mutate(version='gbd2019')
) %>%
  filter(estimate=='central') %>%
  group_by(Outcome, Cause, Pollutant, version) %>%
  dplyr::summarise(number=sum(number)) %>%
  ggplot() +
  geom_col(aes(version, number, fill=Pollutant)) +
  facet_wrap(~Outcome, scales='free_y')


hia_gbd2017_deaths <- hia_gbd2017 %>%
  filter(!double_counted,
         Outcome=='Deaths',
         estimate=='central') %>%
  pull(number) %>% sum()


hia_gbd2019_deaths <- hia_gbd2019 %>%
  filter(!double_counted,
         Outcome=='Deaths',
         estimate=='central') %>%
  pull(number) %>% sum()

# No double counting on deaths or YLL number
library(testthat)
testthat::expect(hia_gbd2017_deaths, 67000)
testthat::expect_gt(hia_gbd2017_deaths, 62000)


testthat::expect(hia_gbd2019_deaths, 67000)
testthat::expect_gt(hia_gbd2019_deaths, 62000)


saveRDS(hia, 'examples/example_ph_who/hia.RDS')
hia <- readRDS('examples/example_ph_who/hia.RDS')


# Test our valuation
hia_costs <- creahia::compute_econ_costs(hia, results_dir="examples/example_ph_who/", valuation_version="viscusi")

# Create table for report
hia_costs$cost_by_outcome %>%
  ungroup() %>%
  filter(cost_mn_currentLCU>0) %>%
  mutate(
    Outcome = Outcome.long,
    Number = sprintf('%s\n%s',
                     number,
                     CI_number),
    `PHP mn (2019 PHP)` = sprintf('%s\n%s',
                                  gsub('\\..*', '', cost_mn_currentLCU),
                                  gsub('\\..*', '', CI_mn_currentLCU)
                                  ),
    `Share of GDP` = sprintf('%s\n%s',
                             share_gdp,
                             CI_share_gdp)
    ) %>%
  arrange(desc(cost_mn_currentLCU)) %>%
  select(Outcome, Number, `PHP mn (2019 PHP)`, `Share of GDP`) %>%
  write_csv('examples/example_ph_who/formatted_hia.csv')



hia_costs <- creahia::compute_econ_costs(hia,
                                         results_dir="examples/example_ph_who/",
                                         valuation_version="viscusi",
                                         projection_years = c(2021, 2022, 2023, 2030),
                                         GDP_scaling = T)
View(hia_costs$cost_forecast)


