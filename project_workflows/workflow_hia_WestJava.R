# For development only
library(raster)
library(sf)
library(readxl)
library(zoo)
library(magrittr)
library(lubridate)

library(creahia)
library(creapuff)
require(rcrea)
require(creahelpers)


# change the following ----
project_dir <- "C:/Users/danny/Desktop/CREA Github/creahia" # project data location

input_dir <- file.path(project_dir, "sample_data", 'updated_TIFs') # where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"sample_data", 'updated_TIFs'); if(!dir.exists(output_dir)) dir.create(output_dir) # where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

gis_dir <- get_gis_dir()

pollutants_to_process <- c('NO2', 'PM2.5', 'PM10', 'SO2')

# Load CALMET parameters
calmet_result <- readRDS(file.path(input_dir, "calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN # UTM zone
UTMH <- calmet_result$params[[01]]$UTMHEM # UTM hemisphere

#list CALMET csvs
calpuff_files <- creapuff::get_calpuff_files(ext = ".csv", gasunit = 'ug',
                                             dir = input_dir, hg_scaling = 1e-3)
grids <- creapuff::get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res = 1)
grid_raster <- grids$gridR

# make tifs
# TODO Danny - add outline to the resulting plots
scenarios_to_process <- c('base', 'complian', 'bat', 'baseh')
calpuff_files %>%
  filter(scenario %in% scenarios_to_process, period == 'annual',
         speciesName %in% pollutants_to_process) %>%
  creapuff::make_tifs(grids = grids, overwrite = T)


# 02: Get base concentration levels -------------------------------------------------------------
# same code in wrapper.compute_hia_two_images in section "04"
conc_base <- creahia::get_conc_baseline(species = unique(calpuff_files$species),
                                        grid_raster = grid_raster,
                                        no2_targetyear = 2020) # creapuff # 2020 # Target year of model simulations (CALPUFF and WRF)
saveRDS(conc_base, 'cached_data/conc_base.RDS')
conc_base <- readRDS('cached_data/conc_base.RDS')

conc_base <- conc_base %>% filter(species=='pm25') %>%
  mutate(conc_baseline = lapply(conc_baseline, multiply_by, 1/.7),
         species = 'tpm10') %>%
  bind_rows(conc_base)

names(conc_base$conc_baseline) <- conc_base$species


# 03: Create support maps (e.g. countries, provinces, cities ) ----------------------------------
#regions <- creahia::get_adm(grid_raster, admin_level=2, res="low")
shp <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_low.RDS'))
regions <- creahia::get_adm(grid_raster, shp = shp, admin_level = 2)


calpuff_files_all <- creapuff::get_calpuff_files(ext = ".tif", gasunit = 'ug',
                                                 dir = input_dir, hg_scaling = 1e-3) %>%
  filter(scenario %in% scenarios_to_process)


runs <- calpuff_files_all$scenario %>% unique
queue <- T

causes_to_include = creahia::get_calc_causes() %>% grep('Death|YLD', ., value = T)

# HIA ###########################################################################################
#require(doFuture)
#registerDoFuture()
#future::plan("multisession", workers = 4)
#Sys.setenv(GIS_DIR='F:/gis')

#foreach (scen = runs[queue]) %dopar% ({

for(scen in runs[queue]) {
  message(scen)
  # =============================== Get Perturbation Raster ========================================
  conc_perturbation <- calpuff_files_all  %>%
    filter(scenario == scen, period == 'annual', speciesName %in% pollutants_to_process)

  conc_perturbation$conc_perturbation <- lapply(conc_perturbation$path, raster)
  names(conc_perturbation$conc_perturbation) <- conc_perturbation$species

  pollutants_for_hia <- intersect(conc_perturbation$species, conc_base$species) # %>% c('tpm10')


  # 04: HIA Calculations:
  # TODO : add no2_targetyear as input parameter in the wrapper, to calculate conc_base in the wrapper?
  # TODO : change name no2_targetyear -> no2_target_year
  # TODO : change name scale_base_year -> pop_base_year
  # TODO : change value for scale_base_year from 2020 -> 2019, as default in wrapper
  # TODO : change name scale_target_year -> pop_target_year
  hia <-  wrappers.compute_hia_two_images(perturbation_rasters = conc_perturbation$conc_perturbation[pollutants_for_hia],       # perturbation_rasters=raster::stack(perturbation_map)
                                          baseline_rasters = conc_base$conc_baseline,  # baseline_rasters=raster::stack(who_map)
                                          regions = regions,
                                          scenario_name = scen,
                                          scale_base_year = 2019,        # Population base year : reference year of INPUT data, for total epidemiological and total population
                                          scale_target_year = 2022,      #Population target year
                                          crfs_version = "C40",
                                          epi_version = "C40",       # epi_version="C40"
                                          valuation_version = "viscusi",
                                          return_concentrations = T,
                                          gbd_causes = 'default',
                                          calc_causes = causes_to_include,
                                          pm2.5_to_pm10_ratio = .7
  ) # valuation_version="viscusi"

  # TODO Danny - add create folder if not exist
  saveRDS(hia, file.path(project_dir, 'HIA', paste0('hia_GBD__', scen, '.RDS')))
}


#read HIA data
hia <- runs %>%
  lapply(function(scen) readRDS(file.path(project_dir, 'HIA',
                                          paste0('hia_GBD__', scen, '.RDS')))$hia) %>%
  bind_rows

#select admin regions to include
#adm <- creahelpers::get_adm(level = 2, res='coarse')
adm <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_2_coarse.RDS'))

hia_totals <- hia %>%
  left_join(adm@data %>% dplyr::select(region_id = GID_2, starts_with('NAME'))) %>%
  group_by(across(c(starts_with('NAME'), Outcome, Pollutant, Cause, AgeGrp, iso3,
                    scenario, estimate, double_counted))) %>%
  summarise(across(number, sum)) %>%
  filter(Pollutant != 'PM25' | Cause != 'AllCause') %>%
  mutate(number = number * case_when(Pollutant != 'NO2' | Cause != 'AllCause' ~ 1,
                                     estimate == 'central' ~ 1/2,
                                     estimate == 'low' ~ 1/2,
                                     estimate == 'high' ~ 2/3))


hia_totals %>% group_by(scenario, Pollutant) %>%
  filter(!double_counted, Outcome == 'Deaths', estimate == 'central') %>%
  summarise(across(number, sum, na.rm = T))


# 06: Compute and extract economic costs --------------------------------------------------------
targetyears <- 2022

hia_cost <- creahia::get_hia_cost(hia = hia_totals, valuation_version = "viscusi")

#valuations <- read_csv('~/Rpackages/creahia/inst/extdata/valuation_viscusi.csv')
valuations <- get_valuations_raw('viscusi')

usd_to_lcu <- 15447

hia_cost %>%
  distinct(Outcome, valuation_world_2017, valuation_current_usd, iso3) %>%
  left_join(valuations %>% select(Outcome, reference)) %>%
  na.omit %>%
  add_long_names() %>%
  select(-Outcome, Outcome = Outcome_long) %>%
  mutate(valuation_current_lcu = valuation_current_usd * usd_to_lcu,
         across(is.numeric, function(x) x %>% signif(4) %>% scales::comma(accuracy = 1))) %>%
  relocate(Outcome) %>%
  relocate(reference, .after = everything()) %>%
  write_csv(file.path(output_dir, 'valuations.csv'))


hia_fut <- hia_cost %>% creahia::get_econ_forecast(forecast_years = targetyears, reference_year = 2019)

hia_totals <- hia_fut %>% creahia::add_long_names() %>%
  group_by(Outcome = Outcome_long, Cause = Cause_long, Pollutant, double_counted, scenario, estimate) %>%
  mutate(across(cost_mn_currentLCU, divide_by, 1000)) %>%
  rename(cost_bn_currentLCU = cost_mn_currentLCU) %>%
  summarise(across(c(number, starts_with('cost')), sum, na.rm = T))

hia_totals %>% filter(!double_counted) %>%
  group_by(scenario, estimate) %>%
  summarise(across(starts_with('cost'), sum, na.rm = T)) %>%
  pivot_longer(is.numeric, names_to = 'Outcome', values_to = 'number') %>%
  bind_rows(hia_totals) %>%
  select(-starts_with('cost')) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(names_from = estimate, values_from = number) %>%
  write_csv(file.path(output_dir, 'HIA results.csv'))

# HIA ends here ----

plotdata <- hia_fut %>%
  filter(Outcome == 'Deaths', !double_counted, estimate == 'central') %>%
  group_by(scenario, province = NAME_1, year) %>% # added year to the group
  summarise(across(c(number, cost_mn_currentUSD), sum))


label_pos <- plotdata %>% group_by(province, scenario) %>%
  summarise(max_value = max(number),
            line_start = number[year == 2022]) %>%
  arrange(line_start) %>%
  ungroup %>%
  mutate(max_value = max(max_value)) %>%
  group_by(province) %>%
  mutate(line_end = max_value * (1.5 + seq_along(scenario)) / 7.5) %>%
  pivot_longer(starts_with('line'), values_to = 'number') %>%
  mutate(year = ifelse(name == 'line_start', 2032, 2050)) %>%
  filter(province == 'Gauteng')


p <- plotdata %>%
  ggplot(aes(year, number, col = scenario)) +
  facet_wrap(~province) +
  geom_line(size = 1) +
  geom_label(aes(label = scenario), data = label_pos %>% filter(name == 'line_end'), hjust = 0, size = 3) +
  geom_line(data = label_pos) +
  theme_crea(legend.position = 'top') +
  scale_color_manual(values = scenario_colors,
                     guide = guide_legend(nrow = 1, override.aes = list(label = '', linewidth = 1))) +
  labs(title = 'Deaths attributed to Eskom emissions by province',
       y = 'cases per year', x = '') +
  snug_x + x_at_zero()
quicksave(file.path(output_dir, 'deaths by province and scenario.png'), plot = p)

plotdata <- hia_scen %>%
  filter(Outcome == 'Deaths', !double_counted, estimate == 'central',
         region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T) %>%
  group_by(year, scenario) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum))


label_pos <- plotdata %>% group_by(scenario) %>%
  summarise(max_value = max(number),
            line_start = number[year == 2032]) %>%
  arrange(line_start) %>%
  mutate(line_end = max_value * (1.5 + seq_along(scenario)) / 7.5) %>%
  pivot_longer(starts_with('line'), values_to = 'number') %>%
  mutate(year = ifelse(name == 'line_start', 2032, 2050))


p <- plotdata %>%
  ggplot(aes(year, number, col = scenario)) +
  geom_line(size = 2) +
  geom_label(aes(label = scenario), data = label_pos %>% filter(name == 'line_end'), hjust = 0, size = 5) +
  geom_line(data=label_pos, size = 1) +
  theme_crea(legend.position = 'top') +
  scale_color_manual(values = scenario_colors,
                     guide = guide_legend(nrow = 1, override.aes = list(label = '', linewidth = 1))) +
  labs(title = 'Deaths attributed to Eskom emissions',
       y = 'cases per year', x = '') +
  snug_x + x_at_zero()
quicksave(file.path(output_dir, 'deaths by scenario.png'), plot = p)


p <- hia_scen %>%
  filter(Outcome == 'Deaths', !double_counted, estimate == 'central',
         region_id %in% c('Mpumalanga', 'Gauteng', 'Limpopo') | T,
         year >= 2025) %>%
  group_by(scenario, province = region_id) %>%
  summarise(across(c(number, cost_mn_currentUSD), sum)) %>%
  ggplot(aes(scenario, number, fill = scenario)) + geom_col() + facet_wrap(~province) +
  theme_crea(legend.position = 'top', axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = scenario_colors, guide = guide_legend(nrow = 1)) +
  x_at_zero(labels = scales::comma) +
  labs(title = 'Cumulative deaths attributed to Eskom emissions',
       subtitle = '2025 until end-of-life',
       y = 'cases', x = '')
quicksave(file.path(output_dir, 'cumulative deaths by province and scenario.png'), plot = p)


# TODO Danny - add this as official creahia package function?
make_nice_numbers <- function(df, sigdigs = 3, accuracy = 1, columns = c('number', 'central', 'low', 'high')) {
  df %>% mutate(across(any_of(columns),
                       function(x) {
                         x <- x %>% signif(sigdigs)
                         ifelse(grepl('mln|bln', Outcome) & !grepl('USD', Outcome),
                                scales::comma(x, accuracy = accuracy / 100),
                                scales::comma(x, accuracy = accuracy))
                       }))
}

output_tables <- function(hiadata, output_name = '', rounding_function = make_nice_numbers,
                          bad_scenario = 'Eskom plan', good_scenario = 'compliance') {
  hiadata <- hiadata %>% filter(!double_counted, !grepl('YLLs|LBW', Outcome)) %>%
    add_long_names() %>%
    select(-Outcome, -Cause) %>%
    rename(Outcome = Outcome_long, Cause = Cause_long)

  cost_totals <- hiadata %>% group_by(scenario, estimate) %>%
    summarise(across(cost_mn_currentUSD, sum, na.rm = T)) %>%
    rename(number = cost_mn_currentUSD) %>%
    mutate(Outcome = 'total economic cost, mln USD',
           Pollutant = 'all', Cause = 'all', double_counted = F)

  death_totals <- hiadata %>% filter(grepl('deaths', Outcome)) %>%
    group_by(scenario, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(Outcome = 'deaths',
           Pollutant = 'all', Cause = 'all causes', double_counted = F)

  yld_totals <- hiadata %>% filter(grepl('disabi', Outcome)) %>%
    group_by(scenario, Outcome, Pollutant, estimate) %>%
    summarise(across(number, sum)) %>%
    mutate(Cause = 'all causes', double_counted = F)

  pm25_death_totals <- hiadata %>% filter(grepl('deaths', Outcome), Pollutant == 'PM2.5') %>%
    group_by(scenario, estimate, Pollutant) %>%
    summarise(across(number, sum)) %>%
    mutate(Outcome = 'deaths', Cause = 'all causes', double_counted = T)

  hiadata$double_counted[grepl('disabi', hiadata$Outcome)] <- T

  hia_out <- hiadata %>%
    filter(!grepl('prevalence', Outcome)) %>%
    mutate(double_counted = grepl('deaths', Outcome),
           across(Cause, tolower)) %>%
    group_by(scenario, Outcome, Cause, Pollutant, double_counted, estimate) %>%
    summarise(across(number, sum)) %>%
    bind_rows(cost_totals,
              pm25_death_totals,
              death_totals,
              yld_totals)

  hia_out <- hia_out %>% mutate(number = number * ifelse(grepl('absence', Outcome), 1e-6, 1),
                                Cause = ifelse(Outcome == Cause, '', Cause),
                                Outcome = ifelse(grepl('absence', Outcome), 'work absence (mln sick leave days)', Outcome))

  if(good_scenario == 'nocoal')
    hia_out <- hia_out %>% filter(scenario == 'Eskom plan') %>%
    mutate(number = 0, scenario = 'nocoal') %>%
    bind_rows(hia_out)

  hia_out <- hia_out %>% filter(grepl('economic', Outcome)) %>%
    mutate(number = number * usd_to_lcu / 1000,
           Outcome = 'total economic cost, bln R') %>%
    bind_rows(hia_out)

  hia_avoided <- hia_out %>%
    group_by(Outcome, Cause, Pollutant, estimate, double_counted) %>%
    summarise(number = number[scenario == bad_scenario] - number[scenario == good_scenario]) %>%
    spread(estimate, number) %>%
    arrange(!grepl('deaths', Outcome), !grepl('asthma', Outcome),
            !grepl('births', Outcome), grepl('economic', Outcome),
            Outcome, Pollutant != 'all', Pollutant != 'PM2.5', double_counted,
            Cause, Pollutant) %>%
    select(Outcome, Cause, Pollutant, central, low, high, double_counted) %>%
    filter(!is.na(Outcome))


  hia_avoided %>%
    filter(!(Pollutant == 'PM2.5' & Cause == 'all causes')) %>%
    rounding_function %>%
    write_csv(file.path(output_dir,
                        glue::glue('avoided health impacts, {good_scenario} vs ',
                                   '{bad_scenario}, {output_name}.csv')))

  hia_out %>% ungroup %>% filter(Pollutant == 'all') %>%
    rounding_function %>%
    spread(estimate, number) %>%
    arrange(Outcome, scenario) %>%
    select(scenario, Outcome, central, low, high) %T>%
    print() %>%
    write_csv(file.path(output_dir,
                        glue::glue('deaths and total costs, all scenarios, {output_name}.csv')))

  hia_avoided <- hia_avoided %>% rounding_function(sigdigs=2, accuracy=10)

  statements <- character()

  statements <- hia_avoided %>% filter(!(Outcome == 'deaths' & Cause != 'all causes')) %>%
    mutate(statement = case_when(Outcome == 'deaths' & Pollutant == 'all' ~
                                   glue::glue('{central} {Outcome} (95% confidence interval: {low} - {high}), of which ')
                                 grepl('disabi', Outcome) & Cause == 'all causes' ~
                                   glue::glue('{central} {Outcome} of which '),
                                 Outcome == 'deaths' & Cause == 'all causes' ~
                                   glue::glue('{central} due to exposure to {Pollutant}, '),
                                 grepl('disability', Outcome) ~
                                   glue::glue('{central} due to {Cause}, '),
                                 grepl('USD', Outcome) ~
                                   glue::glue('total economic costs of ${central} mln'),
                                 grepl('absence', Outcome) ~
                                   glue::glue('{central} million days of work absence'),
                                 T ~ glue::glue('{central} {Outcome}, '))) %>%
    use_series(statement) %>%
    paste(collapse = '') %>%
    c(statements, .)

  statements <- hia_avoided %>% filter(Outcome == 'deaths' & Cause != 'all causes') %>%
    (function(df) {
      c(glue::glue('Of the deaths caused by PM2.5 exposure, {df$central[1]} are attributed to {df$Cause[1]}, ',
                   '{df$central[-1]} to {df$Cause[-1]}') %>% paste(collapse = ', '))
    }) %>% c(statements, .)

  statements <- hia_out %>% filter(Pollutant == 'all') %>%
    group_by(Outcome, estimate) %>%
    mutate(number = number[scenario == bad_scenario] - number) %>%
    spread(estimate, number) %>%
    filter(central > 0) %>%
    make_nice_numbers() %>%
    arrange(!grepl('deaths', Outcome)) %>%
    group_by(scenario) %>%
    summarise(statement = glue::glue('the {scenario[1]} scenario would avoid a projected {central[1] ',
                                     'deaths from air pollution (95% confidence interval: ',
                                     '{low[1]} – {high[1]}) and economic costs of USD {central[2]} ',
                                     'million (95% confidence interval: {low[2]} - {high[2]})'),
              central = central[1]) %>%
    arrange(central) %>%
    use_series(statement) %>%
    paste(collapse = '; ') %>%
    paste('Compared to the', bad_scenario, 'scenario,', .) %>%
    c(statements, .)

  statements %>%
    c(paste('Compared to the', bad_scenario, 'scenario, the', good_scenario, 'scenario would avoid...'), .) %T>%
    print() %>%
    writeLines(file.path(output_dir, paste0(good_scenario, ' vs ', bad_scenario, ', ', output_name, '.txt')))
}


