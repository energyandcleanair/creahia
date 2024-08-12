# This is a comparison with a literature article that also uses GEMM with von Donkelar

library(testthat)

# We use the following references for comparison, that both use GEMM:
#
# Reversal of trends in global fine particulate matter air pollution
# Li, van Donkelaar et al. 2023
# https://www.nature.com/articles/s41467-023-41086-z
#
# Year: 2019
# Mortality: 5.74 million deaths
# Uses van Donkelaar: yes
# Gloal average population-weighted average: 34.7 μg/m3


# Global estimates of mortality associated with long-term exposure to outdoor fine particulate matter
# Burnett et al. 2018
# https://www.pnas.org/doi/10.1073/pnas.1803222115
#
# Year: 2015
# Mortality: 8.9 million deaths
# Uses van Donkelaar: no
# India average population-weighted average: 74 μg/m3
# India premature deaths: 2,219,000



library(testthat)
library(terra)
library(tidyverse)
library(wbstats)
library(memoise)
library(glue)

sources <- list(
  "Li, van Donkelaar et al. (2023)" = " https://www.nature.com/articles/s41467-023-41086-z",
  "Burnett et al. (2018)" = "https://www.pnas.org/doi/10.1073/pnas.1803222115",
  "Zheng et al. (2021)" = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8647684",
  "Giani et al. (2020)" = "https://iopscience.iop.org/article/10.1088/1748-9326/ab7f0f",
  "Wu et al. (2021)" = "https://www.sciencedirect.com/science/article/abs/pii/S135223102030830X?via%3Dihub",
  "Liu et al. (2021)" = "https://www.sciencedirect.com/science/article/abs/pii/S0269749121004644?via%3Dihub",
  "Lelieveld et al. (2020)" = "https://academic.oup.com/cardiovascres/article/116/11/1910/5770885",
  "Yin et al. (2020)" = "https://www.medrxiv.org/content/10.1101/2020.04.28.20083576v1"
)

validation <- list(
  tibble(
    region_id="Global",
    source=c("Li, van Donkelaar et al. (2023)", "Burnett et al. (2018)"),
    deaths=c(5.74e6, 8.915e6),
    year=c(2019, 2015),
    pm25=c(34.7, NA)
    ),

  tibble(
    source=c("Burnett et al. (2018)"),
    region_id=c("IND", "CHN"),
    deaths=c(2.219e6, 2.470e6),
    year=2015,
    pm25=c(74.0, 57.5)
    ),

  tibble(
    source=c("Giani et al. (2020)"),
    region_id=c("CHN"),
    deaths=c(1.424e6),
    year=2018,
    pm25=NA
  ),

  tibble(
    source=c("Wu et al. (2021)"),
    region_id=c("CHN"),
    deaths=c(1.94e6),
    year=2015,
    pm25=NA
  ),

  tibble(
    source=c("Liu et al. (2021)"),
    region_id=c("CHN"),
    deaths=c(1.8e6),
    year=2017,
    pm25=NA
  ),

  tibble(
    source=c("Lelieveld et al. (2020)"),
    region_id=c("Global"),
    deaths=c(8.8e6),
    year=2015,
    pm25=NA
  ),

  tibble(
    source=c("Yin et al. (2020)"),
    region_id=c("Global"),
    deaths=c(8.42e6),
    year=2016,
    pm25=NA
  )
) %>%
  bind_rows()



bbox <- NULL #creaexposure::get_bbox(list("IN", "CN"))
res <- creaexposure::RES_2PT5_MIN
pop <- creaexposure::data.pop(res=res, bbox=bbox)
grid <- pop %>% rast()
adm_res <- "low"
adm <- creahelpers::get_adm(level=0, res=adm_res)
years <- unique(validation$year)
suffix <- ""
creahia_versions <- c("0.3.0", "0.3.1", "0.4.0", "0.5.0")


get_hia_deaths_pm <- function(year, res, grid, adm, adm_res, suffix, creahia_version){

  print(year)

  remotes::install_github(glue("energyandcleanair/creahia@{creahia_version}"), upgrade=F) # Before the fix
  creahelpers::reload_packages("creahia")
  if(!creahia_version == packageVersion("creahia")){
    stop("Version mismatch")
  }

  # Get PM2.5 exposure map from van Donkelaar
  pm25 <- creaexposure::data.basemap_pm25(year=year, res=res, pop=grid, use_cache = T, suffix = suffix)
  baseline_rasters <- list(pm25=pm25,
                           no2=pm25 %>% `values<-`(0))
  perturbation_rasters <- list(pm25=-pm25,
                               no2=baseline_rasters$no2)

  # Get global population-weighted average PM2.5
  global_extent <- terra::ext(grid)
  global_vect <- terra::vect(global_extent)
  global_vect$GID_0 <- "Global"
  adm_w_global <- rbind(global_vect, terra::vect(adm))


  pm25_pwavg <- NULL
  # pm25_pwavg <- creahelpers::to_rast(list(pm25=pm25, pop=pop)) %>%
  #   terra::rast() %>%
  #   terra::extract(adm_w_global, weights=T) %>%
  #   mutate(pop=pop*weight) %>%
  #   mutate(region_id=adm_w_global$GID_0[ID]) %>%
  #   group_by(region_id) %>%
  #   summarise(value=weighted.mean(pm25, w=pop, na.rm=T))


  # Computing HIA
  if(version %in% c("0.3.0", "0.3.1")){
    scale_base_year <- 2020
    scale_target_year <- if(year < 2020) year else NULL
  }else{
    scale_base_year <- creahia::get_pop_year(year)
    scale_target_year <- year
  }


  hia <- creahia::wrappers.compute_hia_two_images.default(
    perturbation_rasters = perturbation_rasters,
    baseline_rasters = baseline_rasters,
    scale_base_year = scale_base_year,
    scale_target_year = scale_target_year,
    crfs_version = "C40",
    epi_version = "gbd2019",
    administrative_level = 0,
    administrative_res = adm_res
  )

  # Extract deaths and population
  deaths <- hia %>%
    filter(!double_counted,
           Pollutant=="PM25",
           grepl("Deaths", Outcome),
           !is.na(number),
           estimate=="central") %>%
    group_by(region_id) %>%
    summarise(number=sum(number),
              pop=first(pop)) %>%
    # Add a Global row as sum of all
    bind_rows(tibble(region_id="Global",
                     number=sum(.$number),
                     pop=sum(.$pop))) %>%
    mutate(year=year,
           res=res)

  return(list(hia=hia, deaths=deaths, pm25_pwavg=pm25_pwavg))
}


# Create a memoise versions
get_hia_deaths_pm_memo <- memoise::memoise(get_hia_deaths_pm)


# We compute one HIA for all countries at once
lapply(creahia_versions, function(creahia_version){
  lapply(years, function(year){
    tryCatch({
      hia <- get_hia_deaths_pm_memo(year, res, grid, adm, adm_res, suffix, creahia_version)
      saveRDS(hia, glue("validation/hias_{year}_{creahia_version}.RDS"))
    }, error=function(e){
      message(glue("Error for {year} and {creahia_version}: {e$message}"))
    })
  })
})

names(hias) <- creahia_versions


memoise::has_cache(get_hia_deaths_pm_memo)(year, res, grid, adm, adm_res, creahia_version)

version <- packageVersion("creahia")


# List all files with a year and a version (four digits then d.d.d)
hias_files <- list.files("validation", "hias_\\d{4}_\\d+\\.\\d+\\.\\d+\\.RDS", full.names=T)

all_deaths <- lapply(hias_files, function(filepath){
  # extrqct version from filepath
  year <- as.numeric(stringr::str_extract(basename(filepath), "\\d{4}"))
  version <- stringr::str_extract(basename(filepath), "\\d+\\.\\d+\\.\\d+")
  hias <- readRDS(filepath)
  hias$deaths %>%
    mutate(version = version)
}) %>%
  bind_rows()

# Comparison
comparison_long <- all_deaths %>%
  rename(deaths=number) %>%
  mutate(source=glue("CREAHIA {version}")) %>%
  inner_join(validation %>% select(region_id, year)) %>%
  bind_rows(validation)


comparison_long %>%
  mutate(source = factor(source, levels=c("CREA (GBD 2019)", setdiff(unique(source), "CREA (GBD 2019)"))),
         deaths = deaths / 1e6) %>%
  ggplot(aes(x=region_id, y=deaths, group=source)) +
  geom_col(aes(fill=source), position="dodge") +
  geom_text(aes(label=scales::comma(deaths, 0.01)), vjust=-0.5, position = position_dodge(width=.9), size=3) +
  facet_wrap(~year) +
  rcrea::scale_fill_crea_d() +

  labs(title="Global premature deaths from PM2.5 exposure using GEMM",
       subtitle="million premature deaths",
       y=NULL,
       x=NULL)

# Get population
all_deaths %>%
  filter(region_id != "Global") %>%
  group_by(version, year) %>%
  summarise(pop=sum(pop)) %>%
  ggplot(aes(x=year, y=pop/1e9)) +
  geom_col(aes(fill=version), position="dodge") +
  geom_text(aes(label=scales::comma(pop, 0.01)), vjust=-0.5, position = position_dodge(width=.9), size=3) +
  rcrea::scale_fill_crea_d()
