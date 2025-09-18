testthat::skip("Needs exposure data that is not (yet?) available on GitHub actions")

# This is a comparison with a literature article that also uses GEMM with von Donkelar


test_that("Our GEMM-derived deaths are similar to literature", {

  library(rnaturalearth)
  library(sf)

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

  # creahelpers::reload_packages("creaexposure")

  library(testthat)
  library(terra)
  library(tidyverse)
  library(wbstats)

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



  res <- creaexposure::RES_2PT5_MIN
  pop <- creaexposure::data.pop(res=res)
  grid <- pop %>% rast()
  adm_res <- "low"
  # Replace creahelpers::get_adm with rnaturalearth - get all countries
  adm <- rnaturalearth::ne_countries(scale = if(adm_res == "low") "medium" else "large", returnclass = "sf")
  years <- unique(validation$year)

  # We compute one HIA for all countries at once
  hias <- lapply(years, function(year){

    print(year)

    # Get PM2.5 exposure map from van Donkelaar
    pm25 <- creaexposure::data.basemap_pm25(year=year, res=res, pop=grid)
    baseline_rasters <- list(pm25=pm25 %>% `values<-`(0),
                             no2=pm25 %>% `values<-`(0))
    perturbation_rasters <- list(pm25=pm25,
                                 no2=baseline_rasters$no2)

    # Get global population-weighted average PM2.5
    global_extent <- terra::ext(grid)
    global_vect <- terra::vect(global_extent)
    global_vect$GID_0 <- "Global"
    adm_w_global <- rbind(global_vect, terra::vect(adm))


    pm25_pwavg <- creahelpers::to_rast(list(pm25=pm25, pop=pop)) %>%
      terra::rast() %>%
      terra::extract(adm_w_global, weights=T) %>%
      mutate(pop=pop*weight) %>%
      mutate(region_id=adm_w_global$GID_0[ID]) %>%
      group_by(region_id) %>%
      summarise(value=weighted.mean(pm25, w=pop, na.rm=T))


    # Computing HIA
    hia <- creahia::wrappers.compute_hia_two_images.default(
      perturbation_rasters = perturbation_rasters,
      baseline_rasters = baseline_rasters,
      scale_base_year = year,
      crfs_version = "C40",
      epi_version = "gbd2019",
      administrative_level = 0,
      administrative_res = adm_res,
      rr_sources = c(RR_GEMM),
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
})



  # Recompute things to save time



  # Comparison
  comparison_long <- bind_rows(purrr::map(hias, "deaths")) %>%
    rename(deaths=number) %>%
    mutate(source="CREA (GBD 2019)") %>%
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





  # test that there is no duplicate scenario,region_id,estimate,Outcome,Pollutant,Cause,AgeGrp
  n_duplicates <- hia_gbd2019 %>%
    group_by(scenario, region_id, estimate, Outcome, Pollutant, Cause, AgeGrp) %>%
    summarise(n=n(), pop=n_distinct(pop), number=n_distinct(number)) %>%
    arrange(desc(n)) %>%
    filter(n>1)

  expect_equal(nrow(n_duplicates), 0)


  # Compare deaths with validation data
  validation <- bind_rows(
    hia_gbd2019 %>% mutate(source="CREA (GBD 2019)", year=2019),
    hia_gbd2017 %>% mutate(source="CREA (GBD 2017)", year=2019)
  ) %>%
    filter(!double_counted,
           Pollutant=="PM25",
           grepl("Deaths", Outcome),
           !is.na(number),
           estimate=="central") %>%
    group_by(source, year) %>%
    summarise(total=sum(number)) %>%
    bind_rows(
      tibble(source="Li, van Donkelaar et al. (2023)",
             total=5.74e6,
             year=2019)
    )

  # Check deaths are within 5%
  expect_equal(validation$total[1], validation$total[2], tolerance=0.05)
  expect_equal(validation$total[1], validation$total[3], tolerance=0.05)


  ggplot(validation, aes(x=source, y=total)) +
    geom_col(aes(fill=source)) +
    geom_text(aes(label=scales::comma(total, 1)), vjust=-0.5) +
    labs(title="Global premature deaths from PM2.5 exposure for the year 2019",
         subtitle="Exposure map is van Donkelaar V5.GL.03 for the year 2019",
         y=NULL,
         x=NULL) +
    facet_wrap(~year) +
    rcrea::scale_fill_crea_d()

  # Check population is within 1%
  pop_2019 <- hia_gbd2019 %>%
    distinct(region_id, pop) %>%
    pull(pop) %>%
    sum()

  expect_equal(pop_2019, 7.7e9, tolerance=0.01)

})
