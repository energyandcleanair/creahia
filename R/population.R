# Population related functions
# This should all move to creahelpers at some point

#' Return years for which the population is available
#'
#' @return
#' @export
#'
#' @examples
get_pop_years_available <- function(){
  # extract year from gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif right after adjusted_to
  list.files(creahelpers::get_population_path(), pattern = '.*tif$') %>%
    stringr::str_extract('(?<=unwpp_country_totals_rev11_)[0-9]+') %>%
    .[!is.na(.)] %>%
    unique() %>%
    as.numeric()
}



#' Returns the closest available population year to the desired year
#'
#' @param desired_year
#'
#' @return
#' @export
#'
#' @examples
get_pop_year <- function(year_desired){
  years_available <- get_pop_years_available()
  years_available[which.min(abs(years_available - year_desired))]
}



#' Get population SpatRaster for the closest year available to the desired year
#'
#' @param grid_raster
#' @param year_desired
#'
#' @return
#' @export
#'
#' @examples
get_pop_count <- function(grid_raster, year_desired=2020) {

  year <- get_pop_year(year_desired)
  filename <- glue("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_{year}_30_sec.tif")
  filepath <- creahelpers::get_population_path(filename)
  if(!file.exists(filepath)) {
    stop(sprintf("Can't find population file: %s", filepath))
  }

  pop_density <- terra::rast(filepath)

  # Reproject if grid_raster provided
  if(!is.null(grid_raster)) pop_density <- project(pop_density, terra::rast(grid_raster))

  pop_density[is.na(pop_density)] <- 0

  # Getting pop in million per cell
  pop_mn <- pop_density %>% multiply_by(terra::cellSize(pop_density, unit='km', transform=T))

  # Add year attribute, to be used later for scaling
  attr(pop_mn, 'year') <- year
  return(pop_mn)
}


get_pop_proj <- function() {
  # Read from included CSV
  get_hia_path("population/WPP2024-population-deathrate.csv") %>%
    read_csv()
}


generate_pop_proj <- function(){
  pop_url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"
  death_url1 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_DeathsBySingleAgeSex_Medium_1950-2023.csv.gz"
  death_url2 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_DeathsBySingleAgeSex_Medium_2024-2100.csv.gz"

  # Robust reader for WPP gzipped CSVs
  read_wpp_csv <- function(url){
    filename <- basename(url)
    tf <- tempfile(pattern = filename, fileext = ".gz")
    if(!file.exists(tf)){
      httr::GET(url, httr::write_disk(tf, overwrite = TRUE), httr::timeout(120))
    }
    try(suppressWarnings(readr::read_csv(tf, show_col_types = FALSE, progress = FALSE, guess_max = 100000)), silent = TRUE)
  }

  # Read from gz URLs (with fallbacks)
  pop_raw <- read_wpp_csv(pop_url)
  deaths1_raw <- read_wpp_csv(death_url1)
  deaths2_raw <- read_wpp_csv(death_url2)

  # Standardize column names to lower case for easier handling
  to_lower <- function(df){ names(df) <- tolower(names(df)); df }
  pop <- pop_raw %>%
    select(
           iso3=ISO3_code,
           location_id=LocID,
           location_name=Location,
           age_group=AgeGrp,
           variant=Variant,
           year=Time,
           age_start=AgeGrpStart,
           age_span=AgeGrpSpan,
           pop=PopTotal)

  deaths <- dplyr::bind_rows(deaths1_raw, deaths2_raw) %>%
    select(
           iso3=ISO3_code,
           location_id=LocID,
           age=AgeGrp,
           variant=Variant,
           year=Time,
           deaths=DeathTotal
        )

  # Combine by age group
  ages <- pop %>%
    distinct(age_group, age_start, age_span) %>%
    rowwise() %>%
    mutate(age=case_when(age_group=="100+" ~ list(100),
                       TRUE ~ list(seq(age_start, age_start + age_span - 1)))) %>%
    distinct(age_group, age) %>%
    tidyr::unnest(age) %>%
    mutate(age=as.character(age))

  deaths_by_group <- deaths %>%
    left_join(ages, by=c("age"="age"), multiple="all") %>%
    group_by(iso3, location_id, year, age_group) %>%
    summarise(deaths=sum(deaths, na.rm=TRUE), .groups="drop")

  # Join population and deaths
  final <- pop %>%
    left_join(deaths_by_group, by=c("iso3", "location_id", "year", "age_group")) %>%
    mutate(
      death_rate = dplyr::if_else(pop > 0, deaths / pop, NA_real_),
      birth_rate = NA_real_
    ) %>%
    filter(variant=="Medium") %>%
    select(iso3, location_id, location_name, age_group, age_start, age_span, year, pop, deaths, death_rate) %>%
    filter(!is.na(iso3))

  # 100+ doesn't have death rate. Assume similar to 95-99
  final <- final %>%
    group_by(iso3, location_id, location_name, year) %>%
    arrange(age_start) %>%
    fill(death_rate, .direction="down") %>%
    mutate(deaths = ifelse(is.na(deaths) & !is.na(death_rate), pop * death_rate, deaths))  %>%
    ungroup()

  # Make it lighter
  final <- final %>%
    select(-location_name, -death_rate) %>%
    filter(year >= 2000)


  # Write to default output path for compatibility
  output_dir <- file.path("inst", "extdata", "population")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, "WPP2024-population-deathrate.csv")
  readr::write_csv(final, output_file)

  invisible(output_file)
}
