#' Get baseline concentrations for various species
#'
#' @param species
#' @param grid_raster
#' @param no2_min_incr
#' @param no2_targetyear
#'
#' @return a tibble with `species` (chr) and named `conc_baseline` (RasterLayer) columns
#' @export
#'
#' @examples
wrappers.get_conc_baseline <- function(species, grid_raster,
                                       no2_min_incr = NULL,
                                       no2_targetyear = 2019,
                                       pm25_to_pm10_ratio = .7) {
  avail_species <- c('no2', 'so2', 'pm25', 'tpm10', 'o3', 'o3_8h') # pollutants with available baseline

  species <- species %>% tolower() %>% subset(. %in% avail_species)

  conc <- lapply(species, function(spec) {
    if(spec == 'no2') {
      get_conc_baseline_no2(grid_raster = grid_raster,
                            no2_targetyear = no2_targetyear,
                            no2_min_incr = no2_min_incr)
    } else if(spec == 'so2'){
      grid_raster %>% setValues(10)
    } else if(spec == 'tpm10'){
      get_conc_baseline_pm25(target_year = no2_targetyear, grid_raster = grid_raster) %>%
        multiply_by(1 / pm25_to_pm10_ratio)
    } else if(spec == 'pm25'){
      get_conc_baseline_pm25(target_year = no2_targetyear, grid_raster = grid_raster)
    } else if(spec %in% c('o3', 'o3_8h')){
      get_conc_baseline_o3(grid_raster = grid_raster, species = spec)
    }
  }) %>% `names<-`(species)

  tibble(species = names(conc),
         conc_baseline = conc)
}


#' Generic HIA compute wrapper
#' Compute HIA wrapper: given a list of rasters that represent perturbation in one or several pollutants.
#'
#' @param obj generic object
#' @return
#' @export
#'
#' @examples
wrappers.compute_hia_two_images <- function(obj, ...) {
  UseMethod('wrappers.compute_hia_two_images')
}


#' Compute HIA wrapper: given a list of rasters that represent perturbation in one or several pollutants.
#'
#' @param perturbation_rasters
#' @param baseline_rasters
#' @param crfs_version
#' @param return_concentrations include the population-weighted concentrations by admin area in the results. In this case, the function returns a list.
#' @param pm2.5_to_pm10_ratio if the PM2.5 input data should be used to assess PM10 exposure, provide a ratio to use for calculation of baseline concentrations
#'
#' @return
#' @export
#'
#' @examples
wrappers.compute_hia_two_images.default <- function(perturbation_rasters,
                                                    baseline_rasters = NULL,
                                                    regions = NULL,
                                                    administrative_level = 1,
                                                    administrative_res = "low",
                                                    administrative_iso3s = NULL,
                                                    scenario_name = "scenario",
                                                    pop_year = NULL,
                                                    scale_base_year = 2019, # Deprecated
                                                    scale_target_year = NULL, # Deprecated
                                                    crfs_version = "default",
                                                    epi_version = "default",
                                                    # valuation_version = "default",
                                                    return_concentrations = F,
                                                    pm2.5_to_pm10_ratio = NULL,
                                                    diagnostic_folder = "diagnostic",
                                                    ...){

  # Fix inputs: if scale_base_year or scale_target_year is not null,
  # warn user
  if(!is.null(scale_base_year) | !is.null(scale_target_year)){
    if(is.null(pop_year)){
      pop_year <- coalesce(scale_target_year, scale_base_year)

      messages <- c(
        "scale_base_year and scale_target_year are deprecated. Use pop_year instead, as the year you",
        "want to scale the population to. The base year is now determined automatically based on available data.",
        "\npop_year set to", pop_year
      )
      warning(paste(messages, collapse = " "))
    }
  }

  # For now, creahelpers work with raster package. To ensure transition,
  # we become format agnostic for now.
  perturbation_rasters <- perturbation_rasters %>%
    creahelpers::to_raster() %>%
    creahelpers::unrasterstack()

  baseline_rasters <- baseline_rasters %>%
    creahelpers::to_raster() %>%
    creahelpers::unrasterstack()

  species <- names(perturbation_rasters)
  grid_raster <- perturbation_rasters[[1]] %>% raster::raster()

  conc_perturbation <- tibble(species = species,
                              conc_perturbation = perturbation_rasters,
                              scenario = scenario_name)

  # 02: Get base concentration levels --------------------------------------------------------
  if(is.null(baseline_rasters)) { # get base concentrations level if not already provided
    conc_baseline <- creahia::get_conc_baseline(species = species, grid_raster = grid_raster)
  } else {
    # Work with either RasterStack or list of rasters
    conc_baseline <- tibble(species = names(baseline_rasters), # already in the workflow section 02
                            conc_baseline = raster::as.list(raster::stack(baseline_rasters)))
  }

  if(!is.null(pm2.5_to_pm10_ratio)) {
    if(!('tpm10' %in% conc_perturbation$species)) {
      conc_perturbation <- conc_perturbation %>% filter(species == 'pm25') %>%
        mutate(species = 'tpm10') %>%
        bind_rows(conc_perturbation) # copy PM25 data
      conc_perturbation$conc_perturbation[[which(conc_perturbation$species == 'tpm10')]] <-
        conc_perturbation$conc_perturbation[[which(conc_perturbation$species == 'tpm10')]] %>%
        divide_by(pm2.5_to_pm10_ratio) # divide by the ratio
      names(conc_perturbation$conc_perturbation) <- conc_perturbation$species
      species <- conc_perturbation$species
    }

    if(!('tpm10' %in% conc_baseline$species)) {
      conc_baseline <- conc_baseline %>% filter(species == 'pm25') %>%
        mutate(species = 'tpm10') %>%
        bind_rows(conc_baseline) # copy PM25 data
      conc_baseline$conc_baseline[[which(conc_baseline$species == 'tpm10')]] <-
        conc_baseline$conc_baseline[[which(conc_baseline$species == 'tpm10')]] %>%
        divide_by(pm2.5_to_pm10_ratio) # divide by the ratio
      names(conc_baseline$conc_baseline) <- conc_baseline$species
    }
  }

  # 03: Combine and flatten: one row per scenario --------------------------------------------
  concs <- creahia::combine_concs(conc_perturbation, conc_baseline) %>% # combine table
    creahia::flatten_concs() %>% # long to wide
    creahia::add_pop(grid_raster, year_desired=pop_year)

  # Extract species from the processed data
  species <- names(concs) %>%
    grep("^conc_scenario_|^conc_baseline_", ., value = TRUE) %>%
    gsub("^conc_scenario_|^conc_baseline_", "", .) %>%
    unique()

  # 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
  if(is.null(regions)) {
    regions <- creahia::get_adm(grid_raster,
                                      admin_level = administrative_level,
                                      res = administrative_res,
                                      iso3s = administrative_iso3s)
  }

  # 05: Extract concentrations ---------------------------------------------------------------
  conc_regions <- creahia::extract_concs_and_pop(concs, regions, species)

  # 06: Compute hia --------------------------------------------------------------------------
  hia <- creahia::compute_hia(conc_map = conc_regions,
                              species = species,
                              regions = regions,
                              pop_year = pop_year,
                              epi_version = epi_version,
                              crfs_version = crfs_version,
                              diagnostic_folder = diagnostic_folder,
                              ...)

  if(return_concentrations) {
    conc_regions_mean <- conc_regions %>%
      lapply(function(x){
        x %>% subset(!is.null(x)) %>%
          lapply(as_tibble) %>%
          bind_rows(.id = 'region_id')
      }) %>%
      bind_rows(.id = 'scenario') %>%
      group_by(scenario, region_id) %>%
      summarise(across(-pop, weighted.mean, w = pop, na.rm = T),
                across(pop, sum, na.rm = T))

    hia <- list(hia = hia, concentrations = conc_regions_mean)
  }
  # hia_table <- hia %>% totalise_hia() %>% make_hia_table()

  return(hia)
}


#' Compute HIA wrapper: given the scenarios and a data frame that represent perturbation in one or several pollutants.
#'
#' @param scenarios different scenarios to process.
#' @param perturbation_rasters_table the table from creapuff::get_calpuff_files function or similar.
#' @param baseline_rasters_table the table from wrapper.get_conc_baseline function.
#' @param grid_raster grid raster for the model.
#' @param crfs_version version of the CRFs to use.
#' @param return_concentrations include the population-weighted concentrations by admin area in the results. In this case, the function returns a list.
#' @param output_folder folder to save the results.
#'
#' @return
#' @export
#'
#' @examples
wrappers.compute_hia_two_images.character <- function(scenarios,
                                                      perturbation_rasters_table,
                                                      baseline_rasters_table,
                                                      grid_raster,
                                                      regions = NULL,
                                                      pop_year = NULL,
                                                      scale_base_year = 2019,
                                                      scale_target_year = NULL, # No scaling by default
                                                      crfs_version = "default",
                                                      epi_version = "default",
                                                      # valuation_version = "default",
                                                      return_concentrations = F,
                                                      output_folder = '.',
                                                      diagnostic_folder = "diagnostic",
                                                      ...){

  # Fix inputs: if scale_base_year or scale_target_year is not null,
  # warn user
  if(!is.null(scale_base_year) | !is.null(scale_target_year)){
    pop_year <- ifelse(is.null(pop_year), scale_target_year, pop_year)
    messages <- c(
      "scale_base_year and scale_target_year are deprecated. Use pop_year instead, as the year you",
      "want to scale the population to. The base year is now determined automatically based on available data.",
      "\npop_year set to", pop_year
    )
    warning(paste(messages, collapse = " "))
  }

  pollutants_for_hia <- intersect(perturbation_rasters_table$species,
                                  baseline_rasters_table$species)

  # custom_glue: to provide custom name for HIA .RDS files
  sapply(scenarios, function(scen, custom_glue = NULL, ...) {
    message('✅ Processing scenario: "', scen, '"')

    # 01: Get perturbation concentration levels for the scenario ----
    conc_perturbation <- perturbation_rasters_table %>%
      filter(scenario == scen, species %in% pollutants_for_hia) %>%
      rowwise() %>%
      mutate(conc_perturbation = list(species = raster(path))) %>%
      ungroup() %>%
      select(species, conc_perturbation, scenario)

    # 02: Combine perturbation and baseline, then flatten: one row per scenario ----
    concs <- creahia::combine_concs(conc_perturbation, baseline_rasters_table) %>% # combine table
      creahia::flatten_concs() %>% # long to wide
      creahia::add_pop(grid_raster, year_desired=pop_year)

    # 03: Extract concentrations ----
    conc_regions <- creahia::extract_concs_and_pop(concs, regions, pollutants_for_hia)

    # 04: Compute hia ----
    hia <- creahia::compute_hia(conc_map = conc_regions,
                                species = pollutants_for_hia,
                                regions = regions,
                                pop_year = pop_year,
                                epi_version = epi_version,
                                crfs_version = crfs_version,
                                diagnostic_folder = diagnostic_folder,
                                ...)

    if(return_concentrations) {
      conc_regions_mean <- conc_regions %>%
        lapply(function(x){
          x %>% subset(!is.null(x)) %>%
            lapply(as_tibble) %>%
            bind_rows(.id = 'region_id')
        }) %>%
        bind_rows(.id = 'scenario') %>%
        group_by(scenario, region_id) %>%
        summarise(across(-pop, weighted.mean, w = pop, na.rm = T),
                  across(pop, sum, na.rm = T))

      hia <- list(hia = hia, concentrations = conc_regions_mean)
    }

    dir.create(file.path(output_folder, 'hia'), showWarnings = F)
    saveRDS(hia,
            file.path(output_folder, 'hia',
                      glue(if(!is.null(custom_glue)) custom_glue else 'hia_GBD__{scen}.RDS')))
  }, ...)
}

