#' Build concentration (additional) dataset from CALPUF results
#'
#' @param dir
#' @param utm_zone
#' @param utm_hem 'N' or 'S'
#' @param map_res in degrees??
#'
#' @return
#' @export
#'
#' @examples
get_conc_calpuff <- function(dir, utm_zone, utm_hem, map_res){

  calpuff_files <- getPuffCSVs(ext=".csv", gasunit = 'ug', dir=dir)

  grids <- get_grids_calpuff(calpuff_files=calpuff_files, utm_zone=utm_zone, utm_hem=utm_hem, map_res=map_res)
  # grids$gridR
  # grids$gridSP
  # grids$gridLL

  # Create tifs from csv results
  makeTifs(calpuff_files,
           grids=grids,
           nmax=8, idp=1.5,
           queue=which(calpuff_files$period == 'annual' | !is.na(calpuff_files$threshold)),
           overwrite = F)


  #specify function that returns the concentration grid for a specific scenario and pollutant


  scenarios = unique(calpuff_files$scenario)
  species = unique(calpuff_files$species)
  conc = tidyr::crossing(scenario=scenarios,
                         species=species)

  conc %<>%
    rowwise() %>%
    dplyr::mutate(
      conc_coal_only=list(get_conc_raster(calpuff_files, scenario, species))
    )

  return(conc)
}



get_conc_base <- function(species,
                          grid_raster,
                          no2_min_incr=NULL){

  conc = list()

  if("pm25" %in% species){
    #source of PM2.5 data: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
    conc["pm25"] <- creahelpers::get_concentration_path('GlobalGWRcwUni_PM25_GL_201601_201612-RH35_Median.nc') %>%
      raster %>%
      cropProj(grid_raster)

  }

  if("no2" %in% species){
    #source of NO2 data: https://data.world/datasets/no2
    conc_no2 <- creahelpers::get_concentration_path('no2_agg8.grd') %>%
      raster %>%
      cropProj(grid_raster)

    #adjust NO2 concentrations using OMI data for 2011 and 2019
    creahelpers::get_concentration_path("no2_omi_2011.tif") %>% raster %>% cropProj(grid_raster) -> no2_11
    creahelpers::get_concentration_path("no2_omi_2019.tif") %>% raster %>% cropProj(grid_raster) -> no2_19
    no2_11 %>% focal(focalWeight(., 100, "circle"), mean, na.rm=T, pad=T, padValue=NA) -> no2_11_smooth
    no2_19 %>% focal(focalWeight(., 100, "circle"), mean, na.rm=T, pad=T, padValue=NA) -> no2_19_smooth
    no2_ratio <- no2_19_smooth/no2_11_smooth

    if(!is.null(no2_min_incr)){
      no2_ratio %<>% max(no2_min_incr)
    }

    conc_no2 %<>%
      multiply_by(no2_ratio*2.1) %>%
      multiply_by(1.88)

    conc_no2[] %<>% na.approx(maxgap=5, na.rm=F)
    conc["no2"] <- conc_no2
  }

  if("o3" %in% species){
    #source of NO2 data: https://data.world/datasets/no2
    conc["o3"] <- creahelpers::get_concentration_path('O3_77e3b7-xmessy_mmd_kk.nc') %>%
      raster %>%
      cropProj(grid_raster)
  }

  if("so2" %in% species){
    conc["so2"] <- grid_raster %>% `values<-`(1)
  }

  tibble(species=names(conc),
         conc_base=conc)
}



#' Extract concentration values and population at specified spatial features
#'
#' @param concs
#' @param map
#'
#' @return
#' @export
#'
#' @examples
extract_concs_at_map <- function(concs, map){

  conc_map <- list()

  # One row per scenario
  for(i in seq(nrow(concs))){

    scenario <- concs$scenario[i]

    cols_to_extract <- c(paste0("conc_coal_", species),
                         paste0("conc_base_", species),
                         "pop") %>%
      intersect(names(concs))

    concs[i,cols_to_extract] %>%
      transpose %>%
      `[[`(1) %>%
      stack -> concs_stack

    raster::extract(concs_stack, adm) -> conc_map[[scenario]]
    names(conc_map[[scenario]]) <- map$region_id
  }

  return(conc_map)
}


combine_concs <- function(conc_coal_only, conc_base){
  conc_coal_only %>%
    left_join(conc_base, by=c("species")) %>%
    rowwise() %>%
    filter(!is.null(conc_base)) %>%
    mutate(conc_coal=list(conc_coal_only[[1]] + conc_base[[1]]))
}

flatten_concs <- function(concs){
  concs %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from=species, values_from=-c(scenario, species))
}

add_pop <- function(concs){
  concs$pop <- list(get_pop(grid_raster))
  return(concs)
}
