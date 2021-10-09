#' Compute HIA wrapper: given a list of rasters that represent perturbation in one or several pollutants.
#'
#' @param perturbation_rasters
#' @param baseline_rasters
#' @param crfs_version
#'
#' @return
#' @export
#'
#' @examples
wrappers.compute_hia_two_images <- function(perturbation_rasters,
                                            baseline_rasters=NULL,
                                            regions=NULL,
                                            administrative_level=1,
                                            administrative_res="coarse",
                                            administrative_iso3s=NULL,
                                            scenario_name="scenario",
                                            scale_base_year=2020,
                                            scale_target_year=2025,
                                            crfs_version="default",
                                            epi_version="default",
                                            valuation_version="default"){


  #TODO make it work with raster stack as well
  species <- names(perturbation_rasters)
  grid_raster = perturbation_rasters[[1]] %>% raster

  conc_perturbation <- tibble(
    species=species,
    conc_perturbation=raster::as.list(raster::stack(perturbation_rasters)),
    scenario=scenario_name
  )

  # 02: Get base concentration levels --------------------------------------------------------
  if(is.null(baseline_rasters)){
    conc_baseline <- creahia::get_conc_baseline(species=species, grid_raster=grid_raster)
  }else{
    # Work with either RasterStack or list of rasters
    conc_baseline <- tibble(species=names(baseline_rasters),
                            conc_baseline=raster::as.list(raster::stack(baseline_rasters)))
  }

  # 03: Combine and flatten: one row per scenario --------------------------------------------
  concs <- creahia::combine_concs(conc_perturbation, conc_baseline) %>% creahia::flatten_concs() %>% creahia::add_pop(grid_raster)

  # 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
  if(is.null(regions)){
    regions <- creahia::get_adm(grid_raster,
                                admin_level=administrative_level,
                                res=administrative_res,
                                iso3s=administrative_iso3s)
  }


  # 05: Extract concentrations ---------------------------------------------------------------
  conc_regions <- creahia::extract_concs_at_regions(concs, regions, species)

  # 06: Compute hia --------------------------------------------------------------------------
  hia <- creahia::compute_hia(conc_map=conc_regions,
                              species=species,
                              regions=regions,
                              scale_base_year=scale_base_year,
                              scale_target_year=scale_target_year,
                              epi_version=epi_version,
                              crfs_version=crfs_version)

  # hia_table <- hia %>% totalise_hia() %>% make_hia_table()
  return(hia)
}
