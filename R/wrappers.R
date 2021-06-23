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
                                            administrative_level=1,
                                            administrative_res="coarse",
                                            administrative_iso3s=NULL,
                                            scenario_name="scenario",
                                            crfs_version="default",
                                            scale_base_year=2015,
                                            scale_target_year=2025){


  #TODO make it work with raster stack as well
  species <- names(perturbation_rasters)
  grid_raster = perturbation_rasters[[1]] %>% raster

  # 02: Get base concentration levels --------------------------------------------------------
  if(is.null(baseline_rasters)){
    conc_baseline <- creahia::get_conc_baseline(species=species, grid_raster=grid_raster)
  }else{
    conc_baseline <- tibble(species=names(baseline_rasters), conc_baseline=baseline_rasters)
  }

  # 03: Combine and flatten: one row per scenario --------------------------------------------
  concs <- creahia::combine_concs(conc_perturbation, conc_baseline) %>% flatten_concs() %>% add_pop()

  # 04: Create support maps (e.g. countries, provinces, cities ) -----------------------------
  regions <- creahia::get_map_adm(grid_raster, admin_level=administrative_level,
                              res=administrative_res,
                              iso3s=administrative_iso3s)

  # 05: Extract concentrations ---------------------------------------------------------------
  conc_adm <- creahia::extract_concs_at_map(concs, regions)

  # 06: Compute hia --------------------------------------------------------------------------
  hia <- creahia::compute_hia(conc_map=conc_adm,
                              species=species,
                              regions=regions,
                              scale_base_year=2015,
                              scale_target_year=2025)

  hia_table <- hia %>% make_hia_table()

}
