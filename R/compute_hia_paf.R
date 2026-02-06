#' Compute RR-based PAF calculations
#'
#' The RR-based PAF part corresponds to causes with constant relative risk per concentration change
#' using RR curves from GBD/GEMM for PM2.5 mortality outcomes
#'
#' @param conc_map
#' @param epi_version
#' @param rr_sources
#' @param scenarios
#' @param .mode
#'
#' @return
#' @export
#'
#' @examples
compute_hia_paf_rr_curves <- function(conc_map,
                                      epi_version,
                                      rr_sources,
                                      scenarios = names(conc_map),
                                      .mode = 'change') {

  paf <- list()

  cause_measure_source <- get_cause_source(rr_sources=rr_sources,
                                            add_measure=T)

  empty_rr_df <- tibble::tibble(
    pollutant = character(),
    cause = character(),
    outcome = character(),
    region_id = character(),
    low = numeric(),
    central = numeric(),
    high = numeric()
  )

  for(scenario in scenarios) {

    conc_scenario <- conc_map[[scenario]]
    # Flatten and split back by region using dplyr/base instead of plyr
    conc_df <- purrr::map(conc_scenario, data.frame) %>%
      dplyr::bind_rows(.id = 'region_id')
    conc_scenario <- split(conc_df, conc_df$region_id)


    pg <- progress::progress_bar$new(
      format = "Computing PAF [:bar] :percent :eta",
      total = length(names(conc_scenario))
    )

    scenario_rows <- lapply(names(conc_scenario), function(region_id) {

      tryCatch({
        pg$tick()
        non_na_cols <- c('conc_baseline_pm25', 'conc_scenario_pm25', 'pop')
        conc <- conc_scenario[[region_id]][complete.cases(conc_scenario[[region_id]][,non_na_cols]),]
        if(nrow(conc)==0) return(NULL)

        region_rows <- lapply(seq_len(nrow(cause_measure_source)), function(i) {
          measure_ <- cause_measure_source$measure[i]
          cause_ <- cause_measure_source$cause[i]
          rr_source_ <- cause_measure_source$source[i]
          logger::log_debug(glue("Computing PAF for {cause_} and {measure_} from {rr_source_}"))

          paf_values <- country_paf_perm(pm.base = conc[, 'conc_baseline_pm25'],
                                         pm.perm = conc[, 'conc_scenario_pm25'],
                                         pop = conc[, 'pop'],
                                         region_id = region_id,
                                         cause = cause_,
                                         measure = measure_,
                                         rr_source = rr_source_,
                                         epi_version = epi_version,
                                         .region = "inc_China",
                                         .mode = .mode)

          if(is.null(paf_values)) return(NULL)

          tibble::tibble(
            pollutant = "PM25",
            cause = cause_,
            outcome = measure_,
            region_id = region_id,
            low = unname(paf_values[['low']]),
            central = unname(paf_values[['central']]),
            high = unname(paf_values[['high']])
          )
        })

        region_rows <- region_rows[!vapply(region_rows, is.null, logical(1))]
        if(length(region_rows) == 0) return(NULL)

        dplyr::bind_rows(region_rows)
      }, error = function(e) {
        # For instance if country iso3 not in epi_long$ISO3
        # or paf not well ordered
        logger::log_warn(paste("Failed for region ", region_id, ": ", e$message))
        warning(paste("Failed for region ", region_id, ": ", e$message))
        return(NULL)
      })
    })

    scenario_rows <- scenario_rows[!vapply(scenario_rows, is.null, logical(1))]
    paf[[scenario]] <- if(length(scenario_rows) == 0) empty_rr_df else dplyr::bind_rows(scenario_rows)
  }

  # Combine all scenarios
  paf
}


#' Compute CRF-based PAF calculations
#'
#' This function computes PAF values using concentration-response functions (CRFs)
#' for non-PM2.5 pollutants and outcomes like asthma, birth outcomes, etc.
#'
#' @param species Vector of pollutant species
#' @param conc_map List of concentration maps by scenario
#' @param regions Spatial regions data
#' @param crfs CRF data table
#' @param .mode Computation mode (default: 'change')
#' @return List of PAF values by scenario and region
#' @export
compute_hia_paf_crfs <- function(species,
                                conc_map,
                                regions,
                                crfs = get_crfs(),
                                .mode = 'change') {
  hia_polls <- species_to_hiapoll(species)
  scenarios <- names(conc_map)
  paf_crfs <- list()

  empty_crf_df <- tibble::tibble(
    pollutant = character(),
    cause = character(),
    outcome = character(),
    region_id = character(),
    low = numeric(),
    central = numeric(),
    high = numeric()
  )

  for(scenario in scenarios) {
    conc_scenario <- conc_map[[scenario]]
    conc_df <- dplyr::bind_rows(conc_scenario, .id = 'region_id')

    if(!all(complete.cases(conc_df))) {
      warning('missing values in concentration or population data')
      conc_df %<>% na.omit
    }

    conc_scenario <- split(conc_df, conc_df$region_id)
    region_ids <- names(conc_scenario)

    scenario_rows <- list()

    for(i in which(crfs$pollutant %in% hia_polls)) {

      species_name <- hiapoll_to_species(crfs$pollutant[i])

      if(grepl('nrt', species_name)) {
        source_concs <- get_nrt_conc(region_ids, species_name, 0, conc_map = conc_scenario)
      } else {
        base_name <- species_name %>% paste0('conc_baseline_',.)
        perm_name <- species_name %>% paste0('conc_scenario_',.)
        nrt_flag <- NULL # ifelse(grepl('NCD\\.LRI_', crfs$Incidence[i]), 'grump', NULL)

        base_concs <- get_nrt_conc(region_ids = region_ids,
                                   conc_name = base_name,
                                   nrt = crfs$counterfact[i],
                                   conc_map = conc_scenario,
                                   units_multiplier = crfs$units_multiplier[i],
                                   nrt_flag = nrt_flag)

        perm_concs <- get_nrt_conc(region_ids = region_ids,
                                   conc_name = perm_name,
                                   nrt = crfs$counterfact[i],
                                   conc_map = conc_scenario,
                                   units_multiplier = crfs$units_multiplier[i],
                                   nrt_flag = nrt_flag)

        source_concs <- perm_concs - base_concs
      }

      effect_df <- tibble::tibble(
        pollutant = crfs$pollutant[i],
        cause = crfs$cause[i],
        outcome = crfs$outcome[i],
        region_id = region_ids,
        low = 1 - exp(-log(crfs$rr_low[i]) * source_concs / crfs$conc_change[i]),
        central = 1 - exp(-log(crfs$rr_central[i]) * source_concs / crfs$conc_change[i]),
        high = 1 - exp(-log(crfs$rr_high[i]) * source_concs / crfs$conc_change[i])
      )

      scenario_rows[[length(scenario_rows) + 1]] <- effect_df
    }

    scenario_rows <- scenario_rows[!vapply(scenario_rows, is.null, logical(1))]
    paf_crfs[[scenario]] <- if(length(scenario_rows) == 0) empty_crf_df else dplyr::bind_rows(scenario_rows)
  }

  return(paf_crfs)
}


#' Compute all PAF calculations (RR-based and CRF-based)
#'
#' This is the main coordinator function that computes both types of PAF:
#' 1. RR-based PAF for PM2.5 mortality using RR curves
#' 2. CRF-based PAF for other pollutants/outcomes using concentration-response functions
#'
#' @param conc_map List of concentration maps by scenario
#' @param species Vector of pollutant species
#' @param regions Spatial regions data
#' @param scenarios Vector of scenario names
#' @param epi_version EPI data version
#' @param rr_sources Vector of RR sources (for RR-based PAF)
#' @param crfs CRF data table (for CRF-based PAF)
#' @param .mode Computation mode (default: 'change')
#' @return Combined dataframe of PAF values from both methods
#' @export
compute_hia_paf <- function(conc_map,
                           species,
                           regions,
                           scenarios = names(conc_map),
                           epi_version = "default",
                           rr_sources = c(),
                           crfs = get_crfs(),
                           .mode = 'change',
                           diagnostic_folder = "diagnostic"
                           ) {

  paf <- tibble::tibble(
    scenario = character(),
    pollutant = character(),
    cause = character(),
    outcome = character(),
    region_id = character(),
    low = numeric(),
    central = numeric(),
    high = numeric()
  )

  # Compute RR-based PAF if rr_sources are provided
  if(length(rr_sources) > 0) {
    print("Computing RR-based PAF")
    paf_rr <- compute_hia_paf_rr_curves(conc_map = conc_map,
                                        scenarios = scenarios,
                                        epi_version = epi_version,
                                        rr_sources = rr_sources,
                                        .mode = .mode)
    paf_rr_combined <- paf_rr %>%
      bind_rows(.id = 'scenario')
    paf <- bind_rows(paf, paf_rr_combined)
  }

  # Compute CRF-based PAF
  print("Computing CRF-based PAF")
  paf_crf <- compute_hia_paf_crfs(species = species,
                                  conc_map = conc_map,
                                  regions = regions,
                                  crfs = crfs,
                                  .mode = .mode)
  paf_crf_combined <- paf_crf %>%
    bind_rows(.id = 'scenario')
  paf <- bind_rows(paf, paf_crf_combined)


  # Plot diagnostics
  diagnose_paf(paf, diagnostic_folder)

  return(paf)
}


# define a function to calculate the hazard ratio for a specific concentration, cause and age group
get_hazard_ratio <- function(pm,
                             rr,
                             .age,
                             .cause
                             ) {

  rr_filtered <- rr %>%
    dplyr::filter(cause == .cause, age == .age) %>%
    # Remove duplicate exposure values to avoid interpolation warnings
    distinct(exposure, .keep_all = TRUE) %>%
    arrange(exposure)

  # Guard against extrapolation beyond RR exposure grid
  exp_min <- min(rr_filtered$exposure, na.rm = TRUE)
  exp_max <- max(rr_filtered$exposure, na.rm = TRUE)
  if (any(pm < exp_min | pm > exp_max, na.rm = TRUE)) {
    cause_str <- .cause; age_str <- .age
    req_min <- suppressWarnings(min(pm, na.rm = TRUE))
    req_max <- suppressWarnings(max(pm, na.rm = TRUE))
    stop(glue::glue(
      "Exposure out of RR range for {cause_str} / {age_str}. Allowed: [{exp_min}, {exp_max}], requested: [{req_min}, {req_max}]"
    ))
  }

  rr_filtered %>% sel(low, central, high) %>%
    apply(2, function(y) approx(x = rr_filtered$exposure, y, xout = pm)$y)

}


# total fossil fuel PAF for a permutation run
country_paf_perm <- function(pm.base,
                             pm.perm,
                             pop,
                             region_id,
                             cause,
                             measure,
                             rr_source,
                             epi_version,
                             .region = "inc_China",
                             .mode = 'change') { # change or attribution?


  rr <- get_rr(rr_source) %>%
    filter(cause == !!cause)

  # Get age weights ---
  age_data <- get_age_weights(region_id, cause, measure, rr_source, epi_version)
  if(is.null(age_data)) return(NULL)

  ages <- age_data$ages
  age_weights <- age_data$age_weights


  rr.base <- ages %>% sapply(function(.a) get_hazard_ratio(pm.base, rr = rr, .cause = cause, .age = .a),
                             simplify = 'array')

  if(.mode == 'change') {
    rr.perm <- ages %>% sapply(function(.a) get_hazard_ratio(pm.perm, rr = rr, .cause = cause, .age = .a),
                               simplify = 'array')

    paf <- get_paf_from_rr_lauri(
      rr_base = rr.base,
      rr_perm = rr.perm,
      age_weights = age_weights$val,
      pop = pop,
      cause=cause,measure=measure
    )

  } else {
    stop("Attribution mode not implemented yet")
    #TODO implement the method with this formula
    # paf.perm <- (1 - (1 / rr.base)) * (1 - pm.perm / pm.base)
  }

  return(paf)
}


#' Export PAF diagnostic plots to diagnostic_folder
#'
#'
#' @param paf
#' @param diagnostic_folder
#'
#' @returns
#' @export
#'
#' @examples
diagnose_paf <- function(paf, diagnostic_folder) {

  if(is.null(diagnostic_folder)) {
    logger::log_info("No diagnostic folder provided, skipping PAF diagnostics.")
    return()
  }

  dir.create(diagnostic_folder, showWarnings = FALSE, recursive = TRUE)

  outcomes <- unique(paf$outcome)
  regions <- unique(paf$region_id)

  multiple_regions <- length(regions) > 1

  if(multiple_regions) {

    for(outcome in outcomes) {
      plot <- paf %>%
        filter(outcome == !!outcome) %>%
        ggplot() +
        geom_col(aes(cause, -central, fill=cause), show.legend = F) +
        facet_wrap(~region_id) +
        labs(title = paste("PAF for", outcome), y=NULL, x = NULL) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        rcrea::theme_crea_new() +
        rcrea::scale_fill_crea_d()

      ggsave(filename = file.path(diagnostic_folder, paste0("paf_", tolower(outcome), ".png")), plot = plot, width = 10, height = 6)
    }

  } else {

    plot <- paf %>%
      ggplot() +
      geom_col(aes(cause, -central, fill=cause), show.legend = F) +
      facet_wrap(~outcome, scales='free') +
      labs(title = glue("PAF by cause and outcome for {unique(paf$region_id)}"), y=NULL, x = NULL) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      rcrea::theme_crea_new() +
      rcrea::scale_fill_crea_d()

    ggsave(filename = file.path(diagnostic_folder, "paf_by_outcome.png"), plot = plot, width = 10, height = 6)

  }
}
