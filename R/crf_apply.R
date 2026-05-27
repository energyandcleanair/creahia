apply_crf <- function(
  crf,
  conc_base,
  conc_perm,
  pop,
  region_id,
  epi_version = "default"
) {
  if (nrow(crf) != 1) {
    stop("apply_crf() expects exactly one CRF row.", call. = FALSE)
  }

  if (identical(crf$form, CRF_FORM_TABULAR)) {
    return(apply_crf_tabular(crf, conc_base, conc_perm, pop, region_id, epi_version))
  }

  if (identical(crf$form, CRF_FORM_LOG_LINEAR)) {
    return(apply_crf_log_linear(crf, conc_base, conc_perm, pop, region_id))
  }

  stop("Unsupported CRF form: ", crf$form, call. = FALSE)
}

apply_crf_tabular <- function(
  crf,
  conc_base,
  conc_perm,
  pop,
  region_id,
  epi_version = "default"
) {
  if (nrow(crf) != 1) stop("apply_crf_tabular() expects exactly one CRF row.", call. = FALSE)
  if (!identical(crf$form, CRF_FORM_TABULAR)) stop("CRF form must be tabular.", call. = FALSE)

  # load crf data
  rr <- load_crf_tabular(crf)

  # get age
  ages <- rr %>% 
    dplyr::distinct(age) %>% 
    dplyr::pull(age) %>% 
    deduplicate_adult_ages()

  check_age_coverage_and_uniqueness(
    ages,
    data_name = glue::glue("CRF {crf$crf_id}")
  )

  # get age weight from epi data
  age_weights <- get_crf_age_weights(
    region_id = region_id,
    cause = crf$cause,
    outcome = crf$outcome,
    ages = ages,
    epi_version = epi_version
  )

  if(is.null(age_weights)) return(NULL)

  # calculate rr_base_paf
  rr_base <- ages %>%
    sapply(function(age) get_hazard_ratio_tabular(conc_base, rr, age), simplify = "array")

  # calculate rr_perm_paf
  rr_perm <- ages %>%
    sapply(function(age) get_hazard_ratio_tabular(conc_perm, rr, age), simplify = "array")

  # use get_paf_from_rr_lauri to calculate age-population weighted paf
  paf <- get_paf_from_rr_lauri(
    rr_base = rr_base,
    rr_perm = rr_perm,
    age_weights = age_weights$val,
    pop = pop,
    cause = crf$cause,
    measure = crf$outcome
  )

  tibble::tibble(
    pollutant = crf$pollutant,
    cause = crf$cause,
    outcome = crf$outcome,
    region_id = region_id,
    low = unname(paf[["low"]]),
    central = unname(paf[["central"]]),
    high = unname(paf[["high"]])
  )

}

apply_crf_log_linear <- function(
  crf, 
  conc_base,
  conc_perm,
  pop,
  region_id
) {
  if (nrow(crf) != 1) { 
    stop("apply_crf_log_linear() expects exactly one CRF row.", call. = FALSE)
  }
  if (!identical(crf$form, CRF_FORM_LOG_LINEAR)){ 
    stop("CRF form must be log-linear.", call. = FALSE)
  }

  # caculate the source concentration
  source_conc <- get_log_linear_source_conc(
    conc = conc_perm,
    conc_ref = crf$conc_ref,
    counterfact = crf$counterfact,
    units_multiplier = crf$units_multiplier
  )

  tibble::tibble(
    pollutant = crf$pollutant,
    cause = crf$cause,
    outcome = crf$outcome,
    region_id = region_id,
    low = calculate_log_linear_paf(crf$rr_low, source_conc, crf$conc_ref),
    central = calculate_log_linear_paf(crf$rr_central, source_conc, crf$conc_ref),
    high = calculate_log_linear_paf(crf$rr_high, source_conc, crf$conc_ref)
  )

}


load_crf_tabular <- function(crf) {
  # load the tabular CRF data from the path specified in the registry, and return it as a tibble.
  # check that the file exists and has the expected columns for a tabular CRF.

  if (nrow(crf) != 1) {
    stop("Expected a single row for the CRF registry entry, but got ", nrow(crf), " rows.", call. = FALSE)
  }
  if (!identical(crf$form, CRF_FORM_TABULAR)) stop("CRF form must be tabular.", call. = FALSE)

  if (is.na(crf$data_path) || !nzchar(crf$data_path)) {
    stop("Tabular CRF must have a data_path.", call. = FALSE)
  }

  rr <- readr::read_csv(
    get_hia_path(crf$data_path, error_if_not_exists = TRUE),
    col_types = readr::cols()
  )

  required_cols <- c("exposure", "age", "low", "central", "high")
  missing_cols <- setdiff(required_cols, names(rr))
  if (length(missing_cols) > 0) {
    stop(
      "Tabular CRF data is missing the following required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    ) 
  }

  rr %>% 
    dplyr::arrange(age, exposure)


}

get_crf_age_weights <- function(region_id, cause, outcome, ages, epi_version){
  # get age weights for the specified region. 
  # This will be used to calculate age-weighted PAFs for tabular CRFs.

  epi_long <- get_epi_count_long(epi_version)

  age_weights <- epi_long %>%
    dplyr::mutate(age = recode_age(age)) %>% 
    dplyr::filter(
      location_id == get_epi_location_id(region_id),
      cause == !!cause,
      measure_name == !!outcome,
      age %in% ages,
      estimate == "central"
    )

   if (nrow(age_weights) == 0) {
    warning(glue::glue("No age weights found for {region_id}, {cause}, {outcome}"))
    return(NULL)
  }

  if (length(age_weights$age) != length(ages)) {
    stop("Unmatching age weights.", call. = FALSE)
  }

  age_weights[match(ages, age_weights$age), ]
}

get_hazard_ratio_tabular <- function(conc, rr, age) {
  # given a tabular CRF, get the hazard ratio for a specific concentration and age group. 
  # This will be used to calculate PAFs for tabular CRFs.

  rr_filtered <- rr %>% 
    dplyr::filter(age == !!age) %>%
    dplyr::distinct(exposure, .keep_all = TRUE) %>% 
    dplyr::arrange(exposure)

  if(nrow(rr_filtered) == 0) {
    stop(glue::glue("No RR curve found for age {age}"))
  }

  exp_min <- min(rr_filtered$exposure, na.rm = TRUE)
  exp_max <- max(rr_filtered$exposure, na.rm = TRUE)

  if (any(conc < exp_min | conc > exp_max, na.rm = TRUE)) {
    stop(glue::glue(
      "Exposure out of CRF range for age {age}. Allowed: [{exp_min}, {exp_max}]"
    ), call. = FALSE)
  }

  rr_filtered %>%
    dplyr::select(low, central, high) %>%
    apply(2, function(y) {
      approx(x = rr_filtered$exposure, y = y, xout = conc)$y
    })

 
}

get_log_linear_source_conc <- function(conc, conc_ref, counterfact, units_multiplier) {
  # for log-linear CRFs, calculate the source concentration based on the reference concentration, counterfactual concentration, and units multiplier.
  # This will be used to calculate PAFs for log-linear CRFs.

  if (length(conc_base) != length(conc_perm) || length(conc_base) != length(pop)) {
    stop("conc_base, conc_perm, and pop must have the same length.", call. = FALSE)
  }

  if (all(is.na(pop)) || sum(pop, na.rm = TRUE) <= 0) {
    stop("Population weights must have a positive sum.", call. = FALSE)
  }


  base_excess <- pmax(conc_base * units_multiplier - counterfact, 0)
  perm_excess <- pmax(conc_perm * units_multiplier - counterfact, 0)

  weighted.mean(perm_excess, w = pop, na.rm = TRUE) - weighted.mean(base_excess, w = pop, na.rm = TRUE)
}

calculate_log_linear_paf <- function(rr, source_conc, conc_ref) {
  1 - exp(-log(rr) * source_conc / conc_ref)
}