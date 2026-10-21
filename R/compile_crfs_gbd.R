gbd_release_catalog <- function() {
  gbd2019_source_path <- "rr/processed/rr_gbd2019.csv"

  releases <- list(
    list(
      version = RR_GBD2019,
      source_path = gbd2019_source_path,
      reviewed = TRUE,
      reference = tibble::tibble(
        reference_id = "ihme_2021_gbd2019_pm_risk_curves",
        author = "Global Burden of Disease Collaborative Network",
        year = 2021,
        title = paste(
          "Global Burden of Disease Study 2019 (GBD 2019)",
          "Particulate Matter Risk Curves"
        ),
        journal = "Institute for Health Metrics and Evaluation (IHME)",
        volume = NA_real_,
        pages = NA_character_,
        doi = "10.6069/KHWH-2703",
        url = "https://doi.org/10.6069/KHWH-2703",
        notes = paste(
          "Official IHME dataset for the GBD 2019 particulate-matter risk",
          "curves. The packaged", gbd2019_source_path, "is the direct",
          "migration source. The original downloaded IHME files are not",
          "versioned in this repository, and Git history does not establish",
          "regeneration of the packaged CSV by the current",
          "generate_rr_gbd2019() code. Registry outcome labels preserve the",
          "package's historical death-calculation use and do not establish",
          "YLL or YLD applicability. LRI.child is created locally by",
          "add_lri_child() from the processed LRI curve and is not an",
          "independently downloaded child-specific curve."
        )
      )
    ),
    list(
      version = RR_GBD2021,
      source_path = "rr/processed/rr_gbd2021.csv",
      reviewed = FALSE,
      reference = NULL
    ),
    list(
      version = RR_GBD2023,
      source_path = "rr/processed/rr_gbd2023.csv",
      reviewed = FALSE,
      reference = NULL
    )
  )

  stats::setNames(
    releases,
    vapply(releases, function(release) release$version, character(1))
  )
}


gbd_release_metadata <- function(version) {
  catalog <- gbd_release_catalog()

  if (length(version) != 1 || !version %in% names(catalog)) {
    stop(
      "Unsupported GBD version: ", paste(version, collapse = ", "),
      ". Supported versions: ", paste(names(catalog), collapse = ", "),
      call. = FALSE
    )
  }

  release <- catalog[[version]]
  if (
    isTRUE(release$reviewed) &&
      (
        is.null(release$reference) ||
          nrow(release$reference) != 1 ||
          is.na(release$reference$reference_id[[1]])
      )
  ) {
    stop(
      "Reviewed GBD version has incomplete reference metadata: ", version,
      call. = FALSE
    )
  }

  release
}


gbd_versions <- function() {
  names(gbd_release_catalog())
}


gbd_reviewed_versions <- function() {
  catalog <- gbd_release_catalog()
  names(catalog)[vapply(
    catalog,
    function(release) isTRUE(release$reviewed),
    logical(1)
  )]
}


gbd_death_causes <- function(version) {
  if (!version %in% gbd_versions()) {
    stop(
      "Unsupported GBD version: ", version,
      ". Supported versions: ", paste(gbd_versions(), collapse = ", "),
      call. = FALSE
    )
  }

  causes <- c(
    CAUSE_LRICHILD,
    CAUSE_IHD,
    CAUSE_STROKE,
    CAUSE_COPD,
    CAUSE_LUNGCANCER,
    CAUSE_LRI,
    CAUSE_DIABETES
  )

  if (identical(version, RR_GBD2023)) {
    causes <- c(causes, CAUSE_DEMENTIA)
  }

  causes
}


gbd_expected_ages <- function(version, cause) {
  supported_causes <- gbd_death_causes(version)
  if (!cause %in% supported_causes) {
    stop(
      "Unsupported ", version, " death cause: ", cause,
      ". Supported causes: ", paste(supported_causes, collapse = ", "),
      call. = FALSE
    )
  }

  if (identical(cause, CAUSE_LRICHILD)) {
    return(AGE_CHILDREN)
  }

  if (
    identical(version, RR_GBD2019) &&
      cause %in% c(CAUSE_IHD, CAUSE_STROKE)
  ) {
    return(AGE_ADULTS_SPLIT)
  }

  AGE_ADULTS
}


gbd_deaths_specs <- function(versions = gbd_versions()) {
  unsupported_versions <- setdiff(versions, gbd_versions())
  if (length(unsupported_versions) > 0) {
    stop(
      "Unsupported GBD versions: ",
      paste(unsupported_versions, collapse = ", "),
      call. = FALSE
    )
  }

  specs <- dplyr::bind_rows(lapply(versions, function(version) {
    causes <- gbd_death_causes(version)
    age_structure <- vapply(
      causes,
      function(cause) {
        if (identical(cause, CAUSE_LRICHILD)) {
          "under5"
        } else if (length(gbd_expected_ages(version, cause)) > 1) {
          "adult"
        } else {
          "25plus"
        }
      },
      character(1)
    )
    slugs <- vapply(causes, gbd_cause_slug, character(1))
    ids <- paste0(
      version, "_pm25_", slugs, "_", age_structure, "_deaths_v1"
    )

    tibble::tibble(
      version = version,
      cause = causes,
      outcome = MEASURE_DEATHS,
      age_structure = age_structure,
      crf_id = ids,
      data_path = paste0("crf/tabular/", ids, ".csv")
    )
  }))

  if (anyDuplicated(specs$crf_id)) {
    stop("GBD candidate specifications contain duplicate crf_ids.", call. = FALSE)
  }

  specs
}


compile_gbd_deaths <- function(
  versions = gbd_versions(),
  overwrite = FALSE,
  update_registry = FALSE,
  update_references = FALSE
) {
  if (
    (update_registry || update_references) &&
      any(!versions %in% gbd_reviewed_versions())
  ) {
    unreviewed_versions <- setdiff(versions, gbd_reviewed_versions())
    stop(
      "Cannot register unreviewed GBD versions: ",
      paste(unreviewed_versions, collapse = ", "),
      ". Reviewed versions: ",
      paste(gbd_reviewed_versions(), collapse = ", "),
      call. = FALSE
    )
  }

  specs <- gbd_deaths_specs(versions)
  source_tables <- stats::setNames(lapply(versions, function(version) {
    readr::read_csv(
      get_hia_path(gbd_source_path(version), error_if_not_exists = TRUE),
      col_types = readr::cols()
    )
  }), versions)

  compiled <- stats::setNames(lapply(seq_len(nrow(specs)), function(i) {
    compile_gbd_deaths_data(
      source_tables[[specs$version[[i]]]],
      version = specs$version[[i]],
      cause = specs$cause[[i]]
    )
  }), specs$crf_id)

  crf_root <- dirname(
    get_hia_path("crf/registry.csv", error_if_not_exists = TRUE)
  )
  output_paths <- file.path(dirname(crf_root), specs$data_path)
  existing_paths <- output_paths[file.exists(output_paths)]
  if (length(existing_paths) > 0 && !overwrite) {
    stop(
      "GBD output files already exist: ",
      paste(existing_paths, collapse = ", "),
      ". Use overwrite = TRUE to regenerate the requested set.",
      call. = FALSE
    )
  }

  dir.create(
    dirname(output_paths[[1]]),
    recursive = TRUE,
    showWarnings = FALSE
  )
  for (i in seq_len(nrow(specs))) {
    readr::write_csv(compiled[[specs$crf_id[[i]]]], output_paths[[i]])
  }

  if (update_registry || update_references) {
    registry_rows <- dplyr::bind_rows(lapply(
      versions,
      gbd_deaths_registry_rows
    ))

    if (update_references) {
      for (version in versions) {
        upsert_crf_reference_row(gbd_reference_row(version))
      }
    }

    if (update_registry) {
      upsert_crf_registry_rows(registry_rows)
    }
  }

  specs
}


compile_gbd_deaths_data <- function(rr, version, cause) {
  required_source_cols <- c(
    "cause", "source", "exposure", "age", "low", "central", "high"
  )
  missing_source_cols <- setdiff(required_source_cols, names(rr))
  if (length(missing_source_cols) > 0) {
    stop(
      "GBD source table is missing columns: ",
      paste(missing_source_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ages <- gbd_expected_ages(version, cause)
  compiled <- rr %>%
    dplyr::filter(
      .data$cause == .env$cause,
      .data$source == .env$version
    ) %>%
    dplyr::mutate(.age_order = match(.data$age, .env$ages)) %>%
    dplyr::arrange(.data$.age_order, .data$exposure) %>%
    dplyr::select(exposure, age, low, central, high) %>%
    tibble::as_tibble()

  validate_compiled_tabular_crf(
    compiled,
    expected_ages = ages,
    data_name = paste("Compiled", version, cause, "table")
  )
  compiled
}


gbd_source_path <- function(version) {
  gbd_release_metadata(version)$source_path
}


gbd_cause_slug <- function(cause) {
  slug <- gsub("[^a-z0-9]+", "_", tolower(cause))
  gsub("^_|_$", "", slug)
}


gbd_deaths_registry_rows <- function(version = RR_GBD2019) {
  if (!version %in% gbd_reviewed_versions()) {
    stop(
      "GBD version has not completed provenance review: ", version,
      ". Reviewed versions: ",
      paste(gbd_reviewed_versions(), collapse = ", "),
      call. = FALSE
    )
  }

  specs <- gbd_deaths_specs(version)
  release <- gbd_release_metadata(version)

  dplyr::bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    cause <- specs$cause[[i]]
    child_lri <- identical(cause, CAUSE_LRICHILD)

    tibble::tibble(
      crf_id = specs$crf_id[[i]],
      pollutant = "PM25",
      cause = cause,
      outcome = MEASURE_DEATHS,
      double_counted = FALSE,
      age_low = if (child_lri) 0 else 25,
      age_high = if (child_lri) 4 else Inf,
      region_applicability = "global",
      form = CRF_FORM_TABULAR,
      rr_central = NA_real_,
      rr_low = NA_real_,
      rr_high = NA_real_,
      conc_change = NA_real_,
      counterfact = NA_real_,
      unit = "ug",
      units_multiplier = 1,
      data_path = specs$data_path[[i]],
      reference_id = release$reference$reference_id[[1]],
      notes = gbd_deaths_notes(version, cause)
    )
  }))
}


gbd_deaths_notes <- function(version, cause) {
  if (!version %in% gbd_reviewed_versions()) {
    stop("GBD version has not completed provenance review: ", version, call. = FALSE)
  }

  if (!cause %in% gbd_death_causes(version)) {
    stop("Unsupported ", version, " death cause: ", cause, call. = FALSE)
  }

  release <- gbd_release_metadata(version)
  migration_source <- file.path("inst/extdata", release$source_path)

  if (identical(cause, CAUSE_LRICHILD)) {
    return(paste(
      "Opt-in Registry migration from", paste0(migration_source, ";"),
      "legacy-derived Under 5 alias created by add_lri_child() from the",
      "processed LRI curve, not an independently downloaded child curve"
    ))
  }

  paste(
    "Opt-in Registry migration from", paste0(migration_source, ";"),
    "preserves the packaged", cause, "age structure"
  )
}


gbd_reference_row <- function(version = RR_GBD2019) {
  if (!version %in% gbd_reviewed_versions()) {
    stop(
      "GBD version has not completed provenance review: ", version,
      call. = FALSE
    )
  }

  gbd_release_metadata(version)$reference
}
