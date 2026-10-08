.hia_component_columns <- function() {
  c(
    "component_id",
    "component_type",
    "selection_id",
    "source",
    "cause",
    "outcome",
    "age_contract",
    "readiness_source",
    "runtime_ready",
    "blocking_reason"
  )
}


.empty_hia_components <- function() {
  tibble::tibble(
    component_id = character(),
    component_type = character(),
    selection_id = character(),
    source = character(),
    cause = character(),
    outcome = character(),
    age_contract = character(),
    readiness_source = character(),
    runtime_ready = logical(),
    blocking_reason = character()
  )
}


.component_id <- function(component_type, ...) {
  values <- list(...)
  encoded <- lapply(values, function(value) {
    value <- as.character(value)
    value[is.na(value)] <- "_"
    value
  })

  do.call(paste, c(list(component_type), encoded, sep = "|"))
}


.validate_component_manifest <- function(manifest) {
  required_columns <- c(
    "component_id",
    "component_type",
    "selection_id",
    "source",
    "cause",
    "outcome",
    "age_contract",
    "lifecycle",
    "readiness_status",
    "blocking_reason"
  )

  missing_columns <- setdiff(required_columns, names(manifest))
  if (length(missing_columns) > 0) {
    stop(
      "Component readiness manifest is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_ids <- manifest$component_id[duplicated(manifest$component_id)]
  if (length(duplicated_ids) > 0) {
    stop(
      "Component readiness manifest contains duplicate component_id values: ",
      paste(unique(duplicated_ids), collapse = ", "),
      call. = FALSE
    )
  }

  allowed_component_types <- c("crf", "rr")
  invalid_component_types <- setdiff(
    unique(manifest$component_type),
    allowed_component_types
  )
  if (length(invalid_component_types) > 0) {
    stop(
      "Component readiness manifest contains invalid component_type values: ",
      paste(invalid_component_types, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_lifecycles <- c(
    "reviewed_public",
    "historical_reproduction",
    "validation_fixture"
  )
  invalid_lifecycles <- setdiff(unique(manifest$lifecycle), allowed_lifecycles)
  if (length(invalid_lifecycles) > 0) {
    stop(
      "Component readiness manifest contains invalid lifecycle values: ",
      paste(invalid_lifecycles, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_statuses <- c("registry_ready", "not_ready")
  invalid_statuses <- setdiff(
    unique(manifest$readiness_status),
    allowed_statuses
  )
  if (length(invalid_statuses) > 0) {
    stop(
      "Component readiness manifest contains invalid readiness_status values: ",
      paste(invalid_statuses, collapse = ", "),
      call. = FALSE
    )
  }

  ready_with_blocker <- manifest$readiness_status == "registry_ready" &
    !is.na(manifest$blocking_reason)
  not_ready_without_blocker <- manifest$readiness_status == "not_ready" &
    is.na(manifest$blocking_reason)
  fixture_marked_ready <- manifest$lifecycle == "validation_fixture" &
    manifest$readiness_status == "registry_ready"

  if (any(ready_with_blocker)) {
    stop(
      "Registry-ready components must not retain a blocking_reason.",
      call. = FALSE
    )
  }
  if (any(not_ready_without_blocker)) {
    stop(
      "Not-ready components must have a blocking_reason.",
      call. = FALSE
    )
  }
  if (any(fixture_marked_ready)) {
    stop(
      "Validation fixtures cannot satisfy production Registry readiness.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.normalise_rr_source_pairs <- function(rr_sources, rr_loader = get_rr) {
  input_mode <- NULL

  if (is.data.frame(rr_sources)) {
    input_mode <- "data_frame"
    missing_columns <- setdiff(c("cause", "source"), names(rr_sources))
    if (length(missing_columns) > 0) {
      stop(
        "rr_sources data frame is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }
    pairs <- rr_sources %>%
      dplyr::select(cause, source)
  } else if (is.character(rr_sources) || is.factor(rr_sources)) {
    input_mode <- "preference_vector"
    source_order <- as.character(rr_sources)

    if (length(source_order) == 0) {
      pairs <- tibble::tibble(cause = character(), source = character())
    } else {
      pairs <- lapply(source_order, function(source_name) {
        rr <- rr_loader(source_name)
        missing_columns <- setdiff(c("cause", "source"), names(rr))
        if (length(missing_columns) > 0) {
          stop(
            "RR source '", source_name, "' is missing required columns: ",
            paste(missing_columns, collapse = ", "),
            call. = FALSE
          )
        }
        rr %>%
          dplyr::distinct(cause, source)
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(source_priority = match(source, source_order)) %>%
        dplyr::arrange(source_priority) %>%
        dplyr::distinct(cause, .keep_all = TRUE) %>%
        dplyr::select(cause, source)
    }
  } else if (is.list(rr_sources)) {
    input_mode <- "named_list"
    if (is.null(names(rr_sources)) || any(names(rr_sources) == "")) {
      stop("rr_sources lists must be named by RR source.", call. = FALSE)
    }

    pairs <- lapply(names(rr_sources), function(source_name) {
      tibble::tibble(
        cause = as.character(rr_sources[[source_name]]),
        source = source_name
      )
    }) %>%
      dplyr::bind_rows()
  } else {
    stop(
      "Invalid format for rr_sources. Must be a character vector, named list, or data frame.",
      call. = FALSE
    )
  }

  pairs <- pairs %>%
    dplyr::mutate(
      cause = as.character(cause),
      source = as.character(source)
    ) %>%
    dplyr::distinct(cause, source)

  if (any(is.na(pairs$cause) | pairs$cause == "")) {
    stop("rr_sources contains a missing or empty cause.", call. = FALSE)
  }
  if (any(is.na(pairs$source) | pairs$source == "")) {
    stop("rr_sources contains a missing or empty source.", call. = FALSE)
  }

  attr(pairs, "input_mode") <- input_mode
  pairs
}


.normalise_rr_components <- function(
  rr_sources,
  rr_loader = get_rr,
  measure_map = get_cause_measure()
) {
  pairs <- .normalise_rr_source_pairs(rr_sources, rr_loader = rr_loader)
  input_mode <- attr(pairs, "input_mode")
  attr(pairs, "input_mode") <- NULL

  if (nrow(pairs) == 0) {
    return(.empty_hia_components())
  }

  missing_measure_columns <- setdiff(c("cause", "measure"), names(measure_map))
  if (length(missing_measure_columns) > 0) {
    stop(
      "RR measure map is missing required columns: ",
      paste(missing_measure_columns, collapse = ", "),
      call. = FALSE
    )
  }

  missing_measures <- dplyr::anti_join(
    pairs %>% dplyr::distinct(cause),
    measure_map %>% dplyr::distinct(cause),
    by = "cause"
  )
  if (nrow(missing_measures) > 0) {
    stop(
      "No outcome/measure mapping exists for RR causes: ",
      paste(missing_measures$cause, collapse = ", "),
      call. = FALSE
    )
  }

  available_ages <- lapply(unique(pairs$source), function(source_name) {
    rr <- rr_loader(source_name)
    missing_columns <- setdiff(c("cause", "age"), names(rr))
    if (length(missing_columns) > 0) {
      stop(
        "RR source '", source_name, "' is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }

    rr %>%
      dplyr::transmute(
        cause = as.character(cause),
        source = source_name,
        age_contract = as.character(age)
      ) %>%
      dplyr::distinct()
  }) %>%
    dplyr::bind_rows()

  missing_curves <- dplyr::anti_join(
    pairs,
    available_ages %>% dplyr::distinct(cause, source),
    by = c("cause", "source")
  )
  if (nrow(missing_curves) > 0) {
    missing_labels <- paste(
      missing_curves$source,
      missing_curves$cause,
      sep = "/"
    )
    stop(
      "Selected RR source/cause pairs have no curve data: ",
      paste(missing_labels, collapse = ", "),
      call. = FALSE
    )
  }

  pairs %>%
    dplyr::inner_join(measure_map, by = "cause") %>%
    dplyr::inner_join(
      available_ages,
      by = c("cause", "source"),
      relationship = "many-to-many"
    ) %>%
    dplyr::transmute(
      component_id = .component_id(
        "rr", source, cause, measure, age_contract
      ),
      component_type = "rr",
      selection_id = paste0("rr_sources:", input_mode),
      source,
      cause,
      outcome = as.character(measure),
      age_contract,
      readiness_source = "manifest",
      runtime_ready = NA,
      blocking_reason = NA_character_
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(source, cause, outcome, age_contract)
}


.validate_runtime_registry_crfs <- function(crfs) {
  missing_columns <- setdiff(CRF_REGISTRY_REQUIRED_COLUMNS, names(crfs))
  if (length(missing_columns) > 0) {
    stop(
      "Registry-shaped custom CRFs are missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  validate_crf_selection(crfs)
  validate_log_linear_crfs(crfs)
  validate_tabular_crfs(crfs)
  assert_no_internal_crfs(crfs, context = "runtime Registry CRFs")
  invisible(TRUE)
}


.normalise_crf_components <- function(
  crfs,
  input_mode = c("auto", "legacy_profile", "custom"),
  selection_id = NULL
) {
  input_mode <- match.arg(input_mode)

  if (!is.data.frame(crfs)) {
    stop("crfs must be a data frame or a crfs_set() result.", call. = FALSE)
  }

  required_slot_columns <- c("pollutant", "cause", "outcome")
  missing_slot_columns <- setdiff(required_slot_columns, names(crfs))
  if (length(missing_slot_columns) > 0) {
    stop(
      "CRFs are missing required component columns: ",
      paste(missing_slot_columns, collapse = ", "),
      call. = FALSE
    )
  }

  registry_input <- is_crfs_set(crfs)
  if (identical(input_mode, "auto")) {
    input_mode <- if (registry_input) "registry_set" else "custom"
  }

  if (identical(input_mode, "legacy_profile")) {
    if (is.null(selection_id) || length(selection_id) != 1 ||
        is.na(selection_id) || selection_id == "") {
      stop(
        "Legacy CRF profile components require one non-empty selection_id.",
        call. = FALSE
      )
    }

    return(crfs %>%
      dplyr::transmute(
        component_id = .component_id(
          "crf_profile", selection_id, pollutant, cause, outcome
        ),
        component_type = "crf",
        selection_id = selection_id,
        source = as.character(pollutant),
        cause = as.character(cause),
        outcome = as.character(outcome),
        age_contract = NA_character_,
        readiness_source = "manifest",
        runtime_ready = NA,
        blocking_reason = NA_character_
      ) %>%
      dplyr::distinct())
  }

  registry_shaped <- all(CRF_REGISTRY_REQUIRED_COLUMNS %in% names(crfs))
  if (registry_input || registry_shaped) {
    .validate_runtime_registry_crfs(crfs)
    resolved_selection_id <- selection_id %||%
      if (registry_input) "explicit_registry_set" else "custom_registry"

    return(crfs %>%
      dplyr::transmute(
        component_id = .component_id("crf", crf_id),
        component_type = "crf",
        selection_id = resolved_selection_id,
        source = as.character(pollutant),
        cause = as.character(cause),
        outcome = as.character(outcome),
        age_contract = paste0(age_low, "-", age_high),
        readiness_source = "runtime_validation",
        runtime_ready = TRUE,
        blocking_reason = NA_character_
      ) %>%
      dplyr::distinct())
  }

  resolved_selection_id <- selection_id %||% "custom_legacy_crfs"
  crfs %>%
    dplyr::transmute(
      component_id = .component_id(
        "custom_legacy_crf", pollutant, cause, outcome
      ),
      component_type = "crf",
      selection_id = resolved_selection_id,
      source = as.character(pollutant),
      cause = as.character(cause),
      outcome = as.character(outcome),
      age_contract = NA_character_,
      readiness_source = "runtime_validation",
      runtime_ready = FALSE,
      blocking_reason = paste(
        "Legacy-shaped custom CRFs require explicit Registry-schema",
        "normalisation before shared-engine routing."
      )
    ) %>%
    dplyr::distinct()
}


.normalise_epi_component <- function(epi, required_metric_keys, epi_version) {
  validate_epi_for_crf_check(epi)

  missing_metric_keys <- setdiff(required_metric_keys, get_epi_metric_keys(epi))
  if (length(missing_metric_keys) > 0) {
    stop(
      "Epi data is missing required metric columns: ",
      paste(missing_metric_keys, collapse = ", "),
      call. = FALSE
    )
  }

  missing_estimates <- setdiff(
    CRF_EPI_REQUIRED_ESTIMATES,
    unique(epi$estimate)
  )
  if (length(missing_estimates) > 0) {
    stop(
      "Epi data is missing required estimate rows: ",
      paste(missing_estimates, collapse = ", "),
      call. = FALSE
    )
  }

  missing_metric_estimates <- find_missing_epi_metric_estimates(
    epi = epi,
    metric_keys = required_metric_keys
  )
  if (nrow(missing_metric_estimates) > 0) {
    stop(
      "Epi data has missing values for required metric/estimate combinations.",
      call. = FALSE
    )
  }

  tibble::tibble(
    component_id = .component_id(
      "epi",
      epi_version %||% "custom",
      paste(sort(required_metric_keys), collapse = ",")
    ),
    component_type = "epi",
    selection_id = epi_version %||% "custom",
    source = epi_version %||% "custom",
    cause = NA_character_,
    outcome = paste(sort(required_metric_keys), collapse = ","),
    age_contract = NA_character_,
    readiness_source = "runtime_validation",
    runtime_ready = TRUE,
    blocking_reason = NA_character_
  )
}


.assess_hia_component_readiness <- function(components, manifest) {
  .validate_component_manifest(manifest)

  missing_columns <- setdiff(.hia_component_columns(), names(components))
  if (length(missing_columns) > 0) {
    stop(
      "Requested HIA components are missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  manifest_status <- manifest %>%
    dplyr::select(
      component_id,
      lifecycle,
      readiness_status,
      manifest_blocking_reason = blocking_reason
    )

  assessed <- components %>%
    dplyr::left_join(manifest_status, by = "component_id") %>%
    dplyr::mutate(
      ready = dplyr::case_when(
        readiness_source == "runtime_validation" ~ runtime_ready,
        readiness_source == "manifest" &
          readiness_status == "registry_ready" ~ TRUE,
        TRUE ~ FALSE
      ),
      blocking_reason = dplyr::case_when(
        ready ~ NA_character_,
        readiness_source == "runtime_validation" ~ blocking_reason,
        is.na(readiness_status) ~
          "Component is absent from the readiness manifest.",
        TRUE ~ manifest_blocking_reason
      )
    )

  list(
    ready = nrow(assessed) > 0 && all(assessed$ready),
    components = assessed,
    blocking_components = assessed %>% dplyr::filter(!ready)
  )
}


.resolve_hia_component_readiness <- function(
  crfs,
  rr_sources,
  epi,
  manifest,
  crf_input_mode = c("auto", "legacy_profile", "custom"),
  crf_selection_id = NULL,
  epi_version = "custom",
  rr_loader = get_rr,
  measure_map = get_cause_measure()
) {
  crf_input_mode <- match.arg(crf_input_mode)

  crf_components <- .normalise_crf_components(
    crfs = crfs,
    input_mode = crf_input_mode,
    selection_id = crf_selection_id
  )
  rr_components <- .normalise_rr_components(
    rr_sources = rr_sources,
    rr_loader = rr_loader,
    measure_map = measure_map
  )

  required_metric_keys <- dplyr::bind_rows(
    crf_components,
    rr_components
  ) %>%
    dplyr::filter(!is.na(cause), !is.na(outcome)) %>%
    dplyr::transmute(metric_key = build_metric_key(cause, outcome)) %>%
    dplyr::pull(metric_key) %>%
    unique()

  epi_component <- .normalise_epi_component(
    epi = epi,
    required_metric_keys = required_metric_keys,
    epi_version = epi_version
  )

  components <- dplyr::bind_rows(
    crf_components,
    rr_components,
    epi_component
  )

  .assess_hia_component_readiness(components, manifest = manifest)
}
