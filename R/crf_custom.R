CRF_CUSTOM_CLASS <- "creahia_custom_crf"
CRF_CUSTOM_ID_PREFIX <- "custom_"
CRF_CUSTOM_UNITS <- c("ug", "ppb")

# Build a user-defined log-linear CRF for crfs_set(add = ...) or
# crfs_set(replace = ...), e.g. for a sensitivity run. The row is not part of
# the Registry and has not been reviewed: it never appears in presets or
# search_crf_registry().
#
# rr: c(central, low, high), or named in any order.
# per: concentration change the RR refers to (conc_change), in `unit`.
# counterfactual: concentration below which no effect is counted, in `unit`.
# units_multiplier: converts ug/m3 concentrations to `unit`. Defaults to the
#   value the Registry uses for that pollutant and unit.
# age_low, age_high: metadata only. The baseline rates come from the epi data.
# source: free text describing where the values come from, kept in notes.
crf_log_linear <- function(
  pollutant,
  cause,
  outcome,
  rr,
  per,
  counterfactual,
  unit = "ug",
  units_multiplier = NULL,
  age_low = NA_real_,
  age_high = NA_real_,
  id = NULL,
  source = NULL,
  registry = load_crf_registry()
) {
  for (arg in c("pollutant", "cause", "outcome", "unit")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1 || is.na(value) || value == "") {
      stop("`", arg, "` must be a single non-empty string.", call. = FALSE)
    }
  }

  pollutant <- gsub("PM2\\.5", "PM25", pollutant)

  if (!unit %in% CRF_CUSTOM_UNITS) {
    stop(
      "`unit` must be one of: ", paste(CRF_CUSTOM_UNITS, collapse = ", "), ".",
      call. = FALSE
    )
  }

  for (arg in c("per", "counterfactual")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
      stop("`", arg, "` must be a single number.", call. = FALSE)
    }
  }

  if (per <= 0) {
    stop("`per` must be positive.", call. = FALSE)
  }

  rr <- parse_custom_rr(rr)

  if (is.null(units_multiplier)) {
    units_multiplier <- default_units_multiplier(pollutant, unit, registry = registry)
  }

  if (is.null(id)) {
    id <- paste(pollutant, cause, outcome, sep = "_")
  }
  id <- tolower(gsub("[^A-Za-z0-9]+", "_", id))
  if (!startsWith(id, CRF_CUSTOM_ID_PREFIX)) {
    id <- paste0(CRF_CUSTOM_ID_PREFIX, id)
  }

  notes <- "Custom CRF, not part of the Registry and not reviewed"
  if (!is.null(source)) {
    notes <- paste0(notes, ": ", source)
  }

  crf <- tibble::tibble(
    crf_id = id,
    pollutant = pollutant,
    cause = cause,
    outcome = outcome,
    age_low = as.numeric(age_low),
    age_high = as.numeric(age_high),
    region_applicability = "global",
    form = CRF_FORM_LOG_LINEAR,
    rr_central = rr[["central"]],
    rr_low = rr[["low"]],
    rr_high = rr[["high"]],
    conc_change = per,
    counterfact = counterfactual,
    unit = unit,
    units_multiplier = units_multiplier,
    data_path = NA_character_,
    reference_id = NA_character_,
    notes = notes
  )

  validate_log_linear_crfs(crf)

  class(crf) <- unique(c(CRF_CUSTOM_CLASS, class(crf)))
  crf
}

is_custom_crf <- function(x) {
  inherits(x, CRF_CUSTOM_CLASS)
}

parse_custom_rr <- function(rr) {
  if (!is.numeric(rr) || length(rr) != 3 || any(is.na(rr))) {
    stop("`rr` must be three numbers: central, low and high.", call. = FALSE)
  }

  rr_names <- c("central", "low", "high")
  if (is.null(names(rr))) {
    names(rr) <- rr_names
  } else if (!setequal(names(rr), rr_names)) {
    stop("`rr` names must be central, low and high.", call. = FALSE)
  }

  if (!(rr[["low"]] <= rr[["central"]] && rr[["central"]] <= rr[["high"]])) {
    stop("`rr` must satisfy low <= central <= high.", call. = FALSE)
  }

  rr
}

default_units_multiplier <- function(pollutant, unit, registry) {
  if (unit == "ug") {
    return(1)
  }

  multipliers <- registry %>%
    dplyr::filter(
      .data$form == CRF_FORM_LOG_LINEAR,
      .data$pollutant == !!pollutant,
      .data$unit == !!unit
    ) %>%
    dplyr::distinct(.data$units_multiplier) %>%
    dplyr::pull()

  if (length(multipliers) != 1) {
    stop(
      "No single Registry units_multiplier for ", pollutant, " in ", unit,
      ". Provide `units_multiplier` (the factor converting ug/m3 to ", unit, ").",
      call. = FALSE
    )
  }

  multipliers
}

# Append the custom CRFs found in `entries` to `registry`, and return both the
# extended registry and the entries with each custom CRF replaced by its crf_id.
register_custom_crfs <- function(entries, registry) {
  if (is.null(entries)) {
    return(list(entries = NULL, registry = registry))
  }

  if (is_custom_crf(entries)) {
    entries <- list(entries)
  }

  if (!is.list(entries) || is.data.frame(entries)) {
    return(list(entries = entries, registry = registry))
  }

  for (i in seq_along(entries)) {
    crf <- entries[[i]]
    if (!is_custom_crf(crf)) {
      next
    }

    crf <- tibble::as_tibble(crf)
    existing <- registry %>%
      dplyr::filter(.data$crf_id == crf$crf_id)

    if (nrow(existing) > 0) {
      # The same custom CRF can be passed again (crfs_set() hands its entries
      # to preview_crf_set()), but two different CRFs cannot share an id.
      if (!isTRUE(all.equal(
            as.data.frame(existing[names(crf)]),
            as.data.frame(crf),
            check.attributes = FALSE
          ))) {
        stop(
          "Custom CRF id '", crf$crf_id, "' is already used by another CRF. ",
          "Give it a different `id`.",
          call. = FALSE
        )
      }
    } else {
      registry <- dplyr::bind_rows(registry, crf)
    }

    entries[[i]] <- list(crf_id = crf$crf_id)
  }

  list(entries = entries, registry = registry)
}

warn_custom_crf_overlaps <- function(crfs, dc_groups = load_dc_groups()) {
  custom <- crfs %>%
    dplyr::filter(startsWith(.data$crf_id, CRF_CUSTOM_ID_PREFIX))

  if (nrow(custom) == 0) {
    return(invisible(crfs))
  }

  conflicts <- find_dc_conflicts(crfs, dc_groups = dc_groups)
  if (nrow(conflicts) == 0) {
    return(invisible(crfs))
  }

  conflicts <- conflicts %>%
    dplyr::semi_join(custom, by = c("pollutant", "outcome")) %>%
    dplyr::filter(purrr::map2_lgl(
      .data$aggregates, .data$components,
      function(aggregates, components) {
        any(c(aggregates, components) %in% custom$cause)
      }
    ))

  if (nrow(conflicts) == 0) {
    return(invisible(crfs))
  }

  messages <- purrr::pmap_chr(
    conflicts,
    function(pollutant, outcome, aggregates, components, ...) {
      if (length(components) == 0) {
        return(paste0(
          pollutant, "/", outcome, ": ",
          paste(aggregates, collapse = " and "), " are alternative totals"
        ))
      }
      paste0(
        pollutant, "/", outcome, ": ",
        paste(aggregates, collapse = ", "), " includes ",
        paste(components, collapse = ", ")
      )
    }
  )

  warning(
    "Custom CRFs overlap other selected causes, so their impacts would be counted twice:\n",
    paste0("  - ", unique(messages), collapse = "\n"),
    "\nUse `remove` to drop the overlapping rows.",
    call. = FALSE
  )

  invisible(crfs)
}
