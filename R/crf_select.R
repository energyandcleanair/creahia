CRF_SELECTION_REQUIRED_COLUMNS <- c("pollutant", "cause", "outcome", "crf_id")
CRF_PRESET_REQUIRED_COLUMNS <- c("pollutant", "cause", "outcome", "crf_id", "notes")

available_crf_presets <- function() {
  preset_dir <- get_hia_path("crf/presets", error_if_not_exists = TRUE)
  preset_files <- list.files(preset_dir, pattern = "\\.csv$", full.names = FALSE)

  sort(sub("\\.csv$", "", preset_files))
}

load_crf_preset <- function(name = "experimental_default") {
  path <- get_hia_path(
    file.path("crf/presets", paste0(name, ".csv")),
    error_if_not_exists = TRUE
  )

  preset <- readr::read_csv(path, col_types = readr::cols())

  missing_cols <- setdiff(CRF_PRESET_REQUIRED_COLUMNS, names(preset))
  if (length(missing_cols) > 0) {
    stop(
      "CRF preset is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  preset
}

crfs_preset <- function(name = "experimental_default", registry = load_crf_registry()) {
  available_presets <- available_crf_presets()

  if (!name %in% available_presets) {
    stop(
      "Unknown CRF preset: ", name,
      ". Available presets: ", paste(available_presets, collapse = ", "),
      call. = FALSE
    )
  }

  selection <- load_crf_preset(name) %>%
    dplyr::select(dplyr::all_of(CRF_SELECTION_REQUIRED_COLUMNS))


  resolve_crf_selection(selection, registry = registry)
  
}

resolve_crf_selection <- function(selection, registry = load_crf_registry()) {
  missing_cols <- setdiff(CRF_SELECTION_REQUIRED_COLUMNS, names(selection))
  if (length(missing_cols) > 0) {
    stop(
      "CRF selection is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  unknown_ids <- setdiff(selection$crf_id, registry$crf_id)
  if (length(unknown_ids) > 0) {
    stop(
      "Unknown crf_id values: ",
      paste(unknown_ids, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_keys <- selection %>%
    dplyr::count(pollutant, cause, outcome) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicated_keys) > 0) {
    stop(
      "Each pollutant/cause/outcome triplet must select exactly one CRF source. Duplicated triplets: ",
      paste(
        paste(duplicated_keys$pollutant, duplicated_keys$cause, duplicated_keys$outcome, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  selected <- registry %>%
    dplyr::inner_join(
      selection,
      by = c("pollutant", "cause", "outcome", "crf_id")
    )

  missing_matches <- dplyr::anti_join(
    selection,
    registry,
    by = c("pollutant", "cause", "outcome", "crf_id")
  )

  if (nrow(missing_matches) > 0) {
    stop(
      "Some selections do not match registry pollutant/cause/outcome/crf_id rows: ",
      paste(
        paste(missing_matches$pollutant, missing_matches$cause, missing_matches$outcome, missing_matches$crf_id, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  validate_crf_selection(selected)

  selected
}

crfs_override <- function(crfs, pollutant, cause, outcome, crf_id, registry = load_crf_registry()) {
  replacement <- resolve_crf_selection(
    tibble::tibble(
      pollutant = pollutant,
      cause = cause,
      outcome = outcome,
      crf_id = crf_id
    ),
    registry = registry
  )

  updated <- crfs %>%
    dplyr::filter(!(pollutant == !!pollutant & cause == !!cause & outcome == !!outcome)) %>%
    dplyr::bind_rows(replacement)

  validate_crf_selection(updated)

  updated
}

validate_crf_selection <- function(
  crfs,
  check_double_counting = FALSE,
  dc_groups = load_dc_groups()
) {
  if (nrow(crfs) == 0) {
    stop("CRF selection is empty.", call. = FALSE)
  }

  duplicated_ids <- crfs$crf_id[duplicated(crfs$crf_id)]
  if (length(duplicated_ids) > 0) {
    stop(
      "CRF selection contains duplicated crf_id values: ",
      paste(unique(duplicated_ids), collapse = ", "),
      call. = FALSE
    )
  }

  source_count <- crfs %>%
    dplyr::distinct(pollutant, cause, outcome, crf_id) %>%
    dplyr::count(pollutant, cause, outcome) %>%
    dplyr::filter(n > 1)

  if (nrow(source_count) > 0) {
    stop(
      "CRF selection has multiple sources for the same pollutant/cause/outcome triplet: ",
      paste(
        paste(source_count$pollutant, source_count$cause, source_count$outcome, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (check_double_counting) {
    check_dc_conflicts(crfs, dc_groups = dc_groups)
  }

  invisible(TRUE)
}


describe_crf_preset <- function(name, registry = load_crf_registry()) {
  available_presets <- available_crf_presets()

  if (!name %in% available_presets) {
    stop(
      "Unknown CRF preset: ", name,
      ". Available presets: ", paste(available_presets, collapse = ", "),
      call. = FALSE
    )
  }

  preset <- load_crf_preset(name) %>%
    dplyr::rename(preset_notes = notes)

  described <- registry %>%
    dplyr::inner_join(
      preset,
      by = c("pollutant", "cause", "outcome", "crf_id")
    )

  missing_matches <- dplyr::anti_join(
    preset,
    registry,
    by = c("pollutant", "cause", "outcome", "crf_id")
  )

  if (nrow(missing_matches) > 0) {
    stop(
      "Some preset rows do not match registry pollutant/cause/outcome/crf_id rows: ",
      paste(
        paste(
          missing_matches$pollutant,
          missing_matches$cause,
          missing_matches$outcome,
          missing_matches$crf_id,
          sep = "/"
        ),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  described %>%
    dplyr::mutate(preset = name) %>%
    dplyr::select(
      preset,
      pollutant,
      cause,
      outcome,
      crf_id,
      reference_id,
      form,
      notes = preset_notes
    )
}

search_crf_registry <- function(
  pollutant = NULL,
  cause = NULL,
  outcome = NULL,
  reference_id = NULL,
  form = NULL,
  query = NULL,
  registry = load_crf_registry()
) {
  
  result <- registry
  
  filters <- list(
    pollutant = pollutant,
    cause = cause,
    outcome = outcome,
    reference_id = reference_id,
    form = form
  )

  for (name in names(filters)) {
    value <- filters[[name]]

    if (!is.null(value)) {
      result <- dplyr::filter(
        result,
        .data[[name]] %in% value
      )
    }
  }

  if (!is.null(query)) {
    query_terms <- as.character(query)

    result <- result %>%
      dplyr::filter(
        crf_filter_matches(.data$crf_id, query_terms) |
          crf_filter_matches(.data$pollutant, query_terms) |
          crf_filter_matches(.data$cause, query_terms) |
          crf_filter_matches(.data$outcome, query_terms) |
          crf_filter_matches(.data$reference_id, query_terms) |
          crf_filter_matches(.data$form, query_terms) |
          crf_filter_matches(.data$notes, query_terms)
      )
  }

  result %>%
    dplyr::select(
      pollutant,
      cause,
      outcome,
      crf_id,
      reference_id,
      form,
      notes
    )
}

available_crf_references <- function(references = load_crf_references()) {
  references %>%
    dplyr::select(
      reference_id,
      author,
      year,
      title,
      notes
    ) %>%
    dplyr::arrange(reference_id)
}

crf_override_options <- function(
  presets,
  pollutant = NULL,
  cause = NULL,
  outcome = NULL,
  registry = load_crf_registry()
) {
  if (missing(presets) || length(presets) == 0) {
    stop("`presets` must contain at least one CRF preset name.", call. = FALSE)
  }

  filters <- list(
    pollutant = pollutant,
    cause = cause,
    outcome = outcome
  )

  invalid_filters <- names(filters)[
    vapply(
      filters,
      function(value) {
        !is.null(value) && (
          length(value) == 0 ||
            any(is.na(value)) ||
            any(value == "")
        )
      },
      logical(1)
    )
  ]

  if (length(invalid_filters) > 0) {
    stop(
      "CRF override option filters must be non-empty when provided: ",
      paste(invalid_filters, collapse = ", "),
      call. = FALSE
    )
  }

  available_presets <- available_crf_presets()
  unknown_presets <- setdiff(presets, available_presets)

  if (length(unknown_presets) > 0) {
    stop(
      "Unknown CRF preset: ",
      paste(unknown_presets, collapse = ", "),
      ". Available presets: ",
      paste(available_presets, collapse = ", "),
      call. = FALSE
    )
  }

  selected <- dplyr::bind_rows(
    lapply(
      presets,
      describe_crf_preset,
      registry = registry
    )
  )

  matching_selected <- selected

  for (name in names(filters)) {
    value <- filters[[name]]

    if (!is.null(value)) {
      matching_selected <- matching_selected %>%
        dplyr::filter(crf_filter_matches(.data[[name]], value))
    }
  }

  slot_cols <- c("pollutant", "cause", "outcome")

  matching_slots <- matching_selected %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(slot_cols)))

  selected_by_crf <- matching_selected %>%
    dplyr::group_by(
      pollutant,
      cause,
      outcome,
      crf_id
    ) %>%
    dplyr::summarise(
      selected_by_preset = paste(unique(.data$preset), collapse = ", "),
      .groups = "drop"
    )

  search_crf_registry(registry = registry) %>%
    dplyr::semi_join(
      matching_slots,
      by = slot_cols
    ) %>%
    dplyr::left_join(
      selected_by_crf,
      by = c("pollutant", "cause", "outcome", "crf_id")
    ) %>%
    dplyr::mutate(
      selected = !is.na(.data$selected_by_preset)
    ) %>%
    dplyr::arrange(
      .data$pollutant,
      .data$cause,
      .data$outcome,
      dplyr::desc(.data$selected),
      .data$reference_id,
      .data$crf_id
    ) %>%
    dplyr::select(
      pollutant,
      cause,
      outcome,
      selected,
      selected_by_preset,
      crf_id,
      reference_id,
      form,
      notes
    )
}

crf_filter_matches <- function(values, filters) {
  values_lower <- tolower(as.character(values))
  filters_lower <- tolower(as.character(filters))

  Reduce(
    `|`,
    lapply(
      filters_lower,
      function(filter) {
        grepl(filter, values_lower, fixed = TRUE)
      }
    )
  )
}


preview_crf_set <- function(
  presets,
  add = NULL,
  remove = NULL,
  replace = NULL,
  registry = load_crf_registry()
) {
  if (missing(presets) || length(presets) == 0) {
    stop("`presets` must contain at least one CRF preset name.", call. = FALSE)
  }

  selected <- dplyr::bind_rows(
    lapply(
      presets,
      describe_crf_preset,
      registry = registry
    )
  )

  validate_preview_slots(selected)

  preview <- selected %>%
    dplyr::group_by(
      pollutant,
      cause,
      outcome,
      crf_id,
      reference_id,
      form,
      notes
    ) %>%
    dplyr::summarise(
      selected_by_preset = paste(unique(.data$preset), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(action = "selected")

  if (!is.null(remove)) {
    preview <- apply_crf_preview_removals(
      preview = preview,
      remove = remove
    )
  }

  if (!is.null(add)) {
    preview <- apply_crf_preview_additions(
      preview = preview,
      add = add,
      registry = registry
    )
  }

  if (!is.null(replace)) {
    preview <- apply_crf_preview_replacements(
      preview = preview,
      replace = replace,
      registry = registry
    )
  }

  preview %>%
    dplyr::arrange(
      pollutant,
      cause,
      outcome,
      reference_id,
      crf_id
    ) %>%
    dplyr::select(
      pollutant,
      cause,
      outcome,
      action,
      crf_id,
      reference_id,
      form,
      selected_by_preset,
      notes
    )
}

validate_preview_slots <- function(selected) {
  slot_cols <- c("pollutant", "cause", "outcome")

  conflicts <- selected %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(slot_cols)),
      crf_id
    ) %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(slot_cols)),
      name = "n_crfs"
    ) %>%
    dplyr::filter(.data$n_crfs > 1)

  if (nrow(conflicts) > 0) {
    stop(
      "Selected presets contain conflicting CRFs for the same pollutant/cause/outcome slot: ",
      paste(
        paste(conflicts$pollutant, conflicts$cause, conflicts$outcome, sep = "/"),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

apply_crf_preview_removals <- function(preview, remove) {
  if (!is.list(remove) || length(remove) == 0) {
    stop("`remove` must be a non-empty list of removal entries.", call. = FALSE)
  }

  for (removal in remove) {
    removal_slot <- resolve_crf_removal(removal)

    matched_rows <- preview %>%
      dplyr::filter(
        .data$pollutant == removal_slot$pollutant,
        .data$cause == removal_slot$cause,
        .data$outcome == removal_slot$outcome
      )

    if (nrow(matched_rows) == 0) {
      stop(
        "Cannot remove a slot that is not selected by the current presets: ",
        paste(removal_slot$pollutant, removal_slot$cause, removal_slot$outcome, sep = "/"),
        call. = FALSE
      )
    }

    preview <- preview %>%
      dplyr::mutate(
        action = dplyr::if_else(
          .data$pollutant == removal_slot$pollutant &
            .data$cause == removal_slot$cause &
            .data$outcome == removal_slot$outcome,
          "removed",
          .data$action
        )
      )
  }

  preview
}

resolve_crf_removal <- function(removal) {
  if (!is.list(removal)) {
    stop("Each `remove` entry must be a list.", call. = FALSE)
  }

  required_cols <- c("pollutant", "cause", "outcome")
  missing_cols <- required_cols[
    !vapply(required_cols, function(col) !is.null(removal[[col]]), logical(1))
  ]

  if (length(missing_cols) > 0) {
    stop(
      "Removal entries must include: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  tibble::tibble(
    pollutant = removal$pollutant,
    cause = removal$cause,
    outcome = removal$outcome
  )
}

apply_crf_preview_additions <- function(preview, add, registry) {
  if (!is.list(add) || length(add) == 0) {
    stop("`add` must be a non-empty list of addition entries.", call. = FALSE)
  }

  for (addition in add) {
    addition_row <- resolve_crf_addition(addition, registry = registry)

    slot_exists <- preview %>%
      dplyr::filter(
        .data$pollutant == addition_row$pollutant,
        .data$cause == addition_row$cause,
        .data$outcome == addition_row$outcome
      )

    if (nrow(slot_exists) > 0) {
      stop(
        "Cannot add a slot that is already selected by the current preview. Use `replace` instead: ",
        paste(addition_row$pollutant, addition_row$cause, addition_row$outcome, sep = "/"),
        call. = FALSE
      )
    }

    preview <- preview %>%
      dplyr::bind_rows(
        addition_row %>%
          dplyr::mutate(
            action = "added",
            selected_by_preset = NA_character_
          ) %>%
          dplyr::select(
            pollutant,
            cause,
            outcome,
            crf_id,
            reference_id,
            form,
            notes,
            selected_by_preset,
            action
          )
      )
  }

  preview
}

resolve_crf_addition <- function(addition, registry) {
  if (!is.list(addition)) {
    stop("Each `add` entry must be a list.", call. = FALSE)
  }

  if (!is.null(addition$crf_id)) {
    addition_row <- registry %>%
      dplyr::filter(.data$crf_id == addition$crf_id)

    if (nrow(addition_row) == 0) {
      stop("Unknown addition crf_id: ", addition$crf_id, call. = FALSE)
    }

    if (nrow(addition_row) > 1) {
      stop("Addition crf_id must identify exactly one registry row.", call. = FALSE)
    }

    return(addition_row)
  }

  required_cols <- c("pollutant", "cause", "outcome", "reference_id")
  missing_cols <- required_cols[
    !vapply(required_cols, function(col) !is.null(addition[[col]]), logical(1))
  ]

  if (length(missing_cols) > 0) {
    stop(
      "Source-based addition entries must include: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  addition_row <- registry %>%
    dplyr::filter(
      .data$pollutant == addition$pollutant,
      .data$cause == addition$cause,
      .data$outcome == addition$outcome,
      .data$reference_id == addition$reference_id
    )

  if (nrow(addition_row) == 0) {
    stop(
      "No registry CRF matches addition request: ",
      paste(
        addition$pollutant,
        addition$cause,
        addition$outcome,
        addition$reference_id,
        sep = "/"
      ),
      call. = FALSE
    )
  }

  if (nrow(addition_row) > 1) {
    stop(
      "Addition request matches multiple registry CRFs. Use `crf_id` to disambiguate.",
      call. = FALSE
    )
  }

  addition_row
}

apply_crf_preview_replacements <- function(preview, replace, registry) {
  if (!is.list(replace) || length(replace) == 0) {
    stop("`replace` must be a non-empty list of replacement entries.", call. = FALSE)
  }

  for (replacement in replace) {
    replacement_row <- resolve_crf_replacement(replacement, registry = registry)

    slot_exists <- preview %>%
      dplyr::filter(
        .data$pollutant == replacement_row$pollutant,
        .data$cause == replacement_row$cause,
        .data$outcome == replacement_row$outcome,
        .data$action != "removed"
      )

    if (nrow(slot_exists) == 0) {
      stop(
        "Cannot replace a slot that is not selected by the current presets: ",
        paste(replacement_row$pollutant, replacement_row$cause, replacement_row$outcome, sep = "/"),
        call. = FALSE
      )
    }

    preview <- preview %>%
      dplyr::filter(
        !(
          .data$pollutant == replacement_row$pollutant &
            .data$cause == replacement_row$cause &
            .data$outcome == replacement_row$outcome
        )
      ) %>%
      dplyr::bind_rows(
        replacement_row %>%
          dplyr::mutate(
            action = "replaced",
            selected_by_preset = NA_character_
          ) %>%
          dplyr::select(
            pollutant,
            cause,
            outcome,
            crf_id,
            reference_id,
            form,
            notes,
            selected_by_preset,
            action
          )
      )
  }

  preview
}

resolve_crf_replacement <- function(replacement, registry) {
  if (!is.list(replacement)) {
    stop("Each `replace` entry must be a list.", call. = FALSE)
  }

  if (!is.null(replacement$crf_id)) {
    replacement_row <- registry %>%
      dplyr::filter(.data$crf_id == replacement$crf_id)

    if (nrow(replacement_row) == 0) {
      stop("Unknown replacement crf_id: ", replacement$crf_id, call. = FALSE)
    }

    if (nrow(replacement_row) > 1) {
      stop("Replacement crf_id must identify exactly one registry row.", call. = FALSE)
    }

    return(replacement_row)
  }

  required_cols <- c("pollutant", "cause", "outcome", "reference_id")
  missing_cols <- required_cols[
    !vapply(required_cols, function(col) !is.null(replacement[[col]]), logical(1))
  ]

  if (length(missing_cols) > 0) {
    stop(
      "Source-based replacement entries must include: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  replacement_row <- registry %>%
    dplyr::filter(
      .data$pollutant == replacement$pollutant,
      .data$cause == replacement$cause,
      .data$outcome == replacement$outcome,
      .data$reference_id == replacement$reference_id
    )

  if (nrow(replacement_row) == 0) {
    stop(
      "No registry CRF matches replacement request: ",
      paste(
        replacement$pollutant,
        replacement$cause,
        replacement$outcome,
        replacement$reference_id,
        sep = "/"
      ),
      call. = FALSE
    )
  }

  if (nrow(replacement_row) > 1) {
    stop(
      "Replacement request matches multiple registry CRFs. Use `crf_id` to disambiguate.",
      call. = FALSE
    )
  }

  replacement_row
}

crfs_set <- function(
  presets,
  add = NULL,
  remove = NULL,
  replace = NULL,
  registry = load_crf_registry()
) {
  preview <- preview_crf_set(
    presets = presets,
    add = add,
    remove = remove,
    replace = replace,
    registry = registry
  )

  selection <- preview %>%
    dplyr::filter(.data$action != "removed") %>%
    dplyr::select(dplyr::all_of(CRF_SELECTION_REQUIRED_COLUMNS))

  new_crfs_set(resolve_crf_selection(selection, registry = registry))
}

new_crfs_set <- function(crfs) {
  class(crfs) <- unique(c("creahia_crf_set", class(crfs)))
  crfs
}

is_crfs_set <- function(crfs) {
  inherits(crfs, "creahia_crf_set")
}
