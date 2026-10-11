make_readiness_rr_loader <- function() {
  rr_data <- list(
    source_a = tibble::tribble(
      ~cause, ~age, ~source,
      "CauseA", "25-29", "source_a",
      "CauseA", "30-34", "source_a",
      "CauseShared", "25-29", "source_a"
    ),
    source_b = tibble::tribble(
      ~cause, ~age, ~source,
      "CauseB", "Under 5", "source_b",
      "CauseShared", "30-34", "source_b"
    )
  )

  function(source) rr_data[[source]]
}


make_readiness_measure_map <- function() {
  tibble::tribble(
    ~cause, ~measure,
    "CauseA", "Deaths",
    "CauseA", "YLLs",
    "CauseB", "Deaths",
    "CauseShared", "Deaths"
  )
}


make_readiness_epi <- function(metric_keys) {
  result <- tibble::tibble(estimate = c("low", "central", "high"))
  for (metric_key in metric_keys) {
    result[[metric_key]] <- c(1, 2, 3)
  }
  result
}


make_component_manifest <- function(components, not_ready = character()) {
  components %>%
    dplyr::filter(readiness_source == "manifest") %>%
    dplyr::transmute(
      component_id,
      component_type,
      selection_id,
      source,
      cause,
      outcome,
      age_contract,
      lifecycle = "historical_reproduction",
      reproduction_status = ifelse(
        component_id %in% not_ready,
        "not_verified",
        "parity_verified"
      ),
      reproduction_blocking_reason = ifelse(
        component_id %in% not_ready,
        "Historical Registry data is incomplete.",
        NA_character_
      ),
      routing_status = ifelse(
        component_id %in% not_ready,
        "not_ready",
        "ready"
      ),
      routing_blocking_reason = ifelse(
        component_id %in% not_ready,
        "Component is not approved for Registry routing.",
        NA_character_
      ),
      scientific_review_status = "not_reviewed",
      scientific_review_reason = "Method review is pending."
    )
}


test_that("RR source forms normalise to grouped components with age contracts", {
  rr_loader <- make_readiness_rr_loader()
  measure_map <- make_readiness_measure_map()

  preference <- .normalise_rr_components(
    c("source_a", "source_b"),
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  named_list <- .normalise_rr_components(
    list(
      source_a = c("CauseA", "CauseShared"),
      source_b = "CauseB"
    ),
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  data_frame <- .normalise_rr_components(
    tibble::tribble(
      ~cause, ~source,
      "CauseA", "source_a",
      "CauseShared", "source_a",
      "CauseB", "source_b"
    ),
    rr_loader = rr_loader,
    measure_map = measure_map
  )

  identity_columns <- c(
    "component_id", "source", "cause", "outcome", "age_contract"
  )
  expect_equal(
    preference %>% dplyr::select(dplyr::all_of(identity_columns)),
    named_list %>% dplyr::select(dplyr::all_of(identity_columns))
  )
  expect_equal(
    preference %>% dplyr::select(dplyr::all_of(identity_columns)),
    data_frame %>% dplyr::select(dplyr::all_of(identity_columns))
  )
  expect_setequal(
    unique(preference$selection_id),
    "rr_sources:preference_vector"
  )
  expect_true(any(
    preference$cause == "CauseA" &
      preference$outcome == "YLLs" &
      preference$age_contract == "25-29;30-34"
  ))
  expect_true(all(
    preference$component_id == .component_id(
      "rr",
      preference$source,
      preference$cause,
      preference$outcome
    )
  ))
  expect_false(any(
    preference$cause == "CauseShared" &
      preference$source == "source_b"
  ))
})


test_that("RR component normalisation preserves legacy unavailable-pair filtering", {
  rr_loader <- make_readiness_rr_loader()
  measure_map <- make_readiness_measure_map()

  expect_error(
    .normalise_rr_components(
      list(c("CauseA")),
      rr_loader = rr_loader,
      measure_map = measure_map
    ),
    "must be named"
  )
  components <- .normalise_rr_components(
    list(source_a = c("CauseA", "MissingCause")),
    rr_loader = rr_loader,
    measure_map = measure_map
  )

  expect_true(all(components$cause == "CauseA"))
  expect_equal(
    attr(components, "ignored_rr_pairs"),
    tibble::tibble(
      cause = "MissingCause",
      source = "source_a",
      reason = "No curve data exists for this RR source/cause pair."
    )
  )

  expect_error(
    .normalise_rr_components(
      list(source_a = "CauseA"),
      rr_loader = rr_loader,
      measure_map = dplyr::filter(measure_map, cause != "CauseA")
    ),
    "No outcome/measure mapping"
  )

  malformed_loader <- function(source) {
    tibble::tibble(cause = "CauseA", source = source)
  }
  expect_error(
    .normalise_rr_components(
      list(source_a = "CauseA"),
      rr_loader = malformed_loader,
      measure_map = measure_map
    ),
    "missing required columns: age"
  )
})


test_that("actual default RR preference resolves child LRI to GEMM", {
  components <- .normalise_rr_components(c(RR_GEMM, RR_ORIGINAL))
  child_lri <- components %>%
    dplyr::filter(cause == CAUSE_LRICHILD)
  split_adults <- components %>%
    dplyr::filter(grepl("25-29", age_contract, fixed = TRUE))

  expect_gt(nrow(child_lri), 0)
  expect_true(all(child_lri$source == RR_GEMM))
  expect_true(all(child_lri$age_contract == "Under 5"))
  expect_false(any(grepl("25+", split_adults$age_contract, fixed = TRUE)))
})


test_that("actual named RR selection matches legacy unavailable-pair filtering", {
  requested <- list(gemm = c(CAUSE_NCDLRI, CAUSE_DIABETES))
  legacy_pairs <- get_cause_source(requested) %>%
    dplyr::select(cause, source)
  components <- .normalise_rr_components(requested)

  expect_equal(
    sort(unique(paste(components$cause, components$source, sep = "|"))),
    sort(unique(paste(legacy_pairs$cause, legacy_pairs$source, sep = "|")))
  )
  expect_equal(
    attr(components, "ignored_rr_pairs") %>%
      dplyr::select(cause, source),
    tibble::tibble(cause = CAUSE_DIABETES, source = RR_GEMM)
  )
})


test_that("legacy CRF profiles use stable profile-membership identities", {
  legacy_crfs <- tibble::tribble(
    ~pollutant, ~cause, ~outcome,
    "NO2", "CauseA", "Deaths",
    "PM25", "CauseB", "Deaths"
  )

  components <- .normalise_crf_components(
    legacy_crfs,
    input_mode = "legacy_profile",
    selection_id = "legacy_profile_default"
  )

  expect_equal(nrow(components), 2)
  expect_true(all(components$readiness_source == "manifest"))
  expect_setequal(
    components$component_id,
    c(
      "crf_profile|legacy_profile_default|NO2|CauseA|Deaths",
      "crf_profile|legacy_profile_default|PM25|CauseB|Deaths"
    )
  )
})


test_that("current legacy CRF profiles resolve every active membership", {
  expected_rows <- c(
    default = 15,
    C40 = 19,
    `Krewski-South Africa` = 21
  )

  for (profile in names(expected_rows)) {
    components <- .normalise_crf_components(
      get_crfs(profile),
      input_mode = "legacy_profile",
      selection_id = paste0("legacy_profile_", profile)
    )

    expect_equal(nrow(components), unname(expected_rows[[profile]]))
    expect_equal(dplyr::n_distinct(components$component_id), nrow(components))
  }
})


test_that("custom CRF policy distinguishes Registry-shaped and legacy inputs", {
  registry_custom <- tibble::tibble(
    crf_id = "custom_no2_causea_deaths_v1",
    pollutant = "NO2",
    cause = "CauseA",
    outcome = "Deaths",
    double_counted = FALSE,
    age_low = 25,
    age_high = Inf,
    region_applicability = "global",
    form = CRF_FORM_LOG_LINEAR,
    rr_central = 1.05,
    rr_low = 1.02,
    rr_high = 1.08,
    conc_change = 10,
    counterfact = 0,
    unit = "ug",
    units_multiplier = 1,
    data_path = NA_character_,
    reference_id = "custom_reference",
    notes = "Runtime custom CRF"
  )

  registry_components <- .normalise_crf_components(
    registry_custom,
    input_mode = "custom"
  )
  legacy_components <- .normalise_crf_components(
    registry_custom %>% dplyr::select(-crf_id, -form, -age_low, -age_high),
    input_mode = "custom"
  )

  expect_true(all(registry_components$runtime_ready))
  expect_true(all(registry_components$readiness_source == "runtime_validation"))
  expect_false(any(legacy_components$runtime_ready))
  expect_match(
    unique(legacy_components$blocking_reason),
    "Registry-schema normalisation"
  )

  expect_error(
    .normalise_crf_components(
      dplyr::mutate(registry_custom, reference_id = "registry_fixture"),
      input_mode = "custom"
    ),
    "Internal registry fixtures cannot be used"
  )
})


test_that("explicit Registry Sets are runtime-ready components", {
  components <- .normalise_crf_components(crfs_set("gbd2019_deaths"))

  expect_gt(nrow(components), 0)
  expect_true(all(components$readiness_source == "runtime_validation"))
  expect_true(all(components$runtime_ready))
  expect_true(all(grepl("^crf\\|", components$component_id)))
})


test_that("component manifest separates reproduction from scientific review", {
  components <- tibble::tibble(
    component_id = "rr|source_a|CauseA|Deaths",
    component_type = "rr",
    selection_id = "rr_sources:named_list",
    source = "source_a",
    cause = "CauseA",
    outcome = "Deaths",
    age_contract = "25-29",
    lifecycle = "historical_reproduction",
    reproduction_status = "parity_verified",
    reproduction_blocking_reason = NA_character_,
    routing_status = "ready",
    routing_blocking_reason = NA_character_,
    scientific_review_status = "not_reviewed",
    scientific_review_reason = "Method review is pending."
  )

  expect_invisible(.validate_component_manifest(components))
  expect_error(
    .validate_component_manifest(
      dplyr::mutate(
        components,
        lifecycle = "validation_fixture",
        scientific_review_status = "not_applicable",
        scientific_review_reason = NA_character_
      )
    ),
    "fixtures cannot satisfy"
  )
  expect_error(
    .validate_component_manifest(
      dplyr::mutate(
        components,
        reproduction_status = "not_verified",
        routing_status = "not_ready",
        routing_blocking_reason = "Reproduction parity is incomplete."
      )
    ),
    "must have a reproduction_blocking_reason"
  )
  expect_invisible(
    .validate_component_manifest(
      dplyr::mutate(
        components,
        routing_status = "not_ready",
        routing_blocking_reason = "Routing approval is pending."
      )
    )
  )
  expect_error(
    .validate_component_manifest(
      dplyr::mutate(
        components,
        reproduction_status = "not_verified",
        reproduction_blocking_reason = "Parity is incomplete."
      )
    ),
    "must be parity-verified"
  )
  expect_error(
    .validate_component_manifest(
      dplyr::mutate(
        components,
        routing_status = "not_ready"
      )
    ),
    "must have a routing_blocking_reason"
  )
  expect_error(
    .validate_component_manifest(
      dplyr::mutate(
        components,
        scientific_review_reason = NA_character_
      )
    ),
    "must have a scientific_review_reason"
  )
})


test_that("whole-call readiness requires every resolved component", {
  rr_loader <- make_readiness_rr_loader()
  measure_map <- make_readiness_measure_map()
  legacy_crfs <- tibble::tibble(
    pollutant = "NO2",
    cause = "CauseA",
    outcome = "Deaths"
  )
  rr_sources <- list(source_b = "CauseB")
  required_metrics <- c("CauseA_Deaths", "CauseB_Deaths")
  epi <- make_readiness_epi(required_metrics)

  crf_components <- .normalise_crf_components(
    legacy_crfs,
    input_mode = "legacy_profile",
    selection_id = "legacy_profile_default"
  )
  rr_components <- .normalise_rr_components(
    rr_sources,
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  required_components <- dplyr::bind_rows(crf_components, rr_components)
  complete_manifest <- make_component_manifest(required_components)

  ready <- .resolve_hia_component_readiness(
    crfs = legacy_crfs,
    rr_sources = rr_sources,
    epi = epi,
    manifest = complete_manifest,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = "custom",
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  expect_true(ready$ready)
  expect_equal(nrow(ready$blocking_components), 0)
  expect_equal(nrow(ready$ignored_rr_pairs), 0)
  expect_true(any(ready$components$component_type == "epi"))

  rr_id <- rr_components$component_id[[1]]
  mismatched_age_manifest <- complete_manifest %>%
    dplyr::mutate(
      age_contract = dplyr::if_else(
        component_id == rr_id,
        "25+",
        age_contract
      )
    )
  mismatched_age <- .resolve_hia_component_readiness(
    crfs = legacy_crfs,
    rr_sources = rr_sources,
    epi = epi,
    manifest = mismatched_age_manifest,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = "custom",
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  expect_false(mismatched_age$ready)
  expect_equal(mismatched_age$blocking_components$component_id, rr_id)
  expect_match(
    mismatched_age$blocking_components$blocking_reason,
    "age contract does not match"
  )

  missing_manifest <- complete_manifest[-1, , drop = FALSE]
  missing <- .resolve_hia_component_readiness(
    crfs = legacy_crfs,
    rr_sources = rr_sources,
    epi = epi,
    manifest = missing_manifest,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = "custom",
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  expect_false(missing$ready)
  expect_equal(nrow(missing$blocking_components), 1)
  expect_match(
    missing$blocking_components$blocking_reason,
    "absent from the readiness manifest"
  )

  blocked_id <- complete_manifest$component_id[[1]]
  blocked_manifest <- make_component_manifest(
    required_components,
    not_ready = blocked_id
  )
  blocked <- .resolve_hia_component_readiness(
    crfs = legacy_crfs,
    rr_sources = rr_sources,
    epi = epi,
    manifest = blocked_manifest,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = "custom",
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  expect_false(blocked$ready)
  expect_equal(blocked$blocking_components$component_id, blocked_id)

  routing_blocked_manifest <- complete_manifest %>%
    dplyr::mutate(
      routing_status = dplyr::if_else(
        component_id == blocked_id,
        "not_ready",
        routing_status
      ),
      routing_blocking_reason = dplyr::if_else(
        component_id == blocked_id,
        "Routing approval is pending.",
        routing_blocking_reason
      )
    )
  routing_blocked <- .resolve_hia_component_readiness(
    crfs = legacy_crfs,
    rr_sources = rr_sources,
    epi = epi,
    manifest = routing_blocked_manifest,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = "custom",
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  expect_false(routing_blocked$ready)
  expect_equal(
    routing_blocked$blocking_components$component_id,
    blocked_id
  )
  expect_equal(
    routing_blocked$blocking_components$blocking_reason,
    "Routing approval is pending."
  )
})


test_that("readiness resolver reports RR pairs ignored by the legacy selector", {
  rr_loader <- make_readiness_rr_loader()
  measure_map <- make_readiness_measure_map()
  legacy_crfs <- tibble::tibble(
    pollutant = "NO2",
    cause = "CauseA",
    outcome = "Deaths"
  )
  rr_sources <- list(source_a = c("CauseA", "MissingCause"))
  epi <- make_readiness_epi(c("CauseA_Deaths", "CauseA_YLLs"))

  crf_components <- .normalise_crf_components(
    legacy_crfs,
    input_mode = "legacy_profile",
    selection_id = "legacy_profile_default"
  )
  rr_components <- .normalise_rr_components(
    list(source_a = "CauseA"),
    rr_loader = rr_loader,
    measure_map = measure_map
  )
  manifest <- make_component_manifest(
    dplyr::bind_rows(crf_components, rr_components)
  )

  resolved <- .resolve_hia_component_readiness(
    crfs = legacy_crfs,
    rr_sources = rr_sources,
    epi = epi,
    manifest = manifest,
    crf_input_mode = "legacy_profile",
    crf_selection_id = "legacy_profile_default",
    epi_version = "custom",
    rr_loader = rr_loader,
    measure_map = measure_map
  )

  expect_true(resolved$ready)
  expect_equal(nrow(resolved$blocking_components), 0)
  expect_equal(
    resolved$ignored_rr_pairs %>% dplyr::select(cause, source),
    tibble::tibble(cause = "MissingCause", source = "source_a")
  )
  expect_match(
    resolved$ignored_rr_pairs$reason,
    "No curve data exists"
  )
})


test_that("custom epi is validated against resolved component metrics", {
  components <- tibble::tibble(
    pollutant = "NO2",
    cause = "CauseA",
    outcome = "Deaths"
  )
  crf_components <- .normalise_crf_components(
    components,
    input_mode = "legacy_profile",
    selection_id = "legacy_profile_default"
  )
  manifest <- make_component_manifest(crf_components)

  expect_error(
    .resolve_hia_component_readiness(
      crfs = components,
      rr_sources = tibble::tibble(cause = character(), source = character()),
      epi = make_readiness_epi("DifferentCause_Deaths"),
      manifest = manifest,
      crf_input_mode = "legacy_profile",
      crf_selection_id = "legacy_profile_default",
      epi_version = "custom",
      rr_loader = make_readiness_rr_loader(),
      measure_map = make_readiness_measure_map()
    ),
    "missing required metric columns"
  )
})
