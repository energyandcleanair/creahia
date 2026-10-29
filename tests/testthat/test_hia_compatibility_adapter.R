make_adapter_test_context <- function(...) {
  defaults <- list(
    rr_sources_supplied = FALSE,
    epi_supplied = FALSE,
    crfs_supplied = FALSE,
    epi_version = "default",
    crfs_version = "default",
    crf_compute = "legacy",
    rr_selection_mode = "rr_sources",
    calc_causes = NULL,
    gbd_causes = NULL
  )
  utils::modifyList(defaults, list(...))
}


make_adapter_test_population_year <- function(
  pop_year = NULL,
  scale_base_year = NULL,
  scale_target_year = NULL,
  resolved_pop_year = pop_year
) {
  .capture_population_year_intent(
    pop_year = pop_year,
    scale_base_year = scale_base_year,
    scale_target_year = scale_target_year,
    resolved_pop_year = resolved_pop_year
  )
}


make_adapter_route_observation <- function(ready = FALSE) {
  readiness <- make_specification_readiness()
  if (ready) {
    readiness$components <- readiness$components %>%
      dplyr::mutate(
        ready = TRUE,
        blocking_reason = NA_character_,
        reproduction_status = dplyr::if_else(
          component_type == "epi",
          NA_character_,
          "parity_verified"
        ),
        routing_status = dplyr::if_else(
          component_type == "epi",
          NA_character_,
          "ready"
        )
      )
    readiness$ready <- TRUE
    readiness$blocking_components <- readiness$components[0, , drop = FALSE]
  }
  specification <- .new_hia_specification(
    readiness = readiness,
    population_year_intent = make_adapter_test_population_year(2020),
    input_contract = paste(
      "component_resolved",
      "legacy_profile_default",
      "legacy_cause_selectors",
      "gbd2019",
      sep = "|"
    ),
    epi_version = "gbd2019"
  )

  list(
    status = "resolved",
    input_contract = specification$input_contract,
    result = readiness,
    specification = specification
  )
}


test_that("RR selection boundary preserves legacy translation and precedence", {
  seen <- new.env(parent = emptyenv())
  explicit_rr_sources <- list(custom = "IgnoredCause")
  expected <- convert_old_parameters_to_rr_sources(
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  with_mocked_bindings(
    log_info = function(message) seen$message <- message,
    {
      translated <- .resolve_hia_rr_selection_boundary(
        rr_sources = explicit_rr_sources,
        calc_causes = "GEMM and GBD",
        gbd_causes = NULL
      )
    }
  )

  expect_equal(translated$mode, "legacy_cause_selectors")
  expect_equal(translated$calc_causes, "GEMM and GBD")
  expect_equal(translated$gbd_causes, "default")
  expect_identical(translated$rr_sources, expected)
  expect_equal(
    seen$message,
    "Using old parameters for calc_causes and gbd_causes. Ignoring rr_sources."
  )
  expect_false(identical(translated$rr_sources, explicit_rr_sources))
})


test_that("RR selection boundary preserves explicit legacy outcomes", {
  calc_causes <- c(
    "Stroke_YLDs",
    "Diabetes_YLDs",
    "COPD_YLDs",
    "NCD.LRI_Deaths",
    "IHD_Deaths",
    "Stroke_Deaths",
    "COPD_Deaths",
    "LC_Deaths",
    "LRI_Deaths",
    "LRI.child_Deaths",
    "Diabetes_Deaths"
  )

  translated <- .resolve_hia_rr_selection_boundary(
    rr_sources = c(RR_GEMM, RR_ORIGINAL),
    calc_causes = calc_causes,
    gbd_causes = "default"
  )

  expect_equal(translated$mode, "legacy_cause_selectors")
  expect_identical(translated$calc_causes, calc_causes)
  expect_equal(nrow(translated$rr_sources), length(calc_causes))
  expect_setequal(
    paste(translated$rr_sources$cause, translated$rr_sources$outcome, sep = "_"),
    calc_causes
  )
  expect_equal(
    translated$rr_sources$source[
      translated$rr_sources$cause == CAUSE_DIABETES
    ],
    rep(RR_ORIGINAL, 2)
  )
  expect_equal(
    translated$rr_sources$source[
      translated$rr_sources$cause == CAUSE_LRICHILD
    ],
    RR_ORIGINAL
  )
  expect_equal(
    translated$rr_sources$source[
      translated$rr_sources$cause == CAUSE_NCDLRI
    ],
    RR_GEMM
  )
})


test_that("RR selection boundary preserves direct and invalid inputs", {
  direct <- list(gemm = "NCD.LRI")
  unchanged <- .resolve_hia_rr_selection_boundary(
    rr_sources = direct,
    calc_causes = NULL,
    gbd_causes = NULL
  )

  expect_equal(unchanged$mode, "rr_sources")
  expect_identical(unchanged$rr_sources, direct)
  expect_error(
    .resolve_hia_rr_selection_boundary(
      rr_sources = direct,
      calc_causes = "GBD only",
      gbd_causes = "all"
    ),
    "Invalid combination of gbd_causes and calc_causes"
  )
})


test_that("compatibility boundary emits the canonical default specification", {
  readiness <- make_specification_readiness()

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      resolved <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = make_adapter_test_context(),
        population_year_intent = make_adapter_test_population_year()
      )
    }
  )

  expect_equal(resolved$status, "resolved")
  expect_equal(
    resolved$input_contract,
    "component_resolved|legacy_profile_default|rr_sources|default"
  )
  expect_identical(resolved$result, readiness)
  expect_s3_class(
    resolved$specification,
    "creahia_hia_specification"
  )
  expect_equal(
    resolved$specification$input_contract,
    "component_resolved|legacy_profile_default|rr_sources|default"
  )
})


test_that("compatibility boundary emits the first legacy selector specification", {
  readiness <- make_specification_readiness()
  context <- make_adapter_test_context(
    rr_sources_supplied = TRUE,
    rr_selection_mode = "legacy_cause_selectors",
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      resolved <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = context,
        population_year_intent = make_adapter_test_population_year()
      )
    }
  )

  expect_equal(resolved$status, "resolved")
  expect_equal(
    resolved$input_contract,
    paste(
      "component_resolved",
      "legacy_profile_default",
      "legacy_cause_selectors",
      "default",
      sep = "|"
    )
  )
  expect_equal(
    resolved$specification$input_contract,
    resolved$input_contract
  )
})


test_that("compatibility boundary identifies the GBD 2019 historical mapping", {
  readiness <- make_specification_readiness()
  context <- make_adapter_test_context(
    epi_version = "gbd2019",
    rr_selection_mode = "legacy_cause_selectors",
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      resolved <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = context,
        population_year_intent = make_adapter_test_population_year(2020)
      )
    }
  )

  expect_equal(resolved$status, "resolved")
  expect_equal(
    resolved$input_contract,
    paste(
      "component_resolved",
      "legacy_profile_default",
      "legacy_cause_selectors",
      "gbd2019",
      sep = "|"
    )
  )
  expect_equal(resolved$specification$epi_version, "gbd2019")
  expect_equal(
    resolved$specification$population_year$supplied$pop_year,
    2020
  )
  expect_equal(
    resolved$specification$population_year$resolved_pop_year,
    2020
  )
})


test_that("population year does not determine component routing applicability", {
  readiness <- make_specification_readiness()
  context <- make_adapter_test_context(
    epi_version = "gbd2019",
    rr_selection_mode = "legacy_cause_selectors",
    calc_causes = "GEMM and GBD",
    gbd_causes = "default"
  )
  population_intents <- list(
    missing = make_adapter_test_population_year(),
    another_year = make_adapter_test_population_year(2021),
    deprecated_scaling = make_adapter_test_population_year(
      scale_base_year = 2019,
      scale_target_year = 2020,
      resolved_pop_year = 2020
    )
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) readiness,
    {
      results <- lapply(population_intents, function(population_year_intent) {
        .resolve_hia_compatibility_boundary(
          crfs = tibble::tibble(),
          rr_sources = tibble::tibble(),
          epi = tibble::tibble(),
          context = context,
          population_year_intent = population_year_intent
        )
      })
      another_epi <- .resolve_hia_compatibility_boundary(
        crfs = tibble::tibble(),
        rr_sources = tibble::tibble(),
        epi = tibble::tibble(),
        context = utils::modifyList(context, list(epi_version = "gbd2021")),
        population_year_intent = make_adapter_test_population_year(2020)
      )
    }
  )

  expect_true(all(vapply(
    results,
    function(result) identical(result$status, "resolved"),
    logical(1)
  )))
  expect_length(unique(vapply(
    results,
    `[[`,
    character(1),
    "input_contract"
  )), 1)
  expect_equal(another_epi$status, "resolved")
  expect_equal(another_epi$specification$epi_version, "gbd2021")
})


test_that("compatibility boundary resolves composable legacy selections", {
  readiness <- make_specification_readiness()
  seen <- list()
  contexts <- list(
    legacy_cause_selectors = make_adapter_test_context(
      rr_selection_mode = "legacy_cause_selectors",
      calc_causes = "GBD only",
      gbd_causes = "default"
    ),
    c40 = make_adapter_test_context(crfs_version = "C40"),
    explicit_rr_epi = make_adapter_test_context(
      rr_sources_supplied = TRUE,
      epi_supplied = TRUE,
      epi_version = "custom"
    )
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(
      ...,
      crf_selection_id,
      epi_version
    ) {
      seen[[length(seen) + 1]] <<- list(
        crf_selection_id = crf_selection_id,
        epi_version = epi_version
      )
      readiness
    },
    {
      results <- lapply(contexts, function(context) {
        .resolve_hia_compatibility_boundary(
          crfs = tibble::tibble(),
          rr_sources = tibble::tibble(),
          epi = tibble::tibble(),
          context = context,
          population_year_intent = make_adapter_test_population_year()
        )
      })
    }
  )

  expect_true(all(vapply(
    results,
    function(result) identical(result$status, "resolved"),
    logical(1)
  )))
  expect_equal(
    vapply(seen, `[[`, character(1), "crf_selection_id"),
    c(
      "legacy_profile_default",
      "legacy_profile_c40",
      "legacy_profile_default"
    )
  )
  expect_equal(
    vapply(seen, `[[`, character(1), "epi_version"),
    c("default", "default", "custom")
  )
})


test_that("compatibility boundary explicitly excludes unsupported CRF inputs", {
  contexts <- list(
    registry_route = make_adapter_test_context(crf_compute = "registry"),
    explicit_crfs = make_adapter_test_context(crfs_supplied = TRUE),
    unrepresented_crf_profile = make_adapter_test_context(
      crfs_version = "unknown profile"
    )
  )

  with_mocked_bindings(
    .resolve_hia_component_readiness = function(...) {
      stop("excluded slices must not reach readiness resolution")
    },
    {
      results <- lapply(contexts, function(context) {
        .resolve_hia_compatibility_boundary(
          crfs = tibble::tibble(),
          rr_sources = tibble::tibble(),
          epi = tibble::tibble(),
          context = context,
          population_year_intent = make_adapter_test_population_year()
        )
      })
    }
  )

  expect_true(all(vapply(
    results,
    function(result) identical(result$status, "not_applicable"),
    logical(1)
  )))
  expect_equal(
    names(results),
    unname(vapply(results, `[[`, character(1), "reason"))
  )
})


test_that("compatibility boundary validates argument provenance", {
  incomplete <- make_adapter_test_context()
  incomplete$epi_supplied <- NULL

  expect_error(
    .resolve_hia_compatibility_boundary(
      crfs = tibble::tibble(),
      rr_sources = tibble::tibble(),
      epi = tibble::tibble(),
      context = incomplete,
      population_year_intent = make_adapter_test_population_year()
    ),
    "missing required fields: epi_supplied"
  )
  expect_error(
    .resolve_hia_compatibility_boundary(
      crfs = tibble::tibble(),
      rr_sources = tibble::tibble(),
      epi = tibble::tibble(),
      context = make_adapter_test_context(crfs_supplied = NA),
      population_year_intent = make_adapter_test_population_year()
    ),
    "must be single TRUE/FALSE values: crfs_supplied"
  )
})


test_that("route planning keeps incomplete contracts on the legacy route", {
  observation <- make_adapter_route_observation(ready = FALSE)
  resolver_called <- FALSE
  plan <- .plan_hia_compatibility_route(
    observation,
    registry_set_resolver = function(...) {
      resolver_called <<- TRUE
      stop("unready contracts must not resolve a Registry Set")
    }
  )

  expect_s3_class(plan, "creahia_hia_compatibility_route_plan")
  expect_equal(plan$status, "planned")
  expect_equal(plan$route, "legacy")
  expect_equal(plan$reason, "components_not_ready")
  expect_setequal(
    plan$blocking_component_ids,
    observation$specification$readiness$blocking_component_ids
  )
  expect_false(resolver_called)
  expect_null(plan$registry_set)
})


test_that("route planning selects Registry only when the whole call is ready", {
  observation <- make_adapter_route_observation(ready = TRUE)
  expected_set <- crfs_set("gbd2019_deaths")
  seen_specification <- NULL
  plan <- .plan_hia_compatibility_route(
    observation,
    registry_set_resolver = function(specification) {
      seen_specification <<- specification
      expected_set
    }
  )

  expect_identical(seen_specification, observation$specification)
  expect_equal(plan$status, "planned")
  expect_equal(plan$route, "registry")
  expect_equal(plan$reason, "all_components_ready")
  expect_identical(plan$registry_set, expected_set)
  expect_length(plan$blocking_component_ids, 0)
})


test_that("route planning never hides Registry Set resolution failures", {
  observation <- make_adapter_route_observation(ready = TRUE)
  plan <- .plan_hia_compatibility_route(
    observation,
    registry_set_resolver = function(...) stop("Registry data are unavailable.")
  )

  expect_equal(plan$status, "error")
  expect_null(plan$route)
  expect_equal(plan$reason, "registry_set_resolution_error")
  expect_match(plan$error, "Registry data are unavailable")
  expect_error(
    .plan_hia_compatibility_route(
      observation,
      registry_set_resolver = function(...) tibble::tibble()
    ),
    "must return a crfs_set"
  )
})


test_that("route planning preserves excluded and failed boundary states", {
  excluded <- .plan_hia_compatibility_route(list(
    status = "not_applicable",
    reason = "registry_route"
  ))
  failed <- .plan_hia_compatibility_route(list(
    status = "error",
    error = "Readiness resolution failed."
  ))

  expect_equal(excluded$route, "unchanged")
  expect_equal(excluded$reason, "registry_route")
  expect_equal(failed$status, "error")
  expect_null(failed$route)
  expect_match(failed$error, "Readiness resolution failed")
})


test_that("route planning rejects inconsistent readiness claims", {
  observation <- make_adapter_route_observation(ready = FALSE)
  observation$result$ready <- TRUE

  expect_error(
    .plan_hia_compatibility_route(observation),
    "readiness result and canonical specification disagree"
  )
})


test_that("legacy route application preserves existing compute inputs", {
  plan <- .plan_hia_compatibility_route(
    make_adapter_route_observation(ready = FALSE)
  )
  crfs <- tibble::tibble(
    cause = "COPD",
    outcome = "Deaths"
  )
  rr_sources <- tibble::tibble(
    cause = "COPD",
    source = "gemm"
  )

  applied <- .apply_hia_compatibility_route_plan(
    route_plan = plan,
    crfs = crfs,
    rr_sources = rr_sources,
    crf_compute = "legacy"
  )

  expect_identical(applied$crfs, crfs)
  expect_identical(applied$rr_sources, rr_sources)
  expect_identical(applied$crf_compute, "legacy")
})


test_that("unchanged route application preserves an explicit Registry route", {
  plan <- .plan_hia_compatibility_route(list(
    status = "not_applicable",
    reason = "registry_route"
  ))
  crfs <- crfs_set("gbd2019_deaths")
  rr_sources <- .empty_hia_rr_sources()

  applied <- .apply_hia_compatibility_route_plan(
    route_plan = plan,
    crfs = crfs,
    rr_sources = rr_sources,
    crf_compute = "registry"
  )

  expect_identical(applied$crfs, crfs)
  expect_identical(applied$rr_sources, rr_sources)
  expect_identical(applied$crf_compute, "registry")
})


test_that("Registry route application replaces all compute inputs atomically", {
  registry_set <- crfs_set("gbd2019_deaths")
  class(registry_set) <- unique(c(
    "creahia_hia_diagnostic_set",
    class(registry_set)
  ))
  # Test double: this ordinary CRF is marked only to satisfy the internal RR-provenance guard.
  registry_set <- .mark_hia_compatibility_rr_crfs(
    registry_set,
    registry_set$crf_id[[1]]
  )
  plan <- .plan_hia_compatibility_route(
    make_adapter_route_observation(ready = TRUE),
    registry_set_resolver = function(...) registry_set
  )
  legacy_crfs <- tibble::tibble(
    cause = "COPD",
    outcome = "Deaths"
  )
  legacy_rr_sources <- tibble::tibble(
    cause = "COPD",
    source = "gemm"
  )

  applied <- .apply_hia_compatibility_route_plan(
    route_plan = plan,
    crfs = legacy_crfs,
    rr_sources = legacy_rr_sources,
    crf_compute = "legacy"
  )

  expect_identical(applied$crfs, registry_set)
  expect_false(has_rr_sources(applied$rr_sources))
  expect_named(applied$rr_sources, c("cause", "source"))
  expect_identical(applied$crf_compute, "registry")
})


test_that("route application fails closed for unsafe plans", {
  errored_plan <- .plan_hia_compatibility_route(
    make_adapter_route_observation(ready = TRUE),
    registry_set_resolver = function(...) {
      stop("Registry data are unavailable.")
    }
  )
  legacy_crfs <- tibble::tibble(cause = "COPD")
  rr_sources <- tibble::tibble(cause = "COPD", source = "gemm")

  expect_error(
    .apply_hia_compatibility_route_plan(
      route_plan = errored_plan,
      crfs = legacy_crfs,
      rr_sources = rr_sources,
      crf_compute = "legacy"
    ),
    "Cannot apply an errored HIA compatibility route plan"
  )

  invalid_registry_plan <- .new_hia_compatibility_route_plan(
    status = "planned",
    route = "registry",
    reason = "all_components_ready"
  )
  expect_error(
    .apply_hia_compatibility_route_plan(
      route_plan = invalid_registry_plan,
      crfs = legacy_crfs,
      rr_sources = rr_sources,
      crf_compute = "legacy"
    ),
    "requires a resolved crfs_set"
  )

  legacy_plan <- .plan_hia_compatibility_route(
    make_adapter_route_observation(ready = FALSE)
  )
  expect_error(
    .apply_hia_compatibility_route_plan(
      route_plan = legacy_plan,
      crfs = crfs_set("gbd2019_deaths"),
      rr_sources = .empty_hia_rr_sources(),
      crf_compute = "registry"
    ),
    "requires existing legacy CRF inputs"
  )
})
