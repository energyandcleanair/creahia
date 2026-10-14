inspection_c40_causes <- function() {
  c(
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
}


test_that("inspection explains a ready C40 legacy configuration", {
  inspection <- inspect_hia_configuration(
    crfs_version = "C40",
    epi_version = "C40",
    calc_causes = inspection_c40_causes(),
    gbd_causes = "default",
    pop_year = 2022
  )

  expect_s3_class(inspection, "creahia_hia_configuration_inspection")
  expect_equal(inspection$status, "resolved")
  expect_equal(inspection$route, "registry")
  expect_equal(inspection$reason, "all_components_ready")
  expect_equal(
    inspection$input_contract,
    paste(
      "component_resolved",
      "legacy_profile_c40",
      "legacy_cause_selectors",
      "C40",
      sep = "|"
    )
  )
  expect_s3_class(inspection$canonical_specification, "creahia_hia_specification")
  expect_s3_class(inspection$registry_set, "creahia_crf_set")
  expect_equal(
    inspection$components %>% dplyr::count(component_type, name = "n"),
    tibble::tribble(
      ~component_type, ~n,
      "crf", 19L,
      "epi", 1L,
      "rr", 11L
    )
  )
  expect_equal(nrow(inspection$registry_set), 30L)
  expect_equal(nrow(inspection$blocking_components), 0L)
  expect_equal(inspection$population_year$resolved_pop_year, 2022)
  registry_components <- inspection$components %>%
    dplyr::filter(.data$component_type %in% c("crf", "rr"))
  expect_true(all(
    registry_components$lifecycle == "historical_reproduction"
  ))
  expect_true(all(
    registry_components$scientific_review_status == "not_reviewed"
  ))
  expect_true(all(is.na(
    inspection$components$lifecycle[
      inspection$components$component_type == "epi"
    ]
  )))
  expect_setequal(
    inspection$references$reference_id,
    unique(inspection$registry_set$reference_id)
  )
  expect_false(any(inspection$references$reference_id == "registry_fixture"))
  expect_true(all(!is.na(inspection$references$title)))
})


test_that("inspection exposes blockers without constructing a Registry Set", {
  manifest <- .load_hia_component_manifest()
  blocked_id <- manifest$component_id[[1]]
  manifest$routing_status[[1]] <- "not_ready"
  manifest$routing_blocking_reason[[1]] <- "Inspection test blocker."

  with_mocked_bindings(
    .load_hia_component_manifest = function() manifest,
    {
      inspection <- inspect_hia_configuration()
    }
  )

  expect_equal(inspection$status, "resolved")
  expect_equal(inspection$route, "legacy")
  expect_equal(inspection$reason, "components_not_ready")
  expect_null(inspection$registry_set)
  expect_equal(nrow(inspection$references), 0L)
  expect_true(blocked_id %in% inspection$blocking_components$component_id)
  expect_true(any(
    inspection$blocking_components$blocking_reason ==
      "Inspection test blocker."
  ))
})


test_that("inspection is read-only and does not notify shadow observers", {
  observed <- FALSE
  old_options <- options(
    creahia.hia_readiness_shadow_observer = function(...) {
      observed <<- TRUE
    }
  )
  on.exit(options(old_options), add = TRUE)

  inspection <- inspect_hia_configuration(
    crfs_version = "C40",
    epi_version = "C40",
    calc_causes = inspection_c40_causes(),
    gbd_causes = "default"
  )

  expect_false(observed)
  expect_equal(inspection$route, "registry")
  expect_output(print(inspection), "Route: registry")
  expect_output(print(inspection), "Ready for Registry routing: TRUE")
})
