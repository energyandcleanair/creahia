missing_calc_causes_message <- paste(
  "calc_causes must be provided as a preset name,",
  "a '<cause>_<outcome>' vector, or \"none\";",
  "NULL and empty character vectors are not valid conversion inputs."
)


test_that("legacy RR conversion rejects empty calc_causes", {
  expect_error(
    convert_old_parameters_to_rr_sources(NULL, "all"),
    fixed(missing_calc_causes_message)
  )
  expect_error(
    convert_old_parameters_to_rr_sources(NULL, "default"),
    fixed(missing_calc_causes_message)
  )
  expect_error(
    convert_old_parameters_to_rr_sources(character(), "all"),
    fixed(missing_calc_causes_message)
  )
  expect_error(
    convert_old_parameters_to_rr_sources(character(), NULL),
    fixed(missing_calc_causes_message)
  )
})


test_that("legacy RR conversion preserves the explicit none selection", {
  converted <- lapply(list("all", "default", NULL), function(gbd_causes) {
    convert_old_parameters_to_rr_sources("none", gbd_causes)
  })

  for (selection in converted) {
    expect_s3_class(selection, "data.frame")
    expect_equal(nrow(selection), 0L)
    expect_identical(names(selection), c("cause", "source", "outcome"))
    expect_identical(selection$cause, character())
    expect_identical(selection$source, character())
    expect_identical(selection$outcome, character())
  }
  expect_identical(converted[[1]], converted[[2]])
  expect_identical(converted[[2]], converted[[3]])
})


test_that("legacy RR conversion preserves established valid selections", {
  expect_identical(
    convert_old_parameters_to_rr_sources("GEMM and GBD", "all"),
    list(
      gemm = "NCD.LRI",
      original = c(
        "LRI.child", "LRI", "COPD", "Diabetes", "IHD", "LC", "Stroke"
      )
    )
  )
  expect_identical(
    convert_old_parameters_to_rr_sources("GEMM and GBD", "default"),
    list(
      gemm = c("NCD.LRI", "LRI", "COPD", "IHD", "LC", "Stroke"),
      original = c("LRI.child", "Diabetes")
    )
  )
  expect_identical(
    convert_old_parameters_to_rr_sources("GEMM and GBD", NULL),
    list(
      gemm = c("NCD.LRI", "LRI", "COPD", "IHD", "LC", "Stroke"),
      original = c("LRI.child", "Diabetes")
    )
  )
  expect_identical(
    convert_old_parameters_to_rr_sources("GBD only", "default"),
    list(
      original = c(
        "LRI.child", "LRI", "COPD", "Diabetes", "IHD", "LC", "Stroke"
      )
    )
  )
  expect_identical(
    convert_old_parameters_to_rr_sources("GEMM only", "default"),
    list(gemm = c("NCD.LRI", "LRI", "COPD", "IHD", "LC", "Stroke"))
  )
  expect_identical(
    convert_old_parameters_to_rr_sources(
      c("Stroke_YLDs", "COPD_YLDs", "Diabetes_YLDs"),
      "default"
    ),
    tibble::tribble(
      ~cause, ~source, ~outcome,
      "Stroke", "gemm", "YLDs",
      "COPD", "gemm", "YLDs",
      "Diabetes", "original", "YLDs"
    )
  )
  expect_identical(
    convert_old_parameters_to_rr_sources(
      c("IHD_Deaths", "LRI.child_Deaths"),
      "all"
    ),
    tibble::tribble(
      ~cause, ~source, ~outcome,
      "IHD", "original", "Deaths",
      "LRI.child", "original", "Deaths"
    )
  )
  expect_identical(
    convert_old_parameters_to_rr_sources("IHD_Deaths", c("IHD")),
    tibble::tribble(
      ~cause, ~source, ~outcome,
      "IHD", "original", "Deaths"
    )
  )
})


test_that("RR selection boundary rejects empty calc_causes", {
  expect_error(
    .resolve_hia_rr_selection_boundary(
      rr_sources = "gemm",
      calc_causes = NULL,
      gbd_causes = "all"
    ),
    fixed(missing_calc_causes_message)
  )
  expect_error(
    .resolve_hia_rr_selection_boundary(
      rr_sources = "gemm",
      calc_causes = character(),
      gbd_causes = NULL
    ),
    fixed(missing_calc_causes_message)
  )
})


test_that("compute_hia rejects incomplete legacy selectors before PAF", {
  paf_called <- FALSE
  local_mocked_bindings(
    compute_hia_paf = function(...) {
      paf_called <<- TRUE
      stop("compute_hia_paf should not be called", call. = FALSE)
    }
  )

  for (routing in c(TRUE, FALSE)) {
    withr::local_options(creahia.hia_compatibility_routing = routing)

    expect_error(
      compute_hia(
        conc_map = list(),
        species = "pm25",
        regions = tibble::tibble(region_id = character()),
        rr_sources = "gemm",
        calc_causes = NULL,
        gbd_causes = "all",
        diagnostic_folder = NULL
      ),
      fixed(missing_calc_causes_message)
    )
    expect_false(paf_called)
  }
})


test_that("configuration inspection rejects the same incomplete selectors", {
  expect_error(
    inspect_hia_configuration(
      rr_sources = "gemm",
      calc_causes = NULL,
      gbd_causes = "all"
    ),
    fixed(missing_calc_causes_message)
  )
})
