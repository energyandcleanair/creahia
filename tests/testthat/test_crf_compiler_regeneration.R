test_that("public compilers regenerate packaged definitions and curves", {
  original_path <- get_hia_path
  packaged_registry <- load_crf_registry()
  candidate_root <- tempfile("crf-regeneration-")
  dir.create(file.path(candidate_root, "crf"), recursive = TRUE)
  on.exit(unlink(candidate_root, recursive = TRUE), add = TRUE)
  for (file in c("registry.csv", "references.csv")) {
    file.copy(original_path(file.path("crf", file)),
              file.path(candidate_root, "crf", file))
  }

  candidate_path <- function(filename, error_if_not_exists = FALSE) {
    if (startsWith(filename, "crf/")) {
      path <- file.path(candidate_root, filename)
      if (error_if_not_exists && !file.exists(path)) stop("Missing candidate file: ", filename)
      return(path)
    }
    original_path(filename, error_if_not_exists)
  }

  with_mocked_bindings(
    {
      rows <- dplyr::bind_rows(
        compile_gemm_ihd_25plus_deaths(),
        compile_gemm_adult_deaths(),
        compile_fusion_adult_deaths()
      )
      compile_gbd_deaths(versions = gbd_reviewed_versions())
      rows <- dplyr::bind_rows(rows, dplyr::bind_rows(lapply(
        gbd_reviewed_versions(), gbd_deaths_registry_rows
      )))
      expect_false("double_counted" %in% names(rows))
      expected_rows <- packaged_registry %>%
        dplyr::filter(crf_id %in% rows$crf_id) %>%
        dplyr::arrange(crf_id)
      expect_equal(as.data.frame(dplyr::arrange(rows, crf_id)),
                   as.data.frame(expected_rows), tolerance = 0)

      for (file in rows$data_path) {
        actual <- readr::read_csv(candidate_path(file), col_types = readr::cols())
        expected <- readr::read_csv(original_path(file), col_types = readr::cols())
        expect_equal(as.data.frame(actual), as.data.frame(expected), tolerance = 0, info = file)
      }
      expect_identical(readLines(candidate_path("crf/registry.csv")),
                       readLines(original_path("crf/registry.csv")))
    },
    get_hia_path = candidate_path,
    .package = "creahia"
  )
})
