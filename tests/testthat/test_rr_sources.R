test_that("Results with different RR sources are properly ordered", {

  res <- 0.01
  m <- terra::rast(
    xmin=88,
    xmax=92,
    ymin=20,
    ymax=27,
    res=res,
    crs="+proj=longlat +datum=WGS84")

  m[] <- 60

  # Build two perturbations:
  # p1: bring it down to 0
  # p2: bring it down to WHO2021
  p1 <- -m
  p2 <- 5-m

  ordered_sources <- c(
    RR_ORIGINAL,
    RR_GBD2019,
    RR_GBD2021,
    RR_GEMM,
    RR_FUSION # Fusion > old GEMM (the one we have) but lower than new GEMM (e.g. in Fusion paper)
  )

  hias <- lapply(ordered_sources, function(rr_source) {
    print(rr_source)
    creahia::wrappers.compute_hia_two_images.default(
      baseline_rasters = list(pm25 = m),
      perturbation_rasters = list(pm25 = p1),
      pop_year = 2020,
      administrative_level = 1,
      administrative_res = "low",
      administrative_iso3s = "BGD",
      rr_sources = rr_source
    ) %>%
      mutate(
        rr_source = rr_source
      )
  }) %>%
    bind_rows()


  ordered_sources_out <- hias %>%
    filter(estimate=="central", outcome=="Deaths", !double_counted) %>%
    group_by(rr_source) %>%
    summarise(sum=sum(number)) %>%
    arrange(abs(sum))


  expect_equal(
    ordered_sources_out$rr_source,
    ordered_sources
  )

})
