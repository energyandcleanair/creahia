testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("HIAs are consistent across levels", {


  pm25 <- raster::raster("~/development/crea/studies/202412_hia_southafrica/cache/pm25_2023_30_sec.tif")

  hias <- generate_random_exposure_hias(levels=c(0,1,2),
                                        iso3="ZAF",
                                        calc_causes="GBD only",
                                        administrative_res="full",
                                        baseline_rast=pm25
                                        )


  # Check which region is weird
  adm2 <- creahelpers::get_adm(level=2, res="low", iso2s="ZA")

  bind_rows(
    hias %>% filter(level==1) %>% mutate(GID_1=region_id),
    hias %>% filter(level==2) %>% left_join(as.data.frame(adm2) %>% dplyr::select(GID_2, GID_1), by=c("region_id"="GID_2"))
  ) %>%
    filter(Outcome=="PTB") %>%
    group_by(GID_1, level, Outcome, Cause, scenario, estimate) %>%
    dplyr::summarise_at("number", sum) %>%
    spread(level, number) %>%
    mutate(diff=abs(`1`-`2`)/`1`) %>%
    arrange(desc(diff))


  # Comparison
  comparison <- hias %>%
    filter(level %in% c(0,1)) %>%
    filter(estimate=="central") %>%
    group_by(level, Outcome, Cause, scenario, estimate) %>%
    dplyr::summarise_at("number", sum) %>%
    ungroup() %>%
    group_by(Outcome, Cause, scenario, estimate) %>%
    dplyr::summarise(deviation=(max(number) - min(number))/mean(number)) %>%
    ungroup() %>%
    arrange(desc(abs(deviation)))


  # For central, expect very close results
  max_deviation_central <- comparison %>%
    filter(estimate=="central") %>%
    summarise(max_deviation=max(abs(deviation), na.rm=T))
  testthat::expect_lt(max_deviation_central$max_deviation, 0.05)

})

