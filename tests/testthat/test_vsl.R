# Test our results vs GBD
testthat::source_test_helpers("tests", env = globalenv())
testthat::source_test_helpers("../", env = globalenv())


test_that("WorldBank vsl matches manual calculation", {


  # Get computed VSL from creahia
  hia_wb <- generate_donkelaar_exposure_hia(target=5,
                                                 iso3 = "ZAF",
                                                 epi_version = "gbd2021",
                                                 rr_sources=RR_GBD2021)

  costs <- creahia::compute_econ_costs(hia, valuation_version = "worldbank")
  vsl_crea <- costs$hia_cost %>% filter(Outcome == "Deaths") %>% distinct(year, vsl_crea=valuation_current_usd)


  # Manually compute it
  vsl_ref_oecd_2011 <- 3830000
  vsl_ref_elasticity <- 1.2
  vsl_ref_year <- 2011

  vsls <- wbstats::wb_data(c(gdp_current = 'NY.GDP.PCAP.CD',
                             gdp_ppp_constant = 'NY.GDP.PCAP.PP.KD',
                             gdp_ppp_current = 'NY.GDP.PCAP.PP.CD'
  ),
  start_date = vsl_ref_year,
  end_date = max(costs$hia_cost$year),
  country=c("ZAF", "OED")) %>%
    select(iso3c, date, gdp_current, gdp_ppp_constant, gdp_ppp_current) %>%
    pivot_wider(names_from = iso3c, values_from = c(gdp_current, gdp_ppp_constant, gdp_ppp_current))


  vsls$vsl_ppp <- vsl_ref_oecd_2011 * (vsls$gdp_ppp_constant_ZAF / vsls$gdp_ppp_constant_OED[vsls$date==vsl_ref_year]) ^ vsl_ref_elasticity
  vsls$vsl_market <- vsls$vsl_ppp * vsls$gdp_current_ZAF / vsls$gdp_ppp_constant_ZAF
  # Remove attribute
  attr(vsls$vsl_market, "label") <- NULL

  vsl_manual <- vsls %>%
    distinct(year=date, vsl_manual=vsl_market)


  # Compare
  comparison <- vsl_crea %>%
    left_join(vsl_manual, by = "year")

  testthat::expect_equal(comparison$vsl_crea, comparison$vsl_manual, tolerance = 0.1)

})

