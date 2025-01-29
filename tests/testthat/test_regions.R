testthat::skip("Not sure yet what to do with this test")

test_that("Regions extraction works and doesn't have any double counting", {


  # Global annual attributable mortality increased steadily from 4.04 (95% CI: 2.59–5.33) million in 1998 to 5.70 (95% CI: 3.98–7.18) million in 2011.
  # The pace slowed afterwards, peaking at 5.83 million (95% CI: 4.05–7.38) in 2015 and then decreasing slightly to 5.74 million (95% CI: 3.92–7.35) in 2019.
  # https://www.nature.com/articles/s41467-023-41086-z
  library(terra)
  library(tidyverse)
  res <- creaexposure::RES_2PT5_MIN
  grid <- creaexposure::data.pop(res=res) %>% rast()


  regions <- creahia::get_model_adm(grid,
                                    admin_level = 0,
                                    res = "low",
                                    iso3s = NULL)

  regions %>%
    filter(region_name=="China") %>%
    sf::st_as_sf() %>%
    select(region_id) %>%
    # Plot for each region_id
    ggplot() +
    geom_sf(aes(fill=region_id)) +
    facet_wrap(~region_id)

})
