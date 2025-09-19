

hia <- tibble(
  scenario="scenario",
  region_id=c("IND", "CHN"),
  region_name=c("India", "China"),
  iso3=c("IND", "CHN"),
  pop=1,
  Outcome="Deaths",
  number=1,
  Pollutant="PM25",
  cause="cause",
  double_counted=F,
  AgeGrp="25+",

) %>%
  tidyr::crossing(estimate=c("central","low","high"))

cost <- creahia::compute_econ_costs(hia=hia,
                                    results_dir = "tmp",
                                    valuation_version="viscusi",
                                    projection_years=seq(2020, 2060),
                                    GDP_scaling=T
)

cost$cost_forecast %>%
  filter(estimate=="central") %>%
  select(region_id, region_name, year, number, cost_mn_currentUSD) %>%
  mutate(vsl_mn_currentUSD = cost_mn_currentUSD / number) %>%
  select(region_name, year, vsl_mn_currentUSD) %>%
  filter(year >= 2020,
         region_name=="India") %>%
  write_csv('tmp/vsl_india.csv')

cost$cost_forecast %>%
  filter(estimate=="central") %>%
  select(region_id, region_name, year, number, cost_mn_currentUSD) %>%
  mutate(vsl_mn_currentUSD = cost_mn_currentUSD / number) %>%
  select(region_name, year, vsl_mn_currentUSD) %>%
  filter(year >= 2020,
         region_name=="China") %>%
  write_csv('tmp/vsl_china.csv')

arrange(year) %>%
  ggplot() + geom_line(aes(year, vsl_mn_currentUSD))



  hia <- tibble(
    scenario="scenario",
    region_id="CHN",
    estimate=c("central","low","high"),
    pop=1,
    Outcome="Deaths",
    number=1,
    Pollutant="PM25",
    cause="cause",
    double_counted=F,
    AgeGrp="25+",
    iso3="CHN"
  )

  cost <- creahia::compute_econ_costs(hia=hia,
                                      results_dir = "tmp",
                                      valuation_version="viscusi",
                                      projection_years=seq(2020, 2060),
                                      GDP_scaling=T
  )

  cost$cost_forecast %>%
    filter(estimate=="central") %>%
    select(year, number, cost_mn_currentUSD) %>%
    mutate(vsl_mn_currentUSD = cost_mn_currentUSD / number,
           country="India") %>%
    select(country, year, vsl_mn_currentUSD) %>%
    filter(year >= 2020) %>%
    write_csv('tmp/vsl_india.csv')
  arrange(year) %>%
    ggplot() + geom_line(aes(year, vsl_mn_currentUSD))
