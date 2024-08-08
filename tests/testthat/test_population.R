
test_that("Population is properly calculated", {


  # Create a grid raster around Bangladesh
  iso2 <- "BD"

  # Data validation from UN, which is used by GPW, though the numbers below are from the 2024 version
  # whilst GPW seems to use UNWPP 2015
  # https://population.un.org/dataportal/data/indicators/49/locations/608,356/start/1990/end/2024/table/pivotbylocation?df=8779997f-7e7e-46b1-a155-28fa58ffa20d
  pop_validated <- list(
    "BD" = list(
      `2015` = 159e6,
      `2020` = 166e6
    ),
    "PH" = list(
      `2015` = 105.3e6,
      `2020` = 112.1e6
    ),
    "IN" = list(
      `2015` = 132.8e6,
      `2020` = 140.3e6
    ),
    "VN" = list(
      `2015` = 92.8e6,
      `2020` = 98.1e6
    )
  )

  iso3 <- countrycode::countrycode(iso2, origin='iso2c', destination='iso3c')
  adm <- creahelpers::get_adm(level=0, res="full", iso2s=iso2)
  bbox <- adm %>% sf::st_as_sf() %>% sf::st_bbox()

  # Create a grid to project the population
  grid <- terra::rast(terra::ext(bbox), 1000, 1000)

  # Get population raster in 2015
  pop2015 <- get_pop_count(grid, year_desired=2015)
  pop2020 <- get_pop_count(grid, year_desired=2020)

  # Ensure they all have same dimension / areas
  expect_equal(terra::ext(grid), terra::ext(pop2015))
  expect_equal(terra::ext(grid), terra::ext(pop2020))

  # Check that the population in 2015 is less than the population in 2020
  attr(pop2015, 'year') -> year2015
  attr(pop2020, 'year') -> year2020

  expect_equal(year2015, 2015) #If fails: The year attribute should be 2015. Maybe you're missing the population file?
  expect_equal(year2020, 2020) #If fails: The year attribute should be 2020. Maybe you're missing the population file?


  # Extract population values
  pop2015_extracted <- terra::extract(pop2015, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]
  pop2020_extracted <- terra::extract(pop2020, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]


  # Compare with validated data
  # It is not very close at the moment... something is still weird
  # It is not the reprojection, as the unprojected version is also off (see below)
  # Could be the area calculation? Or the boundaries
  expect_equal(pop2015_extracted, pop_validated[[iso2]]$`2015`, tolerance=0.05)
  expect_equal(pop2020_extracted, pop_validated[[iso2]]$`2020`, tolerance=0.05)


  # Try the unprojected version (much slower)
  # pop2015_unproj <- get_pop_count(grid=NULL, year_desired=2015)
  # pop2020_unproj <- get_pop_count(grid=NULL, year_desired=2020)
  #
  # pop2015_unproj_extracted <- terra::extract(pop2015_unproj, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]
  # pop2020_unproj_extracted <- terra::extract(pop2020_unproj, terra::vect(adm), sum, weights=T, na.rm=T)[1,2]

})

