riverspace_actual <- delineate_riverspace(occluders = bucharest_osm$buildings,
                                          river = bucharest_osm$river_surface,
                                          ray_length = 100)
sf::st_crs(riverspace_actual) <- 32635

test_that("The riverspace of Dâmbovița within 100m is correctly returned", {
  actual_surface <- sf::st_area(riverspace_actual)
  # A tolerance of 100,000 m^2 is used to account for changes in input data
  # (buildings added or removed in OSM)
  expected_surface <- units::set_units(7259076, "m^2")
  expect_lt(abs(expected_surface - actual_surface),
            units::set_units(1e05, "m^2"))
})

test_that(
  "The area of the riverspace of Dâmbovița is smaller than
  an unoccluded buffer and larger than the water surface",
  {
    actual_surface <- sf::st_area(riverspace_actual)
    river_surface_buffer <-
      sf::st_buffer(bucharest_osm$river_surface, 100)
    river_surface_buffer_area <- sf::st_area(river_surface_buffer)
    river_surface_area <- sf::st_area(bucharest_osm$river_surface)
    expect_lt(actual_surface, river_surface_buffer_area)
    expect_gt(actual_surface, river_surface_area)
  }
)
