riverspace_actual <- delineate_riverspace(occluders = bucharest_osm$buildings,
                                          river = bucharest_osm$river_surface)

test_that("The riverspace of Dâmbovița within 100m is correctly returned", {
  actual_surface <- sf::st_area(riverspace_actual)
  # The expected surface is rounded to the nearest 100,000 m^2
  # to account for changes in input data (buildings added or removed in OSM)
  expected_surface <- round(7742422, -5)
  expect_equal(round(actual_surface, -5), expected_surface, )
})

test_that(
  "The area of the riverspace of Dâmbovița is smaller than
  an unoccluded buffer and larger than the water surface",
  {
    actual_surface <-
      units::set_units(sf::st_area(riverspace_actual), "m^2")
    river_surface_buffer <-
      sf::st_buffer(bucharest_osm$river_surface, 100)
    river_surface_buffer_area <- sf::st_area(river_surface_buffer)
    river_surface_area <- sf::st_area(bucharest_osm$river_surface)
    expect_lt(actual_surface, river_surface_buffer_area)
    expect_gt(actual_surface, river_surface_area)
  }
)
