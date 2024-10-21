test_that("City boundary is correctly retreived", {
  skip_on_ci()
  bucharest_boundary_path <- testthat::test_path("testdata",
                                                 "city_boundary_Bucharest.gpkg")
  bucharest_boundary <- sf::st_read(bucharest_boundary_path, quiet = TRUE) |>
    sf::st_geometry() |>
    sf::st_transform(4326)

  bucharest_boundary_osm <- get_osm_city_boundary("Bucharest") |>
    sf::st_geometry() |>
    sf::st_transform(4326)

  expect_equal(min(bucharest_boundary[[1]][[1]][[1]]),
               min(bucharest_boundary_osm[[1]][[1]][[1]]))

  expect_equal(max(bucharest_boundary[[1]][[1]][[1]]),
               max(bucharest_boundary_osm[[1]][[1]][[1]]))
})

test_that("City boundary of Paris is correctly returned", {
  skip_on_ci()
  expect_no_error(get_osm_city_boundary("Paris, France"))
})

test_that("Wrong city name throws error", {
  skip_on_ci()
  expect_error(get_osm_city_boundary("Buhcarest"))
})
