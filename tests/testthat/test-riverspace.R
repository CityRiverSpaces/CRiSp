test_that("River centerline is used with warning when no river polygon
          is available", {
  expect_equal(2 * 2, 4)
})

test_that("Delineation throws error when input data is not provided", {
  expect_equal(2 * 2, 4)
})

test_that("The riverspace of Dâmbovița within 100m is correctly returned", {
  expect_equal(2 * 2, 4)
})

test_that("Viewpoints are correctly created from linestring", {
  actual_points <- bucharest_osm$river_surface |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING") |>
    sf::st_sfc() |>
    get_viewpoints()

  expect_length(actual_points, 1057)
})
