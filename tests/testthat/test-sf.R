test_that("Splitting a geometry by a complex line returns more regions", {
  line <- sf::st_sfc(c(sf::st_linestring(cbind(c(-2, 2), c(0, 0))),
                       sf::st_linestring(cbind(c(-0.5, 0, 0.5), c(0, 0.5, 0)))))
  geometry <- sf::st_sfc(sf::st_polygon(list(cbind(c(-1, -1, 1, 1, -1),
                                                   c(-1, 1, 1, -1, -1)))))
  regions <- split(geometry, line)
  expect_equal(length(regions), 3)
})

test_that("Splitting a geometry by a complex line still returns two edges", {
  line <- sf::st_sfc(c(sf::st_linestring(cbind(c(-2, 2), c(0, 0))),
                       sf::st_linestring(cbind(c(-0.5, 0, 0.5), c(0, 0.5, 0)))))
  geometry <- sf::st_sfc(sf::st_polygon(list(cbind(c(-1, -1, 1, 1, -1),
                                                   c(-1, 1, 1, -1, -1)))))
  edges <- split(geometry, line, boundary = TRUE)
  expect_equal(length(edges), 2)
})
