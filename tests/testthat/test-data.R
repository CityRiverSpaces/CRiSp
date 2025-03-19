test_that("Bucharest dataset includes all elements", {
  expect_setequal(names(bucharest_osm),
                  c("boundary", "river_centerline", "river_surface",
                    "aoi", "streets", "railways", "buildings"))
})

test_that("AoI has correct type", {
  expect_true(inherits(bucharest_osm$aoi, "sfc_POLYGON"))
})

test_that("Only the bbox and the AoI's have a geographic CRS", {
  is_longlat <- vapply(bucharest_osm, sf::st_is_longlat, logical(1))
  expect_true(is_longlat["aoi"])
  others <- is_longlat[!(names(bucharest_osm) == "aoi")]
  expect_true(!any(others))
})

test_that("City boundary has one multipolygon geometry", {
  boundary <- bucharest_osm$boundary
  expect_equal(length(boundary), 1)
  expect_true(sf::st_is(boundary, "MULTIPOLYGON"))
})

test_that("River center line has one linestring or multilinestring geometry", {
  river_centerline <- bucharest_osm$river_centerline
  expect_equal(length(river_centerline), 1)
  expect_true(sf::st_geometry_type(river_centerline) %in%
                c("MULTILINESTRING", "LINESTRING"))
})

test_that("River surface has one multipolygon geometry", {
  river_surface <- bucharest_osm$river_surface
  expect_equal(length(river_surface), 1)
  expect_true(sf::st_is(river_surface, "MULTIPOLYGON"))
})

test_that("Streets are all linestrings", {
  expect_true(all(sf::st_is(bucharest_osm$streets, "LINESTRING")))
})

test_that("Streets have type column", {
  expect_equal(colnames(bucharest_osm$streets), c("type", "geometry"))
})

test_that("Railways are all linestrings", {
  expect_true(all(sf::st_is(bucharest_osm$railways, "LINESTRING")))
})

test_that("Railways have type column", {
  expect_equal(colnames(bucharest_osm$railways), c("type", "geometry"))
})
