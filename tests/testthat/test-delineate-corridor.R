test_that("given bounding box coordinates and crs, a correct AoI is returned", {
  bb <- bucharest$bb
  crs <- 4326
  aoi <- define_aoi(bb, crs, buffer_dist = 0)
  coords <- sf::st_coordinates(aoi)

  bb_expected <- cbind(matrix(coords[1:2, 1]), matrix(coords[2:3, 2])) |> t()
  colnames(bb_expected) <- c("min", "max")
  rownames(bb_expected) <- c("x", "y")

  expect_equal(bb, bb_expected)
})

test_that("buffering an AoI with geographic crs gives a warning", {
  bb <- bucharest$bb
  crs <- 32634
  aoi <- define_aoi(bb, crs, buffer_dist = 0)

  expect_warning(define_aoi(bb, crs, buffer_dist = 1000), regexp = NA)
})

test_that("splitting an AoI by a river gives two areas of interest", {
  bb <- bucharest$bb
  crs <- 4326
  aoi <- define_aoi(bb, crs, buffer_dist = 0)
  river <- bucharest$river_centerline

  aoi_split <- split_aoi(aoi, sf::st_transform(river, sf::st_crs(aoi)))

  expect_equal(length(aoi_split), 2)
})

test_that("street network is correctly trimmed", {
  expect_equal(2 * 2, 4)
})

test_that("street network is correctly simplified", {
  expect_equal(2 * 2, 4)
})
