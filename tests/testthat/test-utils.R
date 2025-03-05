test_that("setting units works if x is unitless", {
  x <- 1
  y <- 2
  x_new <- set_units_like(x, y)  # x_new should still be unitless
  expect_true(!inherits(x_new, "units"))
  y_units <- units::set_units(y, "m")
  x_new <- set_units_like(x, y_units)  # x_newshould now have "m" unit
  expect_true(inherits(x_new, "units"))
  expect_equal(units(x_new), units(y_units))
})

test_that("setting units works if x has unit", {
  x <- units::set_units(1, "m")
  y <- 2
  x_new <- set_units_like(x, y)  # x_new should now be unitless
  expect_true(!inherits(x_new, "units"))
  y_units <- units::set_units(y, "m")
  x_new <- set_units_like(x, y_units)  # x_new should now have "m" unit
  expect_true(inherits(x_new, "units"))
  expect_equal(units(x_new), units(y_units))
  y_units <- units::set_units(y, "km")
  x_new <- set_units_like(x, y_units)  # x_new should now be converted to "km"
  expect_true(inherits(x_new, "units"))
  expect_equal(units::drop_units(x_new), 0.001)
})

test_that("correct UTM zone is returend in the southern hemisphere", {
  # bbox for Chennai, India
  bbox <- sf::st_bbox(
    c(xmin = 80.11505, ymin = 12.92453, xmax = 80.27019, ymax = 13.08369),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32644)
})

test_that("correct UTM zone is returend in the northern hemisphere", {
  # bbox for Rejkjavik, Iceland
  bbox <- sf::st_bbox(
    c(xmin = -21.98383, ymin = 64.04040, xmax = -21.40200, ymax = 64.31537),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32627)
})

test_that("both bbox and sf objects can be used to find UTM zone", {
  bbox <- sf::st_bbox(c(xmin = -20, ymin = 20, xmax = -21, ymax = 21),
                      crs = sf::st_crs(4326))
  geom <- sf::st_as_sf(sf::st_as_sfc(bbox))
  utm_epsg_bbox <- get_utm_zone(bbox)
  utm_epsg_geom <- get_utm_zone(geom)
  expect_equal(utm_epsg_bbox, utm_epsg_geom)
})

test_that("a matrix is correctly converted to a bbox", {
  bb <- matrix(data = c(0, 1, 2, 3),
               nrow = 2,
               ncol = 2,
               dimnames = list(c("x", "y"), c("min", "max")))
  bbox <- as_bbox(bb)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
})

test_that("a vector is correctly converted to a bbox", {
  bb <- c(0, 1, 2, 3)
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bbox <- as_bbox(bb)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
})

test_that("a sf object is correctly converted to a bbox", {
  linestring <- sf::st_linestring(matrix(c(0, 1, 2, 3), ncol = 2, byrow = TRUE))
  bbox <- as_bbox(linestring)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
})

test_that("a bbox object does not change class", {
  crs <- 3285
  bb <- sf::st_bbox(c(xmin = 0, ymin = 1, xmax = 2, ymax = 3), crs = crs)
  bbox <- as_bbox(bb)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(crs))
})

test_that("buffering a bbox properly enlarge the area of interest", {
  # bbox in UTM zone 2N
  x <- c(xmin = 263554, xmax = 736446, ymin = 4987330, ymax = 5654109)
  bbox_utm2n <- sf::st_bbox(x, crs = "EPSG:32602")

  bbox_buffer_actual <- buffer_bbox(bbox_utm2n, 1000)

  y <- c(x[c("xmin", "ymin")] - 1000, x[c("xmax", "ymax")] + 1000)
  bbox_buffer_expected <- sf::st_bbox(y, crs = "EPSG:32602")

  expect_equal(bbox_buffer_actual, bbox_buffer_expected)
})

test_that("buffering a bbox does not change its CRS", {
  # bbox in WGS 84
  x <- c(xmin = -174, xmax = -168, ymin = 45, ymax = 51)
  bbox_wgs84 <- sf::st_bbox(x, crs = "EPSG:4326")

  bbox_buffer <- buffer_bbox(bbox_wgs84, 1000)

  crs_expected <- sf::st_crs(bbox_wgs84)
  crs_actual <- sf::st_crs(bbox_buffer)
  expect_equal(crs_actual, crs_expected)
})

test_that("reproject works with raster data", {
  # raster in UTM zone 2 (lon between -174 and -168 deg), northern emisphere
  x <- terra::rast(xmin = -174, xmax = -168, ymin = 45, ymax = 51, res = 1,
                   vals = 1, crs = "EPSG:4326")

  # reproject with integer (EPSG code)
  x_repr_int <- reproject(x, 32602)

  # reproject with string
  x_repr_str <- reproject(x, "EPSG:32602")

  crs_expected <- terra::crs("EPSG:32602")
  crs_actual_int <- terra::crs(x_repr_int)
  expect_equal(crs_actual_int, crs_expected)
  crs_actual_str <- terra::crs(x_repr_str)
  expect_equal(crs_actual_str, crs_expected)
})

test_that("reproject works with vector data", {
  # polygon in UTM zone 2 (lon between -174 and -168 deg), northern emisphere
  x <- sf::st_linestring(cbind(c(-174, -174, -168, -168, -174),
                               c(45, 51, 51, 45, 45)))
  x <- sf::st_polygon(list(x))
  x <- sf::st_sfc(x, crs = "EPSG:4326")

  # reproject with integer (EPSG code)
  x_repr_int <- reproject(x, 32602)

  # reproject with string
  x_repr_str <- reproject(x, "EPSG:32602")

  crs_expected <- sf::st_crs("EPSG:32602")
  crs_actual_int <- sf::st_crs(x_repr_int)
  expect_equal(crs_actual_int, crs_expected)
  crs_actual_str <- sf::st_crs(x_repr_str)
  expect_equal(crs_actual_str, crs_expected)
})

test_that("reproject works with bbox", {
  # bbox in UTM zone 2 (lon between -174 and -168 deg), northern emisphere
  x <- c(xmin = -174, xmax = -168, ymin = 45, ymax = 51)
  x <- sf::st_bbox(x, crs = "EPSG:4326")

  # reproject with integer (EPSG code)
  x_repr_int <- reproject(x, 32602)

  # reproject with string
  x_repr_str <- reproject(x, "EPSG:32602")

  crs_expected <- sf::st_crs("EPSG:32602")
  crs_actual_int <- sf::st_crs(x_repr_int)
  expect_equal(crs_actual_int, crs_expected)
  crs_actual_str <- sf::st_crs(x_repr_str)
  expect_equal(crs_actual_str, crs_expected)
})

test_that("load_raster correctly retrieve and merge local data", {

  write_local_raster <- function(fname, xmin, xmax, ymin, ymax) {
    rast <- terra::rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                        res = 1, vals = 1, crs = "EPSG:4326")
    terra::writeRaster(rast, fname)
  }

  bbox <- sf::st_bbox(c(xmin = 1, xmax = 4, ymin = 1, ymax = 7),
                      crs = "EPSG:4326")
  # create local rasters with adjacent bboxes
  withr::with_file(list("r1.tif" = write_local_raster("r1.tif", 1, 4, 1, 4),
                        "r2.tif" = write_local_raster("r2.tif", 1, 4, 4, 7)), {
      rast <- load_raster(c("r1.tif", "r2.tif"), bbox = bbox)
      # all values should be 1
      expect_true(all(terra::values(rast) == 1))
      # 2 rasters with 3x3 pixels -> 18 pixels in total
      expect_length(terra::values(rast), 18)
      # expect_equal on the two terra::ext objects somehow fails
      expect_true(terra::ext(rast) == terra::ext(bbox))
    }
  )
})

line <- sf::st_linestring(matrix(c(0, 0, 10, 10), ncol = 2, byrow = TRUE)) |>
  sf::st_sfc(crs = 4326)
extended_line <- extend_line(line, 5)

test_that("Extended line is longer", {
  original_length <- sf::st_length(line)
  extended_length <- sf::st_length(extended_line)
  expect_gt(extended_length, original_length)
})

test_that("Original and extended line have the same CRS", {
  expect_equal(sf::st_crs(line), sf::st_crs(extended_line))
})

test_that("Extension with 0 works", {
  extended_line_0 <- extend_line(line, 0)
  expect_equal(sf::st_coordinates(extended_line_0), sf::st_coordinates(line))
})

test_that("Extension with negative distance is not allowed", {
  expect_error(extend_line(line, -1))
})
