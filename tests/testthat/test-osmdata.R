# All functions using [`osmdata_as_sf()`] store data in a cache folder.
# In order not to mess up with the user cache directory, we setup a temporary
# cache folder only used for testing purposes. This is achieved via the
# [`temp_cache_dir()`] helper function, which should be called in each test.

test_that("City boundary of Bucharest is correctly retreived", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Bucharest"
  bb <- get_osm_bb(city_name)
  bucharest_boundary <- get_osm_city_boundary(bb, city_name,
                                              force_download = TRUE)

  expect_equal(as.numeric(bb), as.numeric(sf::st_bbox(bucharest_boundary)))
})

test_that("City boundary of Paris is returned without error", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone(bb)

  boundary <- get_osm_city_boundary(bb, city_name, crs, force_download = TRUE)
  expect_true(!sf::st_is_empty(boundary))
})

test_that("Wrong city name throws error", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  expect_error(get_osm_city_boundary("Buhcarest", force_download = TRUE))
})

test_that("OSM data for Bucharest is correctly retreived", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  bucharest <- get_osmdata("Bucharest", "Dâmbovița", force_download = TRUE)

  expect_length(bucharest, 4)
  expect_true(all(sapply(bucharest, function(x) length(x) >= 1)))
})

test_that("Multiple boundaries are correcly retreived", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone(bb)

  city_boundary <- get_osm_city_boundary(bb, city_name, crs, multiple = TRUE,
                                         force_download = TRUE)
  expect_gt(length(city_boundary), 1)
})

test_that("Queried datasets can be retrieved from the cache on new calls", {
  skip_on_ci()

  # setup cache directory
  cache_dir <- temp_cache_dir()

  # calling get_osm_railways should create a file in the cache folder
  expect_message(get_osm_railways(bucharest_osm$bb, force_download = TRUE),
                 "Saving data to cache directory")
  cached_filename <- list.files(cache_dir, pattern = "^osmdata_railway_rail")
  cached_filepath <- file.path(cache_dir, cached_filename)
  expect_true(file.exists(cached_filepath))

  # calling get_osm_railways again should read data from the cached file,
  # raising a warning that includes the path to the cached file as well
  expect_warning(get_osm_railways(bb, force_download = FALSE), cached_filepath)
})

test_that("All geometries retrieved from OSM are valid", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  bucharest_osm <- get_osmdata("Bucharest", "Dâmbovița", force_download = TRUE)

  expect_true(all(vapply(bucharest_osm[!names(bucharest_osm) %in% "bb"],
                         \(x) if (!inherits(x, "bbox")) all(sf::st_is_valid(x)),
                         logical(1))))
})
