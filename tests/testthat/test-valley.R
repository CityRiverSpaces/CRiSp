# All functions using [`load_dem()`] store data in a cache folder.
# In order not to mess up with the user cache directory, we setup a temporary
# cache folder only used for testing purposes. This is achieved via the
# [`temp_cache_dir()`] helper function, which should be called in each test.

bb <- bucharest_osm$bb
asset_urls <- c(paste0("s3://copernicus-dem-30m/",
                       "Copernicus_DSM_COG_10_N44_00_E026_00_DEM/",
                       "Copernicus_DSM_COG_10_N44_00_E026_00_DEM.tif"),
                paste0("s3://copernicus-dem-30m/",
                       "Copernicus_DSM_COG_10_N44_00_E025_00_DEM/",
                       "Copernicus_DSM_COG_10_N44_00_E025_00_DEM.tif"))

test_that("STAC asset urls are correctly retrieved", {
  skip_on_ci()

  ep <- "https://earth-search.aws.element84.com/v1"
  col <- "cop-dem-glo-30"

  asset_urls_retrieved <- get_stac_asset_urls(bb, endpoint = ep,
                                              collection = col)
  expected_asset_urls <- asset_urls

  expect_equal(expected_asset_urls, asset_urls_retrieved)
})

test_that("load_dem correctly retrieve and merge remote data", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  dem <- load_dem(bb, asset_urls, force_download = TRUE)

  expect_equal(terra::crs(dem), terra::crs("EPSG:4326"))
  expect_equal(as.vector(terra::ext(dem)), as.vector(terra::ext(bb)),
               tolerance = 1.e-5)
})

test_that("Download DEM data can be retrieved from the cache on new calls", {
  skip_on_ci()

  # setup cache directory
  cache_dir <- temp_cache_dir()

  # calling load_dem should create a file in the cache folder
  expect_message(load_dem(bb, asset_urls, force_download = TRUE),
                 "Saving data to cache directory")
  cached_filename <- list.files(cache_dir, pattern = "^dem_")
  cached_filepath <- file.path(cache_dir, cached_filename)
  expect_true(file.exists(cached_filepath))

  # calling load_dem again should read data from the cached file, raising a
  # warning that includes the path to the cached file as well
  expect_warning(load_dem(bb, asset_urls, force_download = FALSE),
                 cached_filepath)
})

test_that("valley polygon is correctly constructed", {
  dem <- terra::unwrap(bucharest_dem)
  river <- bucharest_osm$river_surface

  valley <- get_valley(dem, river)
  expected_valley_path <- testthat::test_path("testdata",
                                              "expected_valley.gpkg")
  expected_valley <- sf::st_read(expected_valley_path, quiet = TRUE)

  valley <- sf::st_set_precision(valley, 1e-06)
  expected_valley <- sf::st_set_precision(expected_valley, 1e-06)
  expect_true(sf::st_equals_exact(valley, expected_valley,
                                  par = 0, sparse = FALSE))
})
