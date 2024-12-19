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
  asset_urls_retrieved_default <- get_stac_asset_urls(bb)
  expected_asset_urls <- asset_urls

  expect_equal(expected_asset_urls, asset_urls_retrieved)
  expect_equal(expected_asset_urls, asset_urls_retrieved_default)
})

test_that("raster data are correctly retrieved and merged", {
  skip_on_ci()

  dem <- load_raster(bb, asset_urls) |> CRiSp::reproject(32635)
  expected_dem <- terra::unwrap(bucharest_dem)

  expect_equal(terra::values(dem), terra::values(expected_dem))
  expect_equal(terra::crs(dem), terra::crs(expected_dem))
  expect_true(all.equal(terra::ext(dem), terra::ext(expected_dem),
                        tolerance = 1e-4))
})

test_that("valley polygon is correctly constructed", {
  dem <- terra::unwrap(bucharest_dem)
  river <- bucharest_osm$river_surface
  crs <- "epsg:32635"

  valley <- get_valley(dem, river, crs)
  expected_valley <- sf::st_read("./testdata/expected_valley.gpkg",
                                 quiet = TRUE) |>
    sf::st_as_sfc()

  valley <- sf::st_set_precision(valley, 1e-8)
  expected_valley <- sf::st_set_precision(expected_valley, 1e-8)
  expect_true(sf::st_equals(valley, expected_valley, sparse = FALSE))
})
