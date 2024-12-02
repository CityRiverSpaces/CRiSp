test_that("STAC asset urls are correctly retrieved", {
  skip_on_ci()
  bb <- osmdata::getbb("Bucharest")
  ep <- "https://earth-search.aws.element84.com/v1"
  col <- "cop-dem-glo-30"

  asset_urls_retrieved <- get_stac_asset_urls(
                                              bb, endpoint = ep,
                                              collection = col)
  asset_urls_retrieved_default <- get_stac_asset_urls(bb)

  expasset1 <- paste0(
                      "s3://copernicus-dem-30m/",
                      "Copernicus_DSM_COG_10_N44_00_E026_00_DEM/",
                      "Copernicus_DSM_COG_10_N44_00_E026_00_DEM.tif")
  expasset2 <- paste0(
                      "s3://copernicus-dem-30m/",
                      "Copernicus_DSM_COG_10_N44_00_E025_00_DEM/",
                      "Copernicus_DSM_COG_10_N44_00_E025_00_DEM.tif")

  expected_asset_urls <- c(expasset1, expasset2)
  expect_equal(expected_asset_urls, asset_urls_retrieved)
  expect_equal(expected_asset_urls, asset_urls_retrieved_default)

})


test_that("raster data are correctly retrieved and merged", {
  skip_on_ci()
  bb <- osmdata::getbb("Bucharest")
  rp1 <- paste0(
                "s3://copernicus-dem-30m/",
                "Copernicus_DSM_COG_10_N44_00_E026_00_DEM/",
                "Copernicus_DSM_COG_10_N44_00_E026_00_DEM.tif")
  rp2 <- paste0(
                "s3://copernicus-dem-30m/",
                "Copernicus_DSM_COG_10_N44_00_E025_00_DEM/",
                "Copernicus_DSM_COG_10_N44_00_E025_00_DEM.tif")

  raster_paths <- c(rp1, rp2)

  dem <- load_raster(bb, raster_paths)
  expected_dem <- terra::rast("path_to_example_data") #TODO add path and data
  expect_equal(dem, expected_dem)
})

test_that("valley polygon is correctly constructed", {
  skip_on_ci()
  dem <- terra::rast("path_to_example_demd") #TODO add path and data
  river_geoms <- st_read("path_to_example_river") #TODO add path and data
  river <- river_geoms[1, ]  # select waterbody extent
  crs <- "epsg:32635"

  valley <- get_valley(dem, river, crs)
  expected_valley <- st_read("path_to_example_valley")
  #TODO add example valley and path

  expect_equal(valley, expected_valley, tolerance = 1e-4)

})