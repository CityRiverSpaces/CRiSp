dem <- terra::unwrap(CRiSp::bucharest_dem)
river <- CRiSp::bucharest_osm$river_surface
crs <- "epsg:32635"

valley <- CRiSp::get_valley(dem, river, crs)

filepath <- testthat::test_path("testdata", "expected_valley.gpkg")
sf::st_write(valley, filepath, append = FALSE, quiet = TRUE)
