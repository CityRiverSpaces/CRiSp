# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
network_buffer <- 2500
buildings_buffer <- 100
dem_buffer <- 2500

# Fetch the OSM data
bucharest_osm <- get_osmdata(
  city_name,
  river_name,
  crs = epsg_code,
  network_buffer = network_buffer,
  buildings_buffer = buildings_buffer,
  force_download = TRUE
)

# Add delineation to package data
bucharest_delineation <- delineate(city_name, river_name, crs = epsg_code,
                                   corridor = TRUE, segments = TRUE,
                                   riverspace = TRUE)

# Fix encoding issue in the WKT strings
fix_wkt_encoding <- function(x) {
  wkt <- sf::st_crs(x)$wkt
  sf::st_crs(x)$wkt <- gsub("°|º", "\\\u00b0", wkt)  # replace with ASCII code
  x
}
bucharest_osm <- lapply(bucharest_osm, fix_wkt_encoding)

# Fetch the DEM data
aoi_buff <- buffer(bucharest_osm$aoi, dem_buffer)
bucharest_dem <- get_dem(aoi_buff, crs = epsg_code, force_download = TRUE) |>
  # SpatRaster objects cannot be directly serialized as RDS/RDA files
  terra::wrap()

# Save as package data
usethis::use_data(bucharest_osm, overwrite = TRUE)
usethis::use_data(bucharest_dem, overwrite = TRUE)
usethis::use_data(bucharest_delineation, overwrite = TRUE)
