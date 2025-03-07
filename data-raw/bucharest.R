# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000

# Fetch the OSM data
bbox <- get_osm_bb(city_name)
bbox_expanded <- buffer_bbox(bbox, bbox_buffer)
bucharest_osm <- get_osmdata(bbox_expanded, city_name, river_name,
                             crs = epsg_code, force_download = TRUE)

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
bucharest_dem <- get_dem(bucharest_osm$bb) |>
  reproject(epsg_code) |>
  # SpatRaster objects cannot be directly serialized as RDS/RDA files
  terra::wrap()

# Save as package data
usethis::use_data(bucharest_osm, overwrite = TRUE)
usethis::use_data(bucharest_dem, overwrite = TRUE)
usethis::use_data(bucharest_delineation, overwrite = TRUE)
