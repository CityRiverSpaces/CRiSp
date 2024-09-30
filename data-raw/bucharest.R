library(CRiSp)
library(osmdata)
library(dplyr)
library(sf)

city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000

city_boundary <- get_osm_city_boundary(city_name) |>
  st_transform(epsg_code)

bb <- getbb(city_name)
aoi <- define_aoi(bb, epsg_code, bbox_buffer)

river_centerline <- osmdata_as_sf("waterway", "river", bb)$osm_multilines |>
  filter(name == river_name) |>
  st_transform(epsg_code) |>
  st_geometry() |>
  st_intersection(st_buffer(aoi, bbox_buffer))

river_surface <- osmdata_as_sf("natural", "water", bb)
river_surface <- river_surface$osm_multipolygons |>
  bind_rows(river_surface$osm_polygons) |>
  st_transform(epsg_code) |>
  st_filter(river_centerline, .predicate = st_intersects) |>
  st_geometry() |>
  st_union()

highway_values <- c("motorway", "primary", "secondary", "tertiary")
streets <- osmdata_as_sf("highway", highway_values, bb)
streets <- merge_streets(streets) |>
  select("highway")

bucharest <- list(
  bb = bb,
  boundary = city_boundary,
  river_centerline = river_centerline,
  river_surface = river_surface,
  streets = streets
)

usethis::use_data(bucharest, overwrite = TRUE)
