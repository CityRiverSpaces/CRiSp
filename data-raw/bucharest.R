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
st_crs(city_boundary)$wkt <- gsub("°|º", "\\\u00b0",
                                  st_crs(city_boundary)$wkt)

bb <- getbb(city_name)
aoi <- define_aoi(bb, epsg_code, bbox_buffer)
bbox_expanded <- aoi |> st_transform(4326) |> st_bbox()

river_centerline <- osmdata_as_sf("waterway", "river", bb)$osm_multilines |>
  filter(name == river_name) |>
  st_transform(epsg_code) |>
  st_geometry() |>
  st_intersection(st_buffer(aoi, bbox_buffer))
st_crs(river_centerline)$wkt <- gsub("°|º", "\\\u00b0",
                                     st_crs(river_centerline)$wkt)

river_surface <- osmdata_as_sf("natural", "water", bb)
river_surface <- river_surface$osm_multipolygons |>
  bind_rows(river_surface$osm_polygons) |>
  st_transform(epsg_code) |>
  st_filter(river_centerline, .predicate = st_intersects) |>
  st_geometry() |>
  st_union()
st_crs(river_surface)$wkt <- gsub("°|º", "\\\u00b0",
                                  st_crs(river_surface)$wkt)

highway_values <- c("motorway", "primary", "secondary", "tertiary")
streets <- osmdata_as_sf("highway", highway_values, bb)
streets <- merge_streets(streets) |>
  select("highway") |>
  st_transform(epsg_code)
st_crs(streets)$wkt <- gsub("°|º", "\\\u00b0", st_crs(streets)$wkt)

railways <- osmdata_as_sf("railway", "rail", bbox_expanded)
railways_lines <- railways$osm_lines |>
  select("railway") |>  # only keep "railway" column
  rename(type = `railway`) |>  # rename it to "type"
  st_transform(epsg_code)
st_crs(railways_lines)$wkt <- gsub("°|º", "\\\u00b0",
                                   st_crs(railways_lines)$wkt)

bucharest <- list(
  bb = bb,
  boundary = city_boundary,
  river_centerline = river_centerline,
  river_surface = river_surface,
  streets = streets,
  railways_lines = railways_lines
)

usethis::use_data(bucharest, overwrite = TRUE)
