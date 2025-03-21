#' Retrieve OpenStreetMap data as sf object
#'
#' Query the Overpass API for a key:value pair within a given bounding box
#' (provided as lat/lon coordiates). Results are cached, so that new queries
#' with the same input parameters will be loaded from disk.
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#' @param aoi An area of interest, provided either as as sf object or "bbox" or
#' as a vector ("xmin", "ymin", "xmax", "ymax")
#' @param force_download Download data even if cached data is available
#'
#' @return An sf object with the retrieved OpenStreetMap data
#' @export
osmdata_as_sf <- function(key, value, aoi, force_download = FALSE) {
  bbox <- as_bbox(aoi) # it should be in lat/lon

  filepath <- get_osmdata_cache_filepath(key, value, bbox)

  if (file.exists(filepath) && !force_download) {
    osmdata_sf <- read_data_from_cache(filepath)
    return(osmdata_sf)
  }

  osmdata_sf <- osmdata_query(key, value, bbox)

  write_data_to_cache(osmdata_sf, filepath)

  osmdata_sf
}

#' Query the Overpass API for a key:value pair within a bounding box
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data. If
#'  value = "" means that you get all features available in OSM for the
#'  specified bounding box
#' @param bb A bounding box, in lat/lon coordinates
#'
#' @return An sf object with the retrieved OpenStreetMap data
osmdata_query <- function(key, value, bb) {
  # this is needed because the add_osm_feature does not support
  # value as an empty string
  if (all(value == "")) value <- NULL
  bb |>
    osmdata::opq() |>
    osmdata::add_osm_feature(key = key, value = value) |>
    osmdata::osmdata_sf()
}

#' Get the bounding box of a city
#'
#' @param city_name The name of the city
#'
#' @return A bbox object with the bounding box of the city
#' @export
#'
#' @examples
#' get_osm_bb("Bucharest")
get_osm_bb <- function(city_name) {
  bb <- osmdata::getbb(city_name)
  as_bbox(bb)
}

#' Retrieve OpenStreetMap data for a given location
#'
#' Retrieve OpenStreetMap data for a given location, including
#' the city boundary, the river centreline and surface, the streets, the
#' railways, and the buildings
#'
#' @param city_name A character string with the name of the city.
#' @param river_name A character string with the name of the river.
#' @param network_buffer Buffer distance in meters around the river
#'   to get the streets and railways, default is 0 means no
#'   network data will be downloaded
#' @param buildings_buffer Buffer distance in meters around the river
#'   to get the buildings, default is 0 means no
#'   buildings data will be downloaded
#' @param crs An integer with the EPSG code for the projection. If no CRS is
#'   specified, the default is the UTM zone for the city.
#' @param force_download Download data even if cached data is available
#'
#' @return An list with the retrieved OpenStreetMap data sets for the
#'         given location
#' @export
#'
#' @examples
#' get_osmdata("Bucharest", "Dâmbovița")

get_osmdata <- function(
  city_name, river_name, network_buffer = NULL, buildings_buffer = NULL,
  crs = NULL, force_download = FALSE
) {
  bb <- get_osm_bb(city_name)
  if (is.null(crs)) crs <- get_utm_zone(bb)

  boundary <- get_osm_city_boundary(
    bb, city_name, crs = crs, force_download = force_download
  )

  # Retrieve the river center line and surface
  river <- get_osm_river(
    bb, river_name, crs = crs, force_download = force_download
  )

  osm_data <- list(
    bb = bb,
    boundary = boundary,
    river_centerline = river$centerline,
    river_surface = river$surface
  )

  # Retrieve streets and railways based on the aoi
  if (!is.null(network_buffer)) {
    aoi_network <- get_river_aoi(river, bb, buffer_distance = network_buffer)
    osm_data <- append(osm_data, list(aoi_network = aoi_network))
    osm_data <- append(osm_data, list(
      streets = get_osm_streets(aoi_network, crs = crs,
                                force_download = force_download)
    ))
    osm_data <- append(osm_data, list(
      railways = get_osm_railways(aoi_network, crs = crs,
                                  force_download = force_download)
    ))
  }

  # Retrieve buildings based on a different aoi
  if (!is.null(buildings_buffer)) {
    aoi_buildings <- get_river_aoi(river, bb,
                                   buffer_distance = buildings_buffer)
    osm_data <- append(osm_data, list(aoi_buildings = aoi_buildings))
    osm_data <- c(osm_data, list(
      buildings = get_osm_buildings(aoi_buildings, crs = crs,
                                    force_download = force_download)
    ))
  }

  osm_data
}

#' Get the city boundary from OpenStreetMap
#'
#' This function retrieves the city boundary from OpenStreetMap based on a
#' bounding box with the OSM tags "place:city" and "boundary:administrative".
#' The result is filtered by the city name.
#'
#' @param bb Bounding box of class `bbox`
#' @param city_name A character string with the name of the city
#' @param crs Coordinate reference system as EPSG code
#' @param multiple A logical indicating if multiple city boundaries should be
#'                 returned. By default, only the first one is returned.
#' @param force_download Download data even if cached data is available
#'
#' @return An sf object with the city boundary
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_city_boundary(bb, "Bucharest", crs)
get_osm_city_boundary <- function(bb, city_name, crs = NULL, multiple = FALSE,
                                  force_download = FALSE) {
  # Define a helper function to fetch the city boundary
  fetch_boundary <- function(key, value) {
    osmdata_sf <- osmdata_as_sf(key, value, bb, force_download = force_download)
    osmdata_sf$osm_multipolygons |>
      # filter using any of the "name" columns (matching different languages)
      match_osm_name(city_name) |>
      sf::st_geometry()
  }

  # Try to get the city boundary with the "place:city" tag
  city_boundary <- tryCatch(fetch_boundary("place", "city"),
                            error = function(e) NULL)

  # If not found, try again with the "boundary:administrative" tag
  if (is.null(city_boundary)) {
    city_boundary <- tryCatch(fetch_boundary("boundary", "administrative"),
                              error = function(e) NULL)
  }

  # If still not found, throw an error
  if (is.null(city_boundary)) {
    stop("No city boundary found. The city name may be incorrect.")
  }

  if (!is.null(crs)) city_boundary <- sf::st_transform(city_boundary, crs)

  if (length(city_boundary) > 1) {
    if (!multiple) {
      message("Multiple boundaries were found. Using the first one.")
      return(city_boundary[1])
    } else {
      message("Multiple boundaries were found. Returning all.")
    }
  }

  city_boundary
}

#' Get the river centreline and surface from OpenStreetMap
#'
#' @param bb Bounding box of class `bbox`
#' @param river_name The name of the river
#' @param crs Coordinate reference system as EPSG code
#' @param force_download Download data even if cached data is available
#'
#' @return A list with the river centreline and surface
#' @export
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_river(bb, "Dâmbovița", crs)
get_osm_river <- function(bb, river_name, crs = NULL, force_download = FALSE) {
  # Get the river centreline
  river_centerline <- osmdata_as_sf("waterway", "river", bb,
                                    force_download = force_download)
  river_centerline <- river_centerline$osm_multilines |>
    # filter using any of the "name" columns (matching different languages)
    match_osm_name(river_name) |>
    # the query can return more features than actually intersecting the bb
    sf::st_filter(sf::st_as_sfc(bb), .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_union()

  # Get the river surface
  river_surface <- osmdata_as_sf("natural", "water", bb,
                                 force_download = force_download)
  river_surface <- dplyr::bind_rows(river_surface$osm_polygons,
                                    river_surface$osm_multipolygons) |>
    sf::st_geometry() |>
    sf::st_as_sf() |>
    # natural:water retrieved some invalid polygons, fix these
    sf::st_make_valid() |>
    sf::st_crop(bb) |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_union()

  if (!is.null(crs)) {
    river_centerline <- sf::st_transform(river_centerline, crs)
    river_surface <- sf::st_transform(river_surface, crs)
  }

  list(centerline = river_centerline, surface = river_surface)
}

#' Get OpenStreetMap streets
#'
#' @param aoi Area of interest as sf object or bbox
#' @param crs Coordinate reference system as EPSG code
#' @param highway_values A character vector with the highway values to retrieve.
#'             If left NULL, the function retrieves the following values:
#'             "motorway", "trunk", "primary", "secondary", "tertiary"
#' @param force_download Download data even if cached data is available
#'
#' @return An sf object with the streets
#' @export
#' @importFrom rlang !! sym
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_streets(bb, crs)
get_osm_streets <- function(aoi, crs = NULL, highway_values = NULL,
                            force_download = FALSE) {
  if (is.null(highway_values)) {
    highway_values <- c("motorway", "trunk", "primary", "secondary", "tertiary")
  }

  link_values <- sapply(X = highway_values,
                        FUN = \(x) sprintf("%s_link", x),
                        USE.NAMES = FALSE)

  streets <- osmdata_as_sf("highway", c(highway_values, link_values), aoi,
                           force_download = force_download)

  # Cast polygons (closed streets) into lines
  poly_to_lines <- suppressWarnings(
    streets$osm_polygons |> sf::st_cast("LINESTRING")
  )

  # Combine all features in one data frame
  streets_lines <- streets$osm_lines |>
    dplyr::bind_rows(poly_to_lines) |>
    dplyr::select("highway") |>
    dplyr::rename(!!sym("type") := !!sym("highway"))

  # Intersect with the bounding polygon
  # this will return a warning, see https://github.com/r-spatial/sf/issues/406
  if (inherits(aoi, "bbox")) aoi <- sf::st_as_sfc(aoi)
  mask <- sf::st_intersects(streets_lines, aoi, sparse = FALSE)
  streets_lines <- streets_lines[mask, ]

  if (!is.null(crs)) streets_lines <- sf::st_transform(streets_lines, crs)

  streets_lines
}

#' Get OpenStreetMap railways
#'
#' @param aoi Area of interest as sf object or bbox
#' @param crs Coordinate reference system as EPSG code
#' @param force_download Download data even if cached data is available
#'
#' @return An sf object with the railways
#' @export
#' @importFrom rlang !! sym
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_railways(bb, crs)
get_osm_railways <- function(aoi, crs = NULL, force_download = FALSE) {
  railways <- osmdata_as_sf("railway", "rail", aoi,
                            force_download = force_download)
  railways_lines <- railways$osm_lines |>
    dplyr::select("railway") |>
    dplyr::rename(!!sym("type") := !!sym("railway"))

  # Intersect with the bounding polygon
  if (inherits(aoi, "bbox")) aoi <- sf::st_as_sfc(aoi)
  mask <- sf::st_intersects(railways_lines, aoi, sparse = FALSE)
  railways_lines <- railways_lines[mask, ]

  if (!is.null(crs)) railways_lines <- sf::st_transform(railways_lines, crs)

  railways_lines
}

#' Get OpenStreetMap buildings
#'
#' Get buildings from OpenStreetMap within a given buffer around a river.
#'
#' @param aoi Area of interest as sf object or bbox
#' @param crs Coordinate reference system as EPSG code
#' @param force_download Download data even if cached data is available
#'
#' @return An sf object with the buildings
#' @export
get_osm_buildings <- function(aoi, crs = NULL, force_download = FALSE) {
  buildings <- osmdata_as_sf("building", "", aoi,
                             force_download = force_download)
  buildings <- buildings$osm_polygons |>
    sf::st_filter(aoi, .predicate = sf::st_intersects) |>
    dplyr::filter(.data$building != "NULL") |>
    sf::st_geometry()

  if (!is.null(crs)) buildings <- sf::st_transform(buildings, crs)

  buildings
}

#' Get an area of interest (AoI) around a river, cropping to the bounding box of
#' a city
#'
#' @param river A list with the river centreline and surface geometries
#' @param city_bbox Bounding box around the city
#' @param buffer_distance Buffer size around the river
#' @return An sf object in lat/lon coordinates
#' @export
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' river <- get_osm_river(bb, "Dâmbovița")
#' get_river_aoi(river, bb, buffer_distance = 100)
get_river_aoi <- function(river, city_bbox, buffer_distance) {
  river <- c(river$centerline, river$surface)

  # Make sure crs are the same for cropping with bb
  river <- sf::st_transform(river, sf::st_crs(city_bbox))

  river_buffer(river, buffer_distance, bbox = city_bbox)
}


#' Match OpenStreetMap data by name
#'
#' @param osm_data An sf object with OpenStreetMap data
#' @param name A character string with the name to match
#'
#' @return sf object containing only rows with filtered name
#' @keywords internal
match_osm_name <- function(osm_data, name) {
  osm_data |>
    dplyr::filter(dplyr::if_any(dplyr::matches("name"),
                                \(x) x == name))
}
