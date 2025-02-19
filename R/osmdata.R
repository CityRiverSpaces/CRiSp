#' Retrieve OpenStreetMap data as sf object
#'
#' Query the Overpass API for a key:value pair within a given bounding box.
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")
#'
#' @return An sf object with the retrieved OpenStreetMap data
#' @export
osmdata_as_sf <- function(key, value, bb) {
  bbox <- as_bbox(bb)  # it should be in lat/lon
  bbox |>
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
#' the city boundary, the river centreline and surface, the streets, and the
#' railways.
#'
#' @param city_name A character string with the name of the city.
#' @param river_name A character string with the name of the river.
#' @param buffer_in_m An integer with the buffer size in meters
#'             around the river center line.
#' @param crs An integer with the EPSG code for the projection. If no CRS is
#'            specified, the default is the UTM zone for the city.
#'
#' @return An list with the retrieved OpenStreetMap data sets for the
#'         given location
#' @export
#'
#' @examples
#' get_osmdata("Bucharest", "Dambovita", 100, crs)

get_osmdata <- function(
  city_name, river_name, buffer_in_m = NULL, crs = NULL
) {
  bb <- get_osm_bb(city_name)
  if (is.null(crs)) crs <- get_utm_zone(bb)

  boundary <- get_osm_city_boundary(bb, city_name, crs = crs)

  # Retrieve the river center line and surface, cropped to bb
  river <- get_osm_river(bb, river_name, crs = crs)

  # Use the river center line as bounding object to retrieve streets and
  # railways
  bounding_obj <- sf::st_transform(river$centerline, sf::st_crs(bb))

  # Apply the buffer around the river center line
  if (!is.null(buffer_in_m)) {
    bounding_obj <- buffer_obj(bounding_obj, buffer_in_m)
  }

  # Retrieve streets and railways
  streets <- get_osm_streets(bounding_obj, crs = crs)
  railways <- get_osm_railways(bounding_obj, crs = crs)

  list(
    aoi = bounding_obj,
    boundary = boundary,
    river_centerline = river$centerline,
    river_surface = river$surface,
    streets = streets,
    railways = railways
  )
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
#'
#' @return An sf object with the city boundary
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_city_boundary(bb, "Bucharest", crs)
get_osm_city_boundary <- function(bb, city_name, crs = NULL, multiple = FALSE) {
  # Define a helper function to fetch the city boundary
  fetch_boundary <- function(key, value) {
    osmdata_as_sf(key, value, bb)$osm_multipolygons |>
      dplyr::filter(
        .data$`name:en` == stringr::str_extract(city_name, "^[^,]+") |
          .data$name == stringr::str_extract(city_name, "^[^,]+")
      ) |>
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
#'
#' @return A list with the river centreline and surface
#' @export
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_river(bb, "Dâmbovița", crs)
get_osm_river <- function(bb, river_name, crs = NULL) {
  # Get the river centreline
  river_centerline <- osmdata_as_sf("waterway", "river", bb)
  river_centerline <- river_centerline$osm_multilines |>
    dplyr::filter(.data$name == river_name) |>
    # the query can return more features than actually intersecting the bb
    sf::st_filter(sf::st_as_sfc(bb), .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_crop(bb)

  # Get the river surface
  river_surface <- osmdata_as_sf("natural", "water", bb)
  river_surface <- dplyr::bind_rows(river_surface$osm_polygons,
                                    river_surface$osm_multipolygons) |>
    sf::st_geometry() |>
    sf::st_as_sf() |>
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
#' @param bb Bounding box of class `bbox`
#' @param crs Coordinate reference system as EPSG code
#' @param highway_values A character vector with the highway values to retrieve.
#'             If left NULL, the function retrieves the following values:
#'             "motorway", "trunk", "primary", "secondary", "tertiary"
#' @return An sf object with the streets
#' @export
#' @importFrom rlang !! sym
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_streets(bb, crs)
get_osm_streets <- function(bb, crs = NULL, highway_values = NULL) {
  if (is.null(highway_values)) {
    highway_values <- c("motorway", "trunk", "primary", "secondary", "tertiary")
  }

  link_values <- sapply(X = highway_values,
                        FUN = \(x) sprintf("%s_link", x),
                        USE.NAMES = FALSE)

  streets <- osmdata_as_sf("highway", c(highway_values, link_values), bb)

  # Cast polygons (closed streets) into lines
  poly_to_lines <- suppressWarnings(
    streets$osm_polygons |> sf::st_cast("LINESTRING")
  )

  # Combine all features in one data frame
  streets_lines <- streets$osm_lines |>
    dplyr::bind_rows(poly_to_lines) |>
    dplyr::select("highway") |>
    dplyr::rename(!!sym("type") := !!sym("highway"))

  if (!is.null(crs)) streets_lines <- sf::st_transform(streets_lines, crs)

  streets_lines
}

#' Get OpenStreetMap railways
#'
#' @param bb Bounding box of class `bbox`
#' @param crs Coordinate reference system as EPSG code
#'
#' @return An sf object with the railways
#' @export
#' @importFrom rlang !! sym
#'
#' @examples
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_railways(bb, crs)
get_osm_railways <- function(bb, crs = NULL) {
  railways <- osmdata_as_sf("railway", "rail", bb)
  railways_lines <- railways$osm_lines |>
    dplyr::select("railway") |>
    dplyr::rename(!!sym("type") := !!sym("railway"))

  if (!is.null(crs)) railways_lines <- sf::st_transform(railways_lines, crs)

  railways_lines
}
