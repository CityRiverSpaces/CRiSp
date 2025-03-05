#' Set the units of x as the units of y
#'
#' @param x x (can be unitless)
#' @param y y (can be unitless)
#' @return Object x with units of y
set_units_like <- function(x, y) {
  has_units_x <- inherits(x, "units")
  has_units_y <- inherits(y, "units")
  if ((!has_units_x) && (!has_units_y)) {
    x
  } else if (has_units_y) {
    units::set_units(x, units(y), mode = "standard")
  } else {
    units::drop_units(x)
  }
}

#' Get the UTM zone of a spatial object
#'
#' @param x Bounding box or geometry object
#' @return The EPSG of the UTM zone
#' @export
get_utm_zone <- function(x) {
  bb <- as_bbox(x)

  centroid_long <- (bb[["xmin"]] + bb[["xmax"]]) / 2
  centroid_lat <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  base <- if (centroid_lat >= 0) 32600 else 32700
  base + floor((centroid_long + 180) / 6) + 1
}

#' Get the bounding box from the x object
#'
#' If the x does not have a CRS, WGS84 is assumed.
#'
#' @param x Simple feature object (or compatible) or a bounding box, provided
#'   either as a matrix (with x, y as rows and min, max as columns) or as a
#'   vector (xmin, ymin, xmax, ymax)
#' @return A bounding box as returned by [`sf::st_bbox()`]
#' @export
as_bbox <- function(x) {
  if (inherits(x, c("numeric", "matrix"))) {
    x <- as.vector(x)
    names(x) <- c("xmin", "ymin", "xmax", "ymax")
  }
  bbox <- sf::st_bbox(x)
  crs <- sf::st_crs(bbox)
  if (is.na(crs)) sf::st_crs(bbox) <- sf::st_crs("EPSG:4326")
  bbox
}

#' Apply a buffer region to a bounding box
#'
#' If the input bbox is in lat/lon coordinates, the buffer is approximately
#' applied by first transforming the bbox in a suitable projected coordinate
#' reference system, expanding it with the given buffer, transforming it back to
#' the lat/lon system, and finally taking the bounding box of the obtained area.
#'
#' @param bbox Bonding box as a simple feature object
#' @param buffer Buffer region in meters
#' @return Expanded bounding box as a simple feature object
buffer_bbox <- function(bbox, buffer) {
  is_bbox_longlat <- sf::st_is_longlat(bbox)
  bbox_sfc <- sf::st_as_sfc(bbox)
  if (is_bbox_longlat) {
    crs_meters <- get_utm_zone(bbox)
    bbox_sfc <- sf::st_transform(bbox_sfc, crs_meters)
  }
  bbox_sfc_buffer <- sf::st_buffer(bbox_sfc, buffer)
  if (is_bbox_longlat) {
    bbox_sfc_buffer <- sf::st_transform(bbox_sfc_buffer, sf::st_crs(bbox))
  }
  sf::st_bbox(bbox_sfc_buffer)
}


#' Extend a line by a given distance
#'
#' @param line Linestring object
#' @param distance Distance to extend the line
#'
#' @return Extended linestring object
extend_line <- function(line, distance) {
  if (distance < 0) {
    stop("Distance must be a positive numeric value.")
  }

  if (distance == 0) {
    return(line)
  }

  # Extract start and end points
  coords <- sf::st_coordinates(line)[, 1:2]  # Get X and Y coordinates
  start <- coords[1, ]
  end <- coords[nrow(coords), ]

  # Compute direction vectors
  direction <- (end - start) / sqrt(sum((end - start)^2))  # Normalize

  # Extend both ends
  new_start <- start - direction * distance
  new_end <- end + direction * distance

  # Create new extended line
  new_coords <- rbind(new_start, coords, new_end)
  new_line <- sf::st_linestring(new_coords) |> sf::st_sfc()
  sf::st_crs(new_line) <- sf::st_crs(line)
  new_line
}

#' Reproject a raster or vector dataset to the specified
#' coordinate reference system (CRS)
#'
#' @param x Raster or vector object
#' @param crs CRS to be projected to
#' @param ... Optional arguments for raster or vector reproject functions
#'
#' @return Object reprojected to specified CRS
#' @export
reproject <- function(x, crs, ...) {
  if (inherits(x, "SpatRaster")) {
    # terra::crs does not support a numeric value as CRS, convert to character
    if (inherits(crs, "numeric")) crs <- sprintf("EPSG:%s", crs)
    terra::project(x, crs, ...)
  } else if (inherits(x, c("bbox", "sfc", "sf"))) {
    sf::st_transform(x, crs, ...)
  } else {
    stop(sprintf("Cannot reproject object type: %s", class(x)))
  }
}

#' Load raster data from one or multiple (remote) files
#'
#' If a bounding box is provided, the file(s) are cropped for the given extent.
#' The resulting rasters are then merged using [`terra::merge`].
#'
#' @param urlpaths Path or URL to the raster file(s)
#' @param bbox A bounding box
#'
#' @return Raster data as a [`terra::SpatRaster`] object
load_raster <- function(urlpaths, bbox = NULL) {
  rasters <- lapply(urlpaths, terra::rast)
  if (!is.null(bbox)) {
    # snap spatial extent outward to include pixels crossed by the boundary
    rasters <- lapply(rasters, terra::crop, terra::ext(bbox), snap = "out")
  }
  if (length(rasters) > 1) {
    do.call(terra::merge, args = rasters)
  } else {
    rasters[[1]]
  }
}
