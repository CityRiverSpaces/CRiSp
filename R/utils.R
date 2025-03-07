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

#' Apply a buffer region to a sf object
#'
#' If the input object is in lat/lon coordinates, the buffer is approximately
#' applied by first transforming the object in a suitable projected CRS,
#' expanding it with the given buffer, and then transforming
#' it back to the lat/lon system.
#'
#' @param obj A sf object
#' @param buffer_distance Buffer distance in meters
#' @return Expanded sf object
buffer <- function(obj, buffer_distance) {
  is_obj_longlat <- sf::st_is_longlat(obj)
  dst_crs <- sf::st_crs(obj)
  # check if obj is a bbox
  is_obj_bbox <- inherits(obj, "bbox")
  if (is_obj_bbox) obj <- sf::st_as_sfc(obj)
  if (is_obj_longlat) {
    crs_meters <- get_utm_zone(obj)
    obj <- sf::st_transform(obj, crs_meters)
  }
  expanded_obj <- sf::st_buffer(obj, buffer_distance)
  if (is_obj_longlat) {
    expanded_obj <- sf::st_transform(expanded_obj, dst_crs)
  }
  if (is_obj_bbox) expanded_obj <- sf::st_bbox(expanded_obj)
  expanded_obj
}

#' Draw a corridor as a fixed buffer region around a river.
#'
#' The river geometry may consist of multiple spatial features, these are merged
#' after applying the buffer.
#'
#' @param river A simple feature geometry representing the river
#' @param buffer_distance Size of the buffer (in the river's CRS units)
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry
river_buffer <- function(river, buffer_distance, bbox = NULL) {
  river_buf <- buffer(river, buffer_distance)
  river_buf_union <- sf::st_union(river_buf)
  if (!is.null(bbox)) {
    sf::st_crop(river_buf_union, bbox)
  } else {
    river_buf_union
  }
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
