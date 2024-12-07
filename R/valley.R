#' Load dem from a STAC endpoint
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")
#' @param resource from which to source dem. Can be "STAC".
#' if "STAC" the parameters the following parameters
#' must be supplied as named parameters. If omitted defaults
#' will be used.
#' @param ... Additional parameters to be passed depending on the resource.
#'            In case `resource = "STAC"`, the arguments `endpoint` and
#'            `collection` need to be passed to `get_stac_asset_urls()`.
#'
#' @return dem
#' @export
get_dem <- function(bb, resource = "STAC", ...) {
  bbox <- as_bbox(bb)
  args <- list(...)
  if (resource == "STAC") {
    if (length(args) && !is.null(...)) {
      endpoint <- args$endpoint
      collection <- args$collection
      asset_urls <- get_stac_asset_urls(bbox,
                                        endpoint = endpoint,
                                        collection = collection)
    } else {
      asset_urls <- get_stac_asset_urls(bbox)
    }
    dem <- load_raster(bbox, asset_urls)
    return(dem)
  } else {
    stop(sprintf("Resource %s unknown", resource))
  }
}

#' Create vector/polygon representation of valley from dem and river polygon
#' for a crs
#'
#' @param dem of region
#' @param river vector/polygon representation of river area
#' @param crs coordiante reference system to use
#'
#' @return (multi)polygon representation of valley
#' area as st_geometry without holes
#' @export
get_valley <- function(dem, river, crs) {
  dem_repr <- reproject(dem, crs)
  river_repr <- reproject(river, crs)
  cd_masked <- smooth_dem(dem_repr) |>
    get_slope() |>
    get_cost_distance(river_repr) |>
    mask_cost_distance(river_repr)

  cd_thresh <- get_cd_char(cd_masked)

  valley <- get_valley_mask(cd_masked, cd_thresh) |>
    get_valley_polygon()
  return(valley)
}

#' Retrieve asset urls for the intersection of a bounding box with a
#' remote STAC endpoint
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")
#' @param endpoint url of (remote) STAC endpoint
#' @param collection STAC collection to be queried
#'
#' @return A list of urls for the assets in the collection
#' overlapping with the specified bounding box
#' @export
get_stac_asset_urls <- function(
    bb,
    endpoint = "https://earth-search.aws.element84.com/v1",
    collection = "cop-dem-glo-30") {
  bbox <- as_bbox(bb)
  it_obj <- rstac::stac(endpoint) |>
    rstac::stac_search(collections = collection, bbox = bbox) |>
    rstac::get_request()
  asset_urls <- rstac::assets_url(it_obj)
  return(asset_urls)
}

#' Retrieve STAC records (of a DEM) corresponding to a list of asset urls,
#' crop and merge with a specified bounding box to create a dem of the
#' specified region
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")
#' @param raster_urlpaths a list of STAC records to be retrieved
#'
#' @return A a merged dem from retrieved assets cropped to the bounding box
#' @export
load_raster <- function(bb, raster_urlpaths) {
  bbox <- as_bbox(bb)
  raster_urlpaths |>
    lapply(terra::rast) |>
    lapply(terra::crop, terra::ext(bbox)) |>
    do.call(terra::merge, args = _)
}

#' Write dem to cloud optimized GeoTiff file as specified location
#'
#' @param dem to write to file
#' @param fpath filepath for output. If no output directory is specified
#' (see below) fpath is parsed to determine
#' the output directory
#' @param output_directory where file should be written.
#' If specified fpath is treated as filename only.
#'
#' @export
dem_to_cog <- function(dem, fpath, output_directory = NULL) {
  if (is.null(output_directory)) {
    filename <- basename(fpath)
    directory_name <- dirname(fpath)
  } else {
    filename <- fpath
    directory_name <- output_directory
  }
  data_dir <- directory_name
  terra::writeRaster(
                     x = dem,
                     filename = sprintf("%s/%s", data_dir, filename),
                     filetype = "COG",
                     overwrite = TRUE)
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
    wkt <- sf::st_crs(crs)$wkt
    return(terra::project(x, wkt, ...))
  } else if (inherits(x, c("bbox", "sfc", "sf"))) {
    return(sf::st_transform(x, crs, ...))
  } else {
    stop(sprintf("Cannot reproject object type: %s", class(x)))
  }
}

#' Spatially smooth dem by (window) filtering
#'
#' @param dem raster data of dem
#' @param method smoothing function to be used, e.g. "median".
#' As accepted by focal
#' @param window size of smoothing kernel
#'
#' @return filtered dem
#' @export
smooth_dem <- function(dem, method = "median", window = 5) {
  dem_smoothed <- terra::focal(dem, w = window, fun = method)
  names(dem_smoothed) <- "dem_smoothed"
  return(dem_smoothed)
}

#' Derive slope as percentage from dem
#' This makes use of the terrain function of the terra package
#'
#' @param dem raster data of dem
#'
#' @return raster of derived slope over dem extent
#' @export
get_slope <- function(dem) {
  slope_radians <- terra::terrain(dem, v = "slope", unit = "radians")
  slope <- tan(slope_radians)
  return(slope)
}

#' Mask slope raster, setting the slope to zero for the pixels overlapping
#' the river area.
#'
#' @param slope raster data of slope
#' @param river vector/polygon data of river
#' @param lthresh lower numerival threshold to consider slope non-zero
#' @param target value to set for pixels overlapping river area
#'
#' @return updated slope raster
#'
#' @export
mask_slope <- function(slope, river, lthresh = 1.e-3, target = 0) {
  slope_masked <- terra::mask(
                              slope,
                              terra::ifel(slope <= lthresh, NA, 1),
                              updatevalue = lthresh)

  slope_masked <- terra::mask(
                              slope_masked,
                              terra::vect(river),
                              inverse = TRUE,
                              updatevalue = target,
                              touches = TRUE)
}

#' Derive cost distance function from masked slope
#'
#' @param slope raster of slope data
#' @param river vector data of river
#' @param target value for cost distance calculation
#'
#' @return raster of cost distance
#' @export
get_cost_distance <- function(slope, river, target = 0) {
  slope_masked <- CRiSp::mask_slope(slope, river, target = target)
  cd <- terra::costDist(slope_masked, target = target)
  names(cd) <- "cost_distance"
  return(cd)
}

#' Mask out river regions incl. a buffer in cost distance raster data
#'
#' @param cd cost distance raster
#' @param river vector/polygon
#' @param buffer size of buffer around river polygon to additionally mask
#'
#' @return cd raster with river+BUFFER pixels masked
#' @export
mask_cost_distance <- function(cd, river, buffer = 2000) {
  river_buffer <- sf::st_buffer(river, buffer) |> terra::vect()
  cd_masked <- terra::mask(
    cd,
    river_buffer,
    updatevalue = NA,
    touches = TRUE
  )
  return(cd_masked)
}

#' Get characteristic value of distribution of cost distance
#'
#' @param cd cost distance raster data
#' @param method function used to derive caracteristic value (mean)
#'
#' @return characteristic value of cd raster
#' @export
get_cd_char <- function(cd, method = "mean") {
  if (method == "mean") {
    cd_char <- mean(terra::values(cd), na.rm = TRUE)
    return(cd_char)
  } else {
    #TODO
  }
}

#' Select valley pixels from cost distance based on threshold
#'
#' @param cd cost distance raster
#' @param thresh threshold cost distance value below which pixels are assuemd
#' to belong to the valley
#' @export
get_valley_mask <- function(cd, thresh) {
  valley_mask <- (cd < thresh)
  return(valley_mask)
}

#' Create vector/polygon representation of valley raster mask
#'
#' @param valley_mask raster mask of valley pixels
#'
#' @return polygon representation of valley area as st_geometry
#' @importFrom rlang .data
#' @export
get_valley_polygon_raw <- function(valley_mask) {
  valley_polygon <- terra::as.polygons(valley_mask, dissolve = TRUE) |>
    sf::st_as_sf() |>
    dplyr::filter(.data$cost_distance == 1) |>
    sf::st_geometry()
  return(valley_polygon)
}

#' Remove possible holes from valley geometry
#'
#' @param valley_polygon st_geometry of valley region
#'
#' @return (multi)polygon geometry of valley
#' @export
get_valley_polygon_no_hole <- function(valley_polygon) {
  valley_polygon_noholes <- valley_polygon |>
    sf::st_cast("POLYGON") |>
    lapply(function(x) x[1]) |>
    sf::st_multipolygon() |>
    sf::st_sfc(crs = sf::st_crs(valley_polygon))
  return(valley_polygon_noholes)
}

#' Create vector/polygon representation of valley without holes from raster mask
#'
#' @param valley_mask raster mask of valley pixels
#'
#' @return (multi)polygon representation of valley area
#' as st_geometry without holes
#' @export
get_valley_polygon <- function(valley_mask) {
  val_poly <- CRiSp::get_valley_polygon_raw(valley_mask) |>
    CRiSp::get_valley_polygon_no_hole()
  return(val_poly)
}
