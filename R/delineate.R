#' Delineate a corridor around a river.
#'
#' @param city_name A place name as a string
#' @param river_name A river name as a string
#' @param crs The projected Coordinate Reference System (CRS) to use. If not
#'   provided, the suitable Universal Transverse Mercator (UTM) CRS is selected
#' @param data_buffer Add a buffer (an integer in meters) around
#'   river centerline to retrieve additional data (streets, railways, etc.).
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param initial_buffer Buffer region to add to the river geometry to setup the
#'   initial corridor (only used if `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#' @param angle_threshold Only network edges forming angles above this threshold
#'   (in degrees) are considered when forming segment edges. See
#'  [get_segments()]  and [rcoins::stroke()]. Only used if `segments` is TRUE.
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#' @param ... Additional (optional) input arguments for retrieving the DEM
#'   dataset (see [get_dem()]). Only relevant if `initial_method` is `"valley"`
#'   and `dem` is NULL
#'
#' @return A simple feature geometry
#' @export
delineate_corridor <- function(
  city_name, river_name, crs = NULL, data_buffer = NULL,
  initial_method = "valley", initial_buffer = NULL, dem = NULL,
  capping_method = "direct", angle_threshold = 90, segments = FALSE,
  riverspace = FALSE, ...
) {

  # Retrieve all relevant OSM datasets within the data_buffer
  osm_data <- get_osmdata(city_name, river_name, data_buffer, crs)

  # Get the bounding box and (if not provided) the CRS
  bbox <- as_bbox(osm_data$aoi)
  if (is.null(crs)) crs <- get_utm_zone(osm_data$aoi)

  # If using the valley method, and the DEM is not provided, retrieve dataset
  if (initial_method == "valley" && is.null(dem)) {
    dem <- get_dem(bbox, crs = crs, ...)
  }

  # Set up the combined street and rail network for the delineation
  network_edges <- dplyr::bind_rows(osm_data$streets, osm_data$railways)
  network <- as_network(network_edges)

  # Run the corridor delineation on the spatial network
  bbox_repr <- reproject(bbox, crs)
  corridor <- get_corridor(
    network, osm_data$river_centerline, osm_data$river_surface, bbox_repr,
    initial_method = initial_method, buffer = initial_buffer, dem = dem,
    capping_method = capping_method
  )

  if (segments) {
    # Select the relevant part of the network
    buffer_corridor <- 100  # TODO should this be an additional input parameter?
    corridor_buffer <- sf::st_buffer(corridor, buffer_corridor)
    network_filtered <- filter_network(network, corridor_buffer)

    corridor <- get_segments(corridor, network_filtered,
                             osm_data$river_centerline, angle_threshold)
  }
  if (riverspace) delineate_riverspace()
  return(corridor)
}

#' Delinate the riverspace.
#'
#' @return A simple feature geometry
#' @export
delineate_riverspace <- function() {
  stop("Riverspace delineation not yet implemented.")
}
