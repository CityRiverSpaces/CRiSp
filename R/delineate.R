#' Delineate a corridor around a river.
#'
#' @param city_name A place name as a string
#' @param river_name A river name as a string
#' @param crs The projected Coordinate Reference System (CRS) to use. If not
#'   provided, the suitable Universal Transverse Mercator (UTM) CRS is selected
#' @param bbox_buffer Add a buffer region to the city boundary to avoid edge
#'   effects close to its limits
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#' @param angle_threshold Only network edges forming angles above this threshold
#'   (in degrees) are considered when forming segment edges. See
#'  [get_segments()]  and [rcoins::stroke()]. Only used if `segments` is TRUE.
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#'
#' @return A simple feature geometry
#' @export
delineate_corridor <- function(
  city_name, river_name, crs = NULL, bbox_buffer = NULL,
  initial_method = "buffer", capping_method = "direct", angle_threshold = 90,
  segments = FALSE, riverspace = FALSE
) {
  # Retrieve all relevant OSM datasets using the extended bounding box
  osm_data <- get_osmdata(city_name, river_name, crs, bbox_buffer)

  # Define the area of interest
  bbox <- sf::st_transform(osm_data$bb, sf::st_crs(osm_data$boundary))

  # Set up the combined street and rail network for the delineation
  network_edges <- dplyr::bind_rows(osm_data$streets, osm_data$railways)
  network <- as_network(network_edges)

  # Run the corridor delineation on the spatial network
  corridor <- get_corridor(
    network, osm_data$river_centerline, osm_data$river_surface, bbox,
    initial_method, capping_method
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
