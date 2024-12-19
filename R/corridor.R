#' Delineate a river corridor on a spatial network.
#'
#' @param network The spatial network to be used for the delineation
#' @param river_centerline A simple feature geometry representing the river
#'   centerline
#' @param river_surface A simple feature geometry representing the river surface
#' @param bbox Bounding box defining the extent of the area of interest
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#' TODO: add input arguments for the initialization method
#'
#' @return A simple feature geometry representing the river corridor
#' @export
get_corridor <- function(
  network, river_centerline, river_surface, bbox, initial_method = "buffer",
  capping_method = "direct"
) {

  # Draw the initial corridor geometry within the area of interest
  river <- c(river_centerline, river_surface)
  # TODO: remove hardcoded buffer value here
  corridor_init <- initial_corridor(river, initial_method, bbox, buffer = 1000)

  # Find the corridor end points
  end_points <- corridor_end_points(river_centerline, bbox)

  # Split the area of interest along the river centerline
  regions <- split_aoi(bbox, river_centerline)

  # Determine the initial corridor edges by splitting the initial corridor
  # boundary through the just determined regions
  edges_init <- initial_edges(corridor_init, regions)

  # Run the delineation, setting the initial corridor geometry as excluded area
  network_1 <- filter_network(network, regions[1])
  edge_1 <- corridor_edge(network_1, end_points, edges_init[1], corridor_init)
  network_2 <- filter_network(network, regions[2])
  edge_2 <- corridor_edge(network_2, end_points, edges_init[2], corridor_init)

  # Run the delineation again, with the previously determined edges as targets.
  # This refinement step has the effect of "smoothening" the edges
  edge_1 <- corridor_edge(network_1, end_points, edge_1)
  edge_2 <- corridor_edge(network_2, end_points, edge_2)

  cap_corridor(c(edge_1, edge_2), capping_method, network)
}

#' Draw the initial geometry of a river corridor.
#'
#' @param river A simple feature geometry representing the river
#' @param method The method employed to draw the initial river corridor:
#'   - "buffer" (default): add a fixed buffer region to the river geometry
#'     (see [river_buffer()])
#' @param ... Additional arguments required by the function that implements the
#'   chosen method (see `method`)
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry
initial_corridor <- function(river, method = "buffer", ..., bbox = NULL) {
  args <- list(...)
  if (method == "buffer") {
    return(river_buffer(river, buffer = args$buffer, bbox = bbox))
  } else {
    stop(
      sprintf("Unknown method to initialize river corridor: {method}", method)
    )
  }
}

#' Draw a corridor as a fixed buffer region around a river.
#'
#' @param river A simple feature geometry representing the river
#' @param buffer Size of the buffer (in the river's CRS units)
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry
river_buffer <- function(river, buffer, bbox = NULL) {
  river_buf <- sf::st_buffer(river, buffer)
  river_buf_union <- sf::st_union(river_buf)
  if (!is.null(bbox)) {
    return(sf::st_crop(river_buf_union, bbox))
  } else {
    return(river_buf_union)
  }
}

#' Find the corridor end points.
#'
#' Using the river center line and the bounding box of the area of interest,
#' determine the extremes (start and end point) of the river corridor.
#'
#' @param river A simple feature geometry representing the river centerline
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry including a pair of points
corridor_end_points <- function(river, bbox) {
  aoi <- sf::st_as_sfc(bbox)
  aoi_boundary <- sf::st_boundary(aoi)
  intersections <- sf::st_intersection(river, aoi_boundary)
  sf::st_cast(intersections, "POINT")
}

#' Split the area of interest (AoI) by a river.
#'
#' Return the fragments produced. If more than two fragments are obtained, only
#' return the two largest fragments.
#'
#' @param bbox Bounding box defining the extent of the area of interest
#' @param river A simple feature geometry representing the river centerline
#'
#' @return A simple feature geometry set of two areas of interest
#' @importFrom rlang .data
split_aoi <- function(bbox, river) {
  regions <- split(sf::st_as_sfc(bbox), river)

  if (length(regions) > 2) {
    # Sort fragments according to area in descending order
    regions_sorted <- sf::st_as_sf(regions) |>
      dplyr::mutate(area = sf::st_area(regions)) |>
      dplyr::arrange(-.data$area)

    # Return the geometries of the two largest fragments
    return(sf::st_geometry(regions_sorted[1:2, ]))
  } else {
    return(regions)
  }
}

#' Identify the initial edges of the river corridor
#'
#' These are defined by splitting the initial corridor boundary into the
#' sub-regions that the river forms in the area of interest
#'
#' @param corridor_initial A simple feature geometry representing the area of
#'   the initial corridor
#' @param regions A simple feature geometry representing the sub-regions formed
#'   by cutting the area of interest along the river
#'
#' @return A simple feature geometry representing the initial corridor edges
initial_edges <- function(corridor_initial, regions) {
  corridor_split <- sf::st_intersection(regions, corridor_initial)
  boundaries <- sf::st_union(sf::st_boundary(regions))
  sf::st_difference(sf::st_boundary(corridor_split), boundaries)
}

#' Draw a corridor edge on the spatial network.
#'
#' @param network The spatial network used for the delineation
#' @param end_points Target start- and end-point
#' @param target_edge Target edge geometry to follow in the delineation
#' @param exclude_area Region that we aim to exclude from the delineation
#'
#' @return A simple feature geometry representing the edge (i.e. a linestring)
corridor_edge <- function(
  network, end_points, target_edge, exclude_area = NULL
) {
  nodes <- nearest_node(network, end_points)
  network <- add_weights(network, target_edge, exclude_area)
  shortest_path(network, from = nodes[1], to = nodes[2])
}

#' Cap the corridor by connecting the edge end points
#'
#' @param edges A simple feature geometry representing the corridor edges
#' @param method The method employed for the capping:
#'   - `direct` (default): connect the start points and the end points of the
#'     edges via straight segments
#'   - `shortest-path`: find the network-based shortest-path connections
#'     between the edge end points.
#' @param network A spatial network object, only required if
#'   `method = 'shortest-path'`
#'
#' @return A simple feature geometry representing the corridor (i.e. a polygon)
cap_corridor <- function(edges, method = "direct", network = NULL) {

  start_pts <- lwgeom::st_startpoint(edges)
  end_pts <- lwgeom::st_endpoint(edges)

  if (method == "direct") {
    cap_edge_1 <- as_linestring(start_pts)
    cap_edge_2 <- as_linestring(end_pts)
  } else if (method == "shortest-path") {
    if (is.null(network)) stop(
      "A network should be provided if `capping_method = 'shortest-path'`"
    )
    network <- add_weights(network)
    cap_edge_1 <- shortest_path(network, from = start_pts[1], to = start_pts[2])
    cap_edge_2 <- shortest_path(network, from = end_pts[1], to = end_pts[2])
    # TODO: raise warning if lenght is 2 times longer than direct segment
  } else {
    stop(
      sprintf("Unknown method to cap the river corridor: {method}", method)
    )
  }
  as_polygon(c(edges, cap_edge_1, cap_edge_2))
}
