#' Delineate a river corridor on a spatial network.
#'
#' The corridor edges on the two river banks are drawn on the provided spatial
#' network starting from an initial guess of the corridor (based e.g. on the
#' river valley).
#'
#' @param network The spatial network to be used for the delineation
#' @param river A (MULTI)LINESTRING simple feature geometry representing the
#'   river centerline
#' @param corridor_init How to estimate the initial guess of the river corridor.
#'   It can take the following values:
#'   * numeric or integer: use a buffer region of the given size (in meters)
#'     around the river centerline
#'   * An [`sf::sf`] or [`sf::sfc`] object: use the given input geometry
#' @param max_width (Approximate) maximum width of the corridor. The spatial
#'   network is trimmed by a buffer region of this size around the river
#' @param max_iterations Maximum number of iterations employed to refine the
#'   corridor edges (see [`corridor_edge()`]).
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#'
#' @return A simple feature geometry representing the river corridor
#' @export
#' @examplesIf interactive()
#' bucharest_osm <- get_osm_example_data()
#' network <- rbind(bucharest_osm$streets, bucharest_osm$railways) |>
#'   as_network()
#' delineate_corridor(network, bucharest_osm$river_centerline)
delineate_corridor <- function(
  network, river, corridor_init = 1000, max_width = 3000, max_iterations = 10,
  capping_method = "shortest-path"
) {
  # Drop all attributes of river but its geometry
  river <- sf::st_geometry(river)

  # If an initial corridor is not given, draw a buffer region around the river
  if (inherits(corridor_init, c("numeric", "integer"))) {
    corridor_init <- river_buffer(river, corridor_init)
  }

  # Build river network in the area covered by the spatial network
  river_network <- build_river_network(river, bbox = sf::st_bbox(network))

  # Define the spatial regions corresponding to the two river banks
  regions <- get_river_banks(river_network, max_width)

  # Determine the initial corridor edges by splitting the initial corridor
  # boundary through the just determined regions
  edges_init <- initial_edges(corridor_init, regions)

  # Also split the network over the two regions
  network_1 <- filter_network(network, regions[1])
  network_2 <- filter_network(network, regions[2])

  # Pick the corridor end points from all the intersection between the spatial
  # network and the river. The two furthest points that connect the sub-networks
  # on the two river sides are selected
  end_points <- corridor_end_points(river_network, network, regions)

  # Draw the edges on the spatial network
  edge_1 <- corridor_edge(network_1, end_points, edges_init[1], corridor_init,
                          max_iterations)
  edge_2 <- corridor_edge(network_2, end_points, edges_init[2], corridor_init,
                          max_iterations)

  # Cap the corritor
  cap_corridor(c(edge_1, edge_2), capping_method, network)
}

#' Build a spatial network from river centerlines
#'
#' If a bounding box is provided, only the river segments that intersect it are
#' considered. If the river intersects the bounding box multiple times, only the
#' longest intersecting segment will be considered.
#'
#' @param river A (MULTI)LINESTRING simple feature geometry representing the
#'   river centerline
#' @param bbox Bounding box of the area of interest
#'
#' @return A [`sfnetworks::sfnetwork`] object
#' @keywords internal
build_river_network <- function(river, bbox = NULL) {
  # Clip the river geometry using the bounding box (if provided)
  if (!is.null(bbox)) river <- sf::st_intersection(river, as_sfc(bbox))

  # The river might consist of a multilinestring, or clipping the river using
  # the area of interest might have lead to multiple segments - cast these into
  # linestring features
  river_segments <- sfheaders::sfc_cast(river, "LINESTRING")

  # Build a spatial network using the river segments and select the connected
  # component with overall longest edge length
  river_network <- as_network(river_segments, flatten = FALSE, clean = FALSE)
  components <- tidygraph::morph(river_network, tidygraph::to_components)
  sum_edge_lengths <- \(x) sum(sf::st_length(sf::st_as_sf(x, "edges")))
  edge_lengths <- vapply(components, sum_edge_lengths, numeric(1))
  river_network <- components[[which.max(edge_lengths)]]
  river_network <- clean_network(river_network, simplify = FALSE)  # keep loops
}

#' Find the corridor end points.
#'
#' Determine the extremes (end points) of the river corridor using the network
#' built from the river center line features (see [`build_river_network()`] and
#' the spatial network used for the delineation. The end points are selected as
#' the two furthest river crossings of the spatial network that connect the
#' sub-networks for each river sides.
#'
#' @param river_network A [`sfnetworks::sfnetwork`] object representing the
#'   river centerline
#' @param spatial_network A [`sfnetworks::sfnetwork`] object representing the
#'   spatial network used for the delineation
#' @param regions A simple feature geometry representing the two river sides
#'
#' @return A simple feature geometry including a pair of points
#' @keywords internal
corridor_end_points <- function(river_network, spatial_network, regions) {
  # Find intersections between the spatial network and the river network, after
  # splitting the former into the sub-networks for each river side
  network_1 <- filter_network(spatial_network, regions[1], elements = "edges")
  inters_reg_1 <- find_intersections(network_1, river_network)
  network_2 <- filter_network(spatial_network, regions[2], elements = "edges")
  inters_reg_2 <- find_intersections(network_2, river_network)
  # Identify common intersections between the two sub-networks
  intersections <- inters_reg_1[inters_reg_1 %in% inters_reg_2]
  # Make sure they are "POINTS" (no "MULTIPOINTS")
  intersections <- sfheaders::sfc_cast(intersections, "POINT")

  # Add the intersections as nodes to the river network. These nodes are the
  # candidates corridor end points
  river_network <- sfnetworks::st_network_blend(river_network, intersections)
  nodes <- nearest_node(river_network, intersections)

  # Compute the network distance between all the candidate end points. The ones
  # that are furthest away from each others are selected as corridor end points
  river_network <- add_weights(river_network, weight_name = "weight")
  distances <- sfnetworks::st_network_cost(river_network, from = nodes,
                                           to = nodes, weights = "weight")

  indices <- which(distances == max(distances), arr.ind = TRUE)[1, ]
  end_points <- c(nodes[indices["row"]], nodes[indices["col"]])
  if (end_points[1] == end_points[2]) {
    stop("Corridor start- and end-points coincide!")
  }
  end_points
}

#' Draw the regions corresponding to the two river banks
#'
#' These are constructed as single-sided buffers around the river geometry (see
#' [`river_buffer()`] for the implementation and refinement steps).
#'
#' @param river River spatial features provided as a [`sfnetworks::sfnetwork`]
#'   or [`sf::sf`]/[`sf::sfc`] object.
#' @param width Width of the regions
#' @return A [`sf::sfc`] object with two polygon features
#' @keywords internal
get_river_banks <- function(river, width) {
  if (inherits(river, "sfnetwork")) {
    river <- sf::st_as_sf(river, "edges")
  }
  river <- sf::st_geometry(river)

  # Simplify river features by merging consecutive segments. Linestrings are
  # also uniformly redirected, which should be important for the single-sided
  # buffering that is carried out next
  river_merged <- sf::st_cast(sf::st_union(river), "MULTILINESTRING")
  river_merged <- sf::st_line_merge(river_merged)
  river_segments <- sfheaders::sfc_cast(river_merged, "LINESTRING")

  # Single-sided buffers can have problems at discontinuities - build river
  # segments that are as long as possible on the basis of continuity
  continous_river_segments <- rcoins::stroke(river_segments)

  # Define the two river bank regions as single-sided buffers
  c(river_buffer(continous_river_segments, width, side = "right"),
    river_buffer(continous_river_segments, width, side = "left"))
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
#' @keywords internal
initial_edges <- function(corridor_initial, regions) {
  corridor_split <- sf::st_intersection(regions, corridor_initial)
  boundaries <- sf::st_union(sf::st_boundary(regions))
  sf::st_difference(sf::st_boundary(corridor_split), boundaries)
}

#' Draw a corridor edge on the spatial network.
#'
#' The corridor edge is drawn on the network as a shortest-path link between a
#' start- and an end-point. The weights in the shortest-path problem are set to
#' account for a) network edge lengths, b) distance from an initial target edge
#' geometry, and c) an excluded area where corridor edges are aimed not to go
#' through. The procedure is iterative, with the excluded area only being
#' accounted for in the first iteration. The identified corridor edge is
#' used as target edge in the following iteration, with the goal of prioritising
#' the "straightening" of the edge (some overlap with the excluded area is
#' allowed).
#'
#' @param network The spatial network used for the delineation
#' @param end_points Target start- and end-point
#' @param target_edge Target edge geometry to follow in the delineation
#' @param exclude_area Region that we aim to exclude from the delineation
#' @param max_iterations Maximum number of iterations employed to refine the
#'   corridor edges
#'
#' @return A simple feature geometry representing the edge (i.e. a linestring)
#' @keywords internal
corridor_edge <- function(network, end_points, target_edge, exclude_area = NULL,
                          max_iterations = 10) {
  # Identify nodes on the network that are closest to the target end points
  nodes <- nearest_node(network, end_points)

  # Iteratively refine
  converged <- FALSE
  niter <- 1
  area <- exclude_area
  while (!converged && niter <= max_iterations) {
    network <- add_weights(network, target_edge, area)
    edge <- shortest_path(network, from = nodes[1], to = nodes[2])
    converged <- edge == target_edge
    target_edge <- edge
    # The excluded area is only accounted for in the first iteration
    area <- NULL
    niter <- niter + 1
  }

  if (!converged) warning(sprintf(
    "River corridor edge not converged within %s iterations", max_iterations
  ))

  edge
}

#' Cap the corridor by connecting the edge end points
#'
#' @param edges A simple feature geometry representing the corridor edges
#' @param method The method employed for the capping:
#'   - `shortest-path` (default): find the network-based shortest-path
#'     connections between the edge end points.
#'   - `direct`: connect the start points and the end points of the
#'     edges via straight segments
#' @param network A spatial network object, only required if
#'   `method = 'shortest-path'`
#'
#' @return A simple feature geometry representing the corridor (i.e. a polygon)
#' @keywords internal
cap_corridor <- function(edges, method = "shortest-path", network = NULL) {

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
      sprintf("Unknown method to cap the river corridor: %s", method)
    )
  }
  polygon <- as_polygon(c(edges, cap_edge_1, cap_edge_2))

  # If the capping edges intersect the given corridor edges in points other
  # than the end points, the polygonization of the corridor boundary leads to
  # small side polygons. We drop these, after raising a warning
  if (length(polygon) > 1) {
    warning(
      "Corridor capping gives multiple polygons - selecting the largest one"
    )
    polygon <- polygon[find_largest(polygon)]
  }

  polygon
}
