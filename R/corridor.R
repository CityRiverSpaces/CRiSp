#' Delineate a river corridor on a spatial network.
#'
#' The corridor edges on the two river banks are drawn on the provided spatial
#' network starting from an initial definition of the corridor (based e.g. on
#' the river valley).
#'
#' @param network The spatial network to be used for the delineation
#' @param river_centerline A simple feature geometry representing the river
#'   centerline
#' @param river_surface A simple feature geometry representing the river surface
#' @param aoi Area of interest as sf object or bbox
#' @param max_width (Approximate) maximum width of the regions considered on the
#'   two river banks
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param buffer Buffer region to add to the river geometry to setup the initial
#'   corridor (only used if `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param max_iterations Maximum number of iterations employed to refine the
#'   corridor edges (see [`corridor_edge()`]).
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#'
#' @return A simple feature geometry representing the river corridor
#' @export
#' @examples
#' network <- rbind(bucharest_osm$streets, bucharest_osm$railways) |>
#'   as_network()
#' crs <- get_utm_zone(bucharest_osm$aoi)
#' aoi <- reproject(bucharest_osm$aoi, crs)
#' delineate_corridor(network,
#'                    bucharest_osm$river_centerline,
#'                    bucharest_osm$river_surface,
#'                    aoi,
#'                    dem = terra::unwrap(bucharest_dem))
delineate_corridor <- function(
  network, river_centerline, river_surface, aoi = NULL, max_width = 2500,
  initial_method = "valley", buffer = NULL, dem = NULL, max_iterations = 10,
  capping_method = "direct"
) {
  # Drop all attributes of river centerline and surface but the geometries
  river_centerline <- sf::st_geometry(river_centerline)
  river_surface <- sf::st_geometry(river_surface)

  # Draw the initial corridor geometry within the area of interest
  river <- c(river_centerline, river_surface)
  corridor_init <- initial_corridor(river, method = initial_method,
                                    buffer = buffer, dem = dem,
                                    bbox = as_bbox(aoi))

  # Build river network in the defined area of interest
  river_network <- build_river_network(river_centerline, aoi = aoi)

  # Pick the corridor end points as the two furthest crossings between the
  # spatial network and the river
  end_points <- corridor_end_points(river_network, network)

  # Define the spatial regions corresponding to the two river banks
  regions <- get_river_banks(river_network, max_width)

  # Determine the initial corridor edges by splitting the initial corridor
  # boundary through the just determined regions
  edges_init <- initial_edges(corridor_init, regions)

  # Also split the network over the two regions
  network_1 <- filter_network(network, regions[1])
  network_2 <- filter_network(network, regions[2])

  # Draw the edges on the spatial network
  edge_1 <- corridor_edge(network_1, end_points, edges_init[1], corridor_init,
                          max_iterations)
  edge_2 <- corridor_edge(network_2, end_points, edges_init[2], corridor_init,
                          max_iterations)

  # Cap the corritor
  cap_corridor(c(edge_1, edge_2), capping_method, network)
}

#' Draw the initial geometry of a river corridor.
#'
#' @param river A simple feature geometry representing the river
#' @param method The method employed to draw the initial river corridor:
#'   - "buffer": add a fixed buffer region to the river geometry (see
#'     [river_buffer()])
#'   - "valley" (default): use the river valley boundary, as estimated from the
#'     provided digital elevation model (DEM, see [get_valley()] for details on
#'     the implementation)
#' @param buffer Buffer region to add to the river geometry (only used if
#'   `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry
#' @keywords internal
initial_corridor <- function(
  river, method = "valley", buffer = NULL, dem = NULL, bbox = NULL
) {
  if (method == "buffer") {
    if (is.null(buffer)) {
      stop("Buffer should be provided if `method` is `'buffer'`")
    }
    river_buffer(river, buffer, bbox = bbox)
  } else if (method == "valley") {
    if (is.null(dem)) {
      stop("DEM should be provided if `method` is `'valley'`")
    }
    valley <- get_valley(dem, river)
    if (!is.null(bbox)) valley <- sf::st_crop(valley, bbox)
    valley
  } else {
    stop(
      sprintf("Unknown method to initialize river corridor: %s", method)
    )
  }
}

#' Build a spatial network from river centerlines
#'
#' If an area of interest (aoi) is provided, only the river segments that
#' intersects it are considered. If the river intersects the area of interest
#' multiple times, only the longest intersecting segment will be considered.
#'
#' @param river A simple feature geometry representing the river centerline
#' @param aoi Area of interest, provided as a bounding box or as a polygon.
#'
#' @return A [`sfnetworks::sfnetwork`] object
#' @keywords internal
build_river_network <- function(river, aoi = NULL) {
  # Clip the river geometry using the area of interest (if provided)
  if (!is.null(aoi)) {
    aoi <- as_sfc(aoi)
    river <- sf::st_intersection(river, aoi)
  }

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
#' the two furthest river crossings of the spatial network.
#'
#' @param river_network A [`sfnetworks::sfnetwork`] object representing the
#'   river centerline
#' @param spatial_network A [`sfnetworks::sfnetwork`] object representing the
#'   spatial network used for the delineation
#'
#' @return A simple feature geometry including a pair of points
corridor_end_points <- function(river_network, spatial_network) {
  # Only consider the edges of the spatial network
  spat_network_edges <- sf::st_geometry(sf::st_as_sf(spatial_network, "edges"))

  # Find intersections with the spatial network, and add these points as nodes
  # to the river network. These nodes are the candidates corridor end points
  river_network_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  intersections <- sf::st_intersection(river_network_edges, spat_network_edges)
  # Make sure they are "POINTS" (no "MULTIPOINTS")
  intersections <- sfheaders::sfc_cast(intersections, "POINT")
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

  # Define the two river bank regions as single-sided buffers
  c(river_buffer(river_segments, width, side = "right"),
    river_buffer(river_segments, width, side = "left"))
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
#'   - `direct` (default): connect the start points and the end points of the
#'     edges via straight segments
#'   - `shortest-path`: find the network-based shortest-path connections
#'     between the edge end points.
#' @param network A spatial network object, only required if
#'   `method = 'shortest-path'`
#'
#' @return A simple feature geometry representing the corridor (i.e. a polygon)
#' @keywords internal
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
