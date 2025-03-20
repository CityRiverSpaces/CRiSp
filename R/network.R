#' Create a network from a collection of line strings.
#'
#' @param edges A data frame with the network edges
#' @param flatten Whether all intersections between edges should be
#'   converted to nodes
#' @param clean Whether general cleaning tasks should be run on the generated
#'   network (see [`clean_network()`] for the description of tasks)
#'
#' @return A spatial network object
#' @export
as_network <- function(edges, flatten = TRUE, clean = TRUE) {
  network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
  if (flatten) network <- flatten_network(network)
  if (clean) network <- clean_network(network)
  network
}

#' Flatten a network by adding points at apparent intersections.
#'
#' All crossing edges are identified, and the points of intersections are
#' injected within the edge geometries. Note that the injected points are
#' not converted to network nodes (this can be achieved via sfnetworks'
#' [`sfnetworks::to_spatial_subdivision()`], which is part of the tasks
#' that are included in [`clean_network()`].
#'
#' The functionality is similar to sfnetworks'
#' [`sfnetworks::st_network_blend()`], but in that case an external point is
#' only injected to the closest edge.
#'
#' @param network A network object
#'
#' @return A network object with additional points at intersections
#' @export
flatten_network <- function(network) {
  nodes <- sf::st_as_sf(network, "nodes")
  edges <- sf::st_as_sf(network, "edges")

  # Determine intersection points between crossing edges
  edges_cross <- get_crossing_edges(edges)
  if (nrow(edges_cross) == 0) return(network)
  points_intersect <- get_intersection_points(edges_cross)

  # Add target points to the edge geometries
  edges_new <- insert_intersections(edges_cross, points_intersect)

  # Update the network with the new edge geometries
  edges[edges_cross$id, ] <- sf::st_set_geometry(edges[edges_cross$id, ],
                                                 edges_new)
  network_new <- sfnetworks::sfnetwork(nodes = nodes,
                                       edges = edges,
                                       directed = FALSE,
                                       force = TRUE)  # skip checks
  network_new
}

get_crossing_edges <- function(edges) {
  geometry <- sf::st_geometry(edges)
  crossings <- sf::st_crosses(geometry)
  mask <- lengths(crossings) > 0
  sf::st_sf(id = which(mask), geometry = geometry[mask])
}

get_intersection_points <- function(edges) {
  # make sure edges is an sf object, so st_intersection also returns origins
  intersections <- sf::st_intersection(sf::st_sf(edges))
  # only consider (multi-)point intersections
  points <- sf::st_collection_extract(intersections, type = "POINT")
  # cast multipoint intersections to points
  sfheaders::sf_cast(points, to = "POINT")
}

insert_intersections <- function(edges, points, tol = 1.e-3) {

  edge_geometry <- sf::st_geometry(edges)
  edge_points <- sfheaders::sfc_to_df(edge_geometry)
  edge_ids <- edge_points$linestring_id
  edge_coords <- as.matrix(edge_points[c("x", "y")])

  # invert mapping from "intersection point" -> "edges" (list `origins` below)
  # to "edge" -> "intersection points" (`targets`). This way we can loop over
  # edges without having to get back to the same edge twice
  point_coords <- sf::st_coordinates(points)
  npoints <- nrow(points)
  origins <- points[["origins"]]
  targets <- split(rep(seq_len(npoints), lengths(origins)), unlist(origins))

  # create empty arrays where to store new edge coordinates and identifiers
  coords <- matrix(, nrow = 0, ncol = 2)
  linestring_ids <- c()

  for (edge_id in seq_along(edge_geometry)) {
    is_current_edge <- edge_ids == edge_id
    edge <- edge_coords[is_current_edge, ]
    for (point_id in targets[[edge_id]]) {
      point <- point_coords[point_id, ]
      if (is_point_in_edge(point, edge, tol = tol)) next
      # calculate distance from the target point to all edge points
      distances_from_target <- calc_distance(point, edge)
      # by computing the rolling sum over `distances` we get the summed
      # distances from the target to consecutive edge points
      summed_distances_from_target <- calc_rolling_sum(distances_from_target)
      # calculate length of line segments
      segment_lengths <- sqrt(apply(diff(edge)^2, sum, MARGIN = 1))
      # the point lies on the line segment for which the summed distances from
      # the endpoints is equal to the segment length
      index <- which(abs(summed_distances_from_target - segment_lengths) < tol)
      # update the coordinates by adding the target at the identified position
      edge <- rbind(head(edge, index),
                    point,
                    tail(edge, nrow(edge) - index))
    }
    coords <- rbind(coords, edge)
    linestring_ids <- c(linestring_ids, rep(edge_id, nrow(edge)))
  }

  edges_new <- sfheaders::sfc_linestring(
    data.frame(coords, linestring_id = linestring_ids),
    x = "x", y = "y", linestring_id = "linestring_id"
  )
  sf::st_crs(edges_new) <- sf::st_crs(edges)
  return(edges_new)
}

is_point_in_edge <- function(point, edge, tol) {
  any(calc_distance(point, edge) < tol)
}

calc_distance <- function(point, edge) {
  sqrt((edge[, "x"] - point["X"]) ^ 2 + (edge[, "y"] - point["Y"]) ^ 2)
}

calc_rolling_sum <- function(x, n = 2) {
  cs <- cumsum(x)
  # roll the cumsum array by adding `n` zeros at its beginning and dropping
  # the last `n` elements
  cs_roll <- head(c(rep(0, n), cs), length(x))
  # the rolling sum is the difference between cumsum and its roll, after
  # dropping the first element of the array
  tail(cs - cs_roll, length(x) - 1)
}

#' Clean a spatial network.
#'
# nolint start
#' Subdivide edges by [adding missing nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#subdivide-edges),
#' (optionally) simplify the network (see [`simplify_network()`]), remove
#' [pseudo-nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#smooth-pseudo-nodes),
#' and discard all but the main connected component.
# nolint end
#'
#' @param network A network object
#' @param simplify Whether the network should be simplified with
#'   [`simplify_network()`]
#'
#' @return A cleaned network object
#' @export
clean_network <- function(network, simplify = TRUE) {
  # subdivide edges by adding missing nodes
  net <- tidygraph::convert(network, sfnetworks::to_spatial_subdivision,
                            .clean = TRUE)
  # run simplification steps
  if (simplify) net <- simplify_network(net)
  # remove pseudo-nodes
  net <- tidygraph::convert(net, sfnetworks::to_spatial_smooth, .clean = TRUE)
  # keep only the main connected component of the network
  net |>
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::group_components() == 1)
}

#' Simplify a spatial network by removing multiple edges and loops.
#'
# nolint start
#' Simplify the graph, removing loops and double-edge connections following
#' [this approach](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#simplify-network).
#' When dropping multiple edges, keep the shortest ones.
# nolint end
#'
#' @param network A network object
#'
#' @return A simplifed network object
simplify_network <- function(network) {
  network |>
    sfnetworks::activate("edges") |>
    dplyr::arrange(sfnetworks::edge_length()) |>
    dplyr::filter(!tidygraph::edge_is_multiple()) |>
    dplyr::filter(!tidygraph::edge_is_loop())
}

#' Add weights to the network.
#'
#' This is to prepare the network for the search of shortest paths between node
#' pairs. The computed weights can account for edge lenghts, distance from a
#' target geometry, and whether or not an edge falls within a specified region,
#' which we aim to exclude from search of the shortest paths.
#'
#' For the i-th edge of the network, its weight \eqn{w_i} is defined in the
#' following way:
#' \deqn{
#'  w_i = |e_i| + d_{geom}(e_i) + p_{buf}(e_i)
#' }{wi = |ei| + d(geom, ei) + p(buffer, ei)}
#' where the first term is the edge length, the second one is the distance
#' from a target geometry (`target`, optional) and the last one is a penalty
#' that is added if the centroid of the edge falls within a specified region
#' (`exclude_area`, optional).
#'
#' Shortest paths calculated on the resulting network will thus tend to prefer
#' edges close to `target` and to avoid edges within `exclude_area`.
#'
#' @param network A network object
#' @param target Target geometry to calculate distances from, as a simple
#'   feature geometry
#' @param exclude_area Area that we aim to exclude from the shortest-path
#'   search, as a simple feature geometry
#' @param penalty Penalty (in the network CRS' units) that is added to the
#'   edges that falls within the excluded area
#' @param weight_name Name of the column in the edge table where to add the
#'   weights
#'
#' @return A network object with weights added as a column in the edge table
#' @importFrom rlang :=
#' @export
add_weights <- function(network, target = NULL, exclude_area = NULL,
                        penalty = 1000., weight_name = "weight") {
  edges <- sf::st_geometry(sf::st_as_sf(network, "edges"))
  lengths <- sf::st_length(edges)

  if (!is.null(target)) {
    distances <- sf::st_distance(edges, target, which = "Euclidean")
    distances <- drop(distances)  # drop column dimension with single element
  } else {
    distances <- 0.
  }
  distances <- set_units_like(distances, lengths)

  if (!is.null(exclude_area)) {
    is_inside <- edges |>
      sf::st_centroid() |>
      sf::st_intersects(exclude_area, sparse = FALSE) |>
      as.numeric()
    repellance <- penalty * is_inside
  } else {
    repellance <- 0.
  }
  repellance <- set_units_like(repellance, lengths)

  network |>
    tidygraph::activate("edges") |>
    dplyr::mutate(!!weight_name := lengths + distances + repellance)
}

#' Find shortest path between a pair of nodes in the network.
#'
#' @param network A spatial network object
#' @param from Start node
#' @param to End node
#' @param weights Name of the column in the network edge table from where to
#'   take the weigths
#'
#' @return A simple feature geometry
#' @importFrom rlang .data
shortest_path <- function(network, from, to, weights = "weight") {
  paths <- sfnetworks::st_network_paths(
    network, from = from, to = to, weights = weights, type = "shortest",
  )
  edges <- sf::st_as_sf(network, "edges") |> sf::st_geometry()
  edge_path <- dplyr::pull(paths, .data$edge_paths) |> unlist()
  path <- edges[edge_path]
  if (length(path) > 1) {
    # if the path consists of multiple edges, merge them in a linestring
    path <- sf::st_union(path) |> sf::st_line_merge()
    # make sure the path direction is correct, reverse it otherwise
    start_pt <- lwgeom::st_startpoint(path)
    if (sf::st_distance(start_pt, from) > sf::st_distance(start_pt, to)) {
      path <- sf::st_reverse(path)
    }
  }
  path
}

#' Find the node in a network that is closest to a target geometry.
#'
#' @param network A network object
#' @param target The target geometry
#'
#' @return A node in the network as a simple feature geometry
nearest_node <- function(network, target) {
  nodes <- sf::st_as_sf(network, "nodes") |>
    sf::st_geometry()
  idx <- sf::st_nearest_feature(target, nodes)
  nodes[idx]
}

#' Subset a network keeping the only nodes that intersect a target geometry.
#'
#' If subsetting results in multiple disconnected components, we keep the main
#' one.
#'
#' @param network A network object
#' @param target The target geometry
#'
#' @return A spatial network object
filter_network <- function(network, target) {
  network |>
    tidygraph::activate("nodes") |>
    tidygraph::filter(sfnetworks::node_intersects(target)) |>
    # keep only the main connected component of the network
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::group_components() == 1)
}

#' Identify network edges that are intersecting a geometry
#'
#' @param network A spatial network object
#' @param geometry A simple feature geometry
#' @param index Whether to return the indices of the matchin edges or the
#'   geometries
#'
#' @return Indices or geometries of the edges intersecting the given geometry
get_intersecting_edges <- function(network, geometry, index = FALSE) {
  edges <- sf::st_as_sf(network, "edges")
  intersects <- sf::st_intersects(edges, geometry, sparse = FALSE)
  if (index) {
    which(intersects)
  } else {
    edges[intersects, ]
  }
}
