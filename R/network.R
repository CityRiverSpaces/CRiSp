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
  return(network)
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
  # Determine intersection points between crossing edges
  edges_cross <- get_crossing_edges(network)
  pts_intersect <- get_intersection_points(edges_cross)

  # Convert edge table to data.frame and add info on boundary points
  edge_pts <- sfheaders::sf_to_df(edges_cross)
  edge_idxs <- edge_pts$linestring_id
  edge_pts$is_startpoint <- !duplicated(edge_idxs)
  edge_pts$is_endpoint <- !duplicated(edge_idxs, fromLast = TRUE)

  # Loop over all points, add them to the edge table
  for (i in seq_len(nrow(pts_intersect))) {
    point <- pts_intersect$geometry[[i]]
    intersecting_edges <- unique(unlist(pts_intersect$origins[i]))
    for (edge_id in intersecting_edges) {
      edge_pts <- insert_intersection(edge_pts, point, edge_id)
    }
  }

  # Convert back edge table to sfc object
  edges_cross_new <- sfheaders::sfc_linestring(edge_pts, linestring_id = "id",
                                               x = "x", y = "y")
  sf::st_crs(edges_cross_new) <- sf::st_crs(edges_cross)

  # Update the network with the new edge geometries
  nodes <- network |> sf::st_as_sf("nodes")
  edges <- network |> sf::st_as_sf("edges")
  edges[edges_cross$id, ] <- edges[edges_cross$id, ] |>
    sf::st_set_geometry(edges_cross_new)
  network_new <- sfnetworks::sfnetwork(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    force = TRUE,  # skip checks
  )
  network_new
}

get_crossing_edges <- function(network) {
  network |>
    tidygraph::activate("edges") |>
    # Add ID to ease replacement later on
    dplyr::mutate(id = seq_len(dplyr::n())) |>
    dplyr::filter(sfnetworks::edge_crosses(tidygraph::.E())) |>
    sf::st_as_sf("edges")
}

get_intersection_points <- function(edges) {
  pts_intersect <- edges |>
    sf::st_intersection() |>
    # Cast multipoint intersections into points
    sf::st_collection_extract("POINT") |>
    sfheaders::sf_cast(to = "POINT")

  pts_intersect_agg <- aggregate(
    pts_intersect,
    by = sf::st_geometry(pts_intersect),
    FUN = unique,
    drop = TRUE
  )

  pts_intersect_unique <- pts_intersect_agg |> dplyr::distinct()
  pts_intersect_unique
}

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}

insert_intersection <- function(edge_pts, point, line_id) {
  line_pts <- subset(edge_pts, edge_pts$linestring_id == line_id)
  pt_x <- point[[1]]
  pt_y <- point[[2]]
  is_point_in_line <- nrow(
    subset(line_pts, line_pts$x == pt_x & line_pts$y == pt_y)
  ) >= 1
  if (!is_point_in_line) {
    startpoint <- subset(line_pts, line_pts$is_startpoint == TRUE)
    kk <- as.numeric(rownames(startpoint))
    w_break <- FALSE
    while (!w_break) {
      # Consider the line segments a - b.
      # x is a valid intersection if the following condition is true:
      # distance(a, b) == distance(a, x) + distance(x, b)  # nolint
      pt_a_x <- edge_pts[kk, ]$x
      pt_a_y <- edge_pts[kk, ]$y
      pt_b_x <- edge_pts[kk + 1, ]$x
      pt_b_y <- edge_pts[kk + 1, ]$y
      d_ab <- distance(pt_a_x, pt_a_y, pt_b_x, pt_b_y)
      d_ax <- distance(pt_a_x, pt_a_y, pt_x, pt_y)
      d_bx <- distance(pt_b_x, pt_b_y, pt_x, pt_y)
      is_intersection <- dplyr::near(d_ab, d_ax + d_bx, tol = 1.e-3)
      if (is_intersection) {
        insertion <- tibble::tibble_row(
          sfg_id = line_id,
          linestring_id = line_id,
          x = pt_x,
          y = pt_y,
          is_startpoint = FALSE,
          is_endpoint = FALSE
        )
        edge_pts <- tibble::add_row(edge_pts, insertion, .after = kk)
        w_break <- TRUE
      } else {
        if (edge_pts[kk + 1, ]$is_endpoint) {
          warning("point is not added to the edge df.")
          w_break <- TRUE
        }
      }
      kk <- kk + 1
    }
  }
  edge_pts
}

#' Clean a spatial network.
#'
# nolint start
#' Subdivide edges by [adding missing nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#subdivide-edges),
#' simplify the network (see [`simplify_network()`]), remove
#' [pseudo-nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#smooth-pseudo-nodes),
#' and discard all but the main connected component.
# nolint end
#'
#' @param network A network object
#'
#' @return A cleaned network object
#' @export
clean_network <- function(network) {
  network |>
    # subdivide edges by adding missing nodes
    tidygraph::convert(sfnetworks::to_spatial_subdivision, .clean = TRUE) |>
    # run simplification steps
    simplify_network() |>
    # remove pseudo-nodes
    tidygraph::convert(sfnetworks::to_spatial_smooth, .clean = TRUE) |>
    # keep only the main connected component of the network
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
  return(path)
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
#' @param network A network object
#' @param target The target geometry
#'
#' @return A spatial network object
filter_network <- function(network, target) {
  network |>
    tidygraph::activate("nodes") |>
    tidygraph::filter(sfnetworks::node_intersects(target))
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
    return(which(intersects))
  } else {
    return(edges[intersects, ])
  }
}
