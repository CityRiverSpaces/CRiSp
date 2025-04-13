#' Split a river corridor into segments
#'
#' Segments are defined as corridor subregions separated by river-crossing
#' transversal lines that form continuous strokes in the network.
#'
#' @param corridor The river corridor as a simple feature geometry
#' @param network The spatial network to be used for the segmentation
#' @param river_centerline The river centerline as a simple feature geometry
#' @param angle_threshold Only consider angles above this threshold (in degrees)
#'   to form continuous strokes in the network. See [`rcoins::stroke()`] for
#'   more details.
#'
#' @return Segment polygons as a simple feature geometry
#' @export
#' @examples
#' if (!requireNamespace("CRiSpData", quietly = TRUE)) {
#'   message("Install CRiSpData from GitHub to run this example.")
#' } else {
#'   corridor <- bucharest_delineation$corridor
#'   network <- rbind(CRiSpData::bucharest_osm$streets,
#'                    CRiSpData::bucharest_osm$railways) |>
#'     as_network()
#'   river_centerline <- CRiSpData::bucharest_osm$river_centerline
#'   delineate_segments(corridor, network, river_centerline)
#' }
delineate_segments <- function(corridor, network, river_centerline,
                               angle_threshold = 90) {

  # Find river crossings in the network and build continuous strokes from them
  crossings <- get_intersecting_edges(network, river_centerline, index = TRUE)
  crossing_strokes <- rcoins::stroke(network, from_edge = crossings,
                                     angle_threshold = angle_threshold)

  # Clip strokes to the corridor extend and select non-intersecting strokes as
  # segment boundaries
  segment_edges <- clip_and_filter(crossing_strokes, corridor, river_centerline)

  # Split the corridor into the segments using the selected boundaries
  split_by(corridor, segment_edges)
}

#' Clip lines to the extent of the corridor, and select valid segment edges
#'
#' Lines that intersect the river and that cross the corridor from side to side
#' are considered valid segment edges. We group valid segment edges that cross
#' the river in nearby locations, and select the shortest line per cluster.
#' From these candidate segment edges, we select the ultimate set of
#' non-intersecting lines by dropping the longest segments with most
#' intersections.
#'
#' @param lines Candidate segment edges as a simple feature geometry
#' @param corridor The river corridor as a simple feature geometry
#' @param river_centerline The river centerline as a simple feature geometry
#'
#' @return Candidate segment edges as a simple feature geometry
#' @importFrom rlang .data
#' @keywords internal
clip_and_filter <- function(lines, corridor, river_centerline) {

  # Split corridor along the river centerline to find edges on the two sides
  corridor_edges <- get_corridor_edges(corridor, river_centerline)

  # Clip the lines, keeping the only fragments that intersect the river
  lines_clipped <- sf::st_intersection(lines, corridor) |>
    sf::st_as_sf() |>
    dplyr::filter(sf::st_is(.data$x, c("MULTILINESTRING", "LINESTRING"))) |>
    sfheaders::sf_cast("LINESTRING") |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_geometry()

  # Select the fragments intersecting both sides of the corridor
  intersects_side_1 <- sf::st_intersects(lines_clipped, corridor_edges[[1]],
                                         sparse = FALSE)
  intersects_side_2 <- sf::st_intersects(lines_clipped, corridor_edges[[2]],
                                         sparse = FALSE)
  lines_valid <- lines_clipped[intersects_side_1 & intersects_side_2]

  # Cluster valid segment edges and select the shortest line per cluster
  lines_shortest <- filter_clusters(lines_valid, river_centerline)

  # Drop intersecting lines, starting from the longest line with most
  # intersections with other lines
  select_nonintersecting_lines(lines_shortest, corridor)
}

#' Split corridor along the river to find edges on the two banks
#'
#' @param corridor The river corridor as a simple feature geometry
#' @param river_centerline The river centerline as a simple feature geometry
#'
#' @return Corridor edges as a simple feature geometry
#' @keywords internal
get_corridor_edges <- function(corridor, river_centerline) {
  corridor_edges <- split_by(corridor, river_centerline, boundary = TRUE)
  # For complex river geometries, splitting the corridor might actually return
  # multiple linestrings - select here the two longest segments
  if (length(corridor_edges) < 2) stop("Cannot identify corridor edges")
  corridor_edges[find_longest(corridor_edges, n = 2)]
}

#' Cluster the river crossings and select the shortest crossing per cluster
#'
#' Create groups of edges that are crossing the river in nearby locations,
#' using a density-based clustering method (DBSCAN). This is to make sure that
#' edges representing e.g. different lanes of the same street are treated as
#' part of the same crossing. For each cluster, select the shortest edge.
#'
#' @param crossings Crossing edge geometries as a simple feature object
#' @param river The river geometry as a simple feature object
#' @param eps DBSCAN parameter referring to the size (radius) distance of the
#'   neighborhood. Should approximate the distance between edges that we want
#'   to consider as a single river crossing
#'
#' @return A simple feature geometry including the shortest edge per cluster
#' @keywords internal
filter_clusters <- function(crossings, river, eps = 100) {
  intersections <- sf::st_intersection(crossings, river)
  # By computing centroids we make sure we only have POINT geometries here
  intersections_centroids <- sf::st_centroid(intersections)
  intersections_coords <- sf::st_coordinates(intersections_centroids)
  # We should not enforce a min mumber of elements - one-element clusters are OK
  db <- dbscan::dbscan(intersections_coords, eps = eps, minPts = 1)

  crossings_clustered <- sf::st_as_sf(crossings)
  crossings_clustered$cluster <- db$cluster
  crossings_clustered$length <- sf::st_length(crossings_clustered)
  crossings_clustered |>
    dplyr::group_by(.data$cluster) |>
    dplyr::filter(length == min(length) & !duplicated(length)) |>
    sf::st_geometry()
}

#' Select non-intersecting line segments
#'
#' Recursively drop intersecting lines, starting from the line that form most
#' intersections with other geometries. When multilple lines form the same
#' number of intersections with other geometries, the longest line is discarded
#' first. Note that lines are allowed to intersect on the corridor boundary.
#'
#' @param lines Candidate edge segment as a simple feature geometry
#' @param corridor The river corridor as a simple feature geometry
#' @return A set of lines that do not intersect within the corridor geometry,
#'   as a simple feature geometry
#' @keywords internal
select_nonintersecting_lines <- function(lines, corridor) {
  # Determine intersections among the lines provided
  intersections <- sf::st_intersection(sf::st_as_sf(lines))
  # Drop self-intersections
  intersections <- intersections[intersections[["n.overlaps"]] > 1, ]
  # Drop intersections lying on the corridor boundary
  idx <- unlist(sf::st_contains(sf::st_boundary(corridor), intersections))
  if (length(idx) > 0) intersections <- intersections[-idx, ]
  # If we are left with no intersections, return lines. If we still have
  # intersections, we recursively exclude lines until no intersection is found
  if (nrow(intersections) == 0) {
    lines
  } else {
    # Identify the line with maximum number of intersections
    intersecting_lines <- intersections[["origins"]]
    num_intersections <- sapply(seq_len(length(lines)),
                                \(x) sum(unlist(intersecting_lines) == x))
    max_intersections <- max(num_intersections)
    idx_line_max_intersections <- which(num_intersections == max_intersections)
    # Among these, find the longest one
    idx_line_longest <- find_longest(lines[idx_line_max_intersections])
    # This is the line that we are going to drop
    idx_line_to_drop <- idx_line_max_intersections[idx_line_longest]
    new_lines <- lines[-idx_line_to_drop]
    # Recursively check for intersections in the remaining lines
    select_nonintersecting_lines(new_lines, corridor)
  }
}
