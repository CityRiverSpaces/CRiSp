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
get_segments <- function(corridor, network, river_centerline,
                         angle_threshold = 90) {

  # Find river crossings in the network and build continuous strokes from them
  crossings <- get_intersecting_edges(network, river_centerline, index = TRUE)
  crossing_strokes <- rcoins::stroke(network, from_edge = crossings,
                                     angle_threshold = angle_threshold)

  # Clip strokes and select the ones that could be used as segment boundaries
  block_edges <- clip_and_filter(crossing_strokes, corridor, river_centerline)

  # Split the corridor into candidate segments ("blocks")
  blocks <- split(corridor, block_edges)

  # Refine the blocks to make sure that all segments touch the river and cross
  # the corridor from side to side
  refine_segments(blocks, river_centerline, corridor)
}

#' Clip lines to the extent of the corridor, and filter valid segment edges
#'
#' Lines that intersect the river only once and that cross the corridor from
#' side to side are considered valid segment edges. We group valid segment edges
#' that cross the river in nearby locations, and select the shortest line per
#' cluster.
#'
#' @param lines Candidate segment edges as a simple feature geometry
#' @param corridor The river corridor as a simple feature geometry
#' @param river_centerline The river centerline as a simple feature geometry
#'
#' @return Candidate segment edges as a simple feature geometry
#' @importFrom rlang .data
clip_and_filter <- function(lines, corridor, river_centerline) {

  # Split corridor along the river centerline to find edges on the two sides
  corridor_edges <- split(corridor, river_centerline, boundary = TRUE)

  # Clip the lines, keeping the only fragments that intersect the river
  lines_clipped <- sf::st_intersection(lines, corridor) |>
    sf::st_as_sf() |>
    dplyr::filter(sf::st_is(.data$x, c("MULTILINESTRING", "LINESTRING"))) |>
    sfheaders::sf_cast("LINESTRING") |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_geometry()

  # Select the fragments that cross the river only once and intersect both
  # sides of the corridor
  river_intersections <- sf::st_intersection(lines_clipped, river_centerline)
  # TODO: we could generalize the following to allow for more complex river
  # geometries (e.g. for river islands)
  intersects_river <- sf::st_is(river_intersections, "POINT")
  intersects_side_1 <- sf::st_intersects(lines_clipped, corridor_edges[[1]],
                                         sparse = FALSE)
  intersects_side_2 <- sf::st_intersects(lines_clipped, corridor_edges[[2]],
                                         sparse = FALSE)
  is_valid <- intersects_river & intersects_side_1 & intersects_side_2
  lines_valid <- lines_clipped[is_valid]

  # Cluster valid segment edges and select the shortest line per cluster
  filter_clusters(lines_valid, river_centerline)
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

#' Refine candidate segments via recursive merging
#'
#' Recursively merge the candidate segments provided ("blocks"), until they all
#' intersect the river centerline and both sides of the corridor.
#'
#' @param blocks Candidate segments as a simple feature geometry
#' @param river_centerline The river centerline as a simple feature geometry
#' @param corridor The river corridor as a simple feature geometry
#'
#' @return Refined corridor segments as a simple feature geometry
refine_segments <- function(blocks, river_centerline, corridor) {

  # Split corridor along the river centerline to find edges on the two sides
  corridor_edges <- split(corridor, river_centerline, boundary = TRUE)

  # Recursively merge blocks until all blocks intersect the river
  not_intersect_river <- function(blocks) {
    index_instersect_river <- find_intersects(blocks, river_centerline)
    index <- seq_along(blocks)
    index[!index %in% index_instersect_river]
  }
  while (TRUE) {
    index_not_instersects_river <- not_intersect_river(blocks = blocks)
    if (length(index_not_instersects_river) == 0) break
    blocks <- merge_blocks(blocks, index_not_instersects_river,
                           method = "longest-intersection")
  }

  # Recursively merge blocks until all blocks intersect both edges
  not_intersect_both_edges <- function(blocks) {
    index_intersects_edge_1 <- find_intersects(blocks, corridor_edges[1])
    index_intersects_edge_2 <- find_intersects(blocks, corridor_edges[2])
    index <- seq_along(blocks)
    index[!(index %in% index_intersects_edge_1 &
              index %in% index_intersects_edge_2)]
  }
  while (TRUE) {
    index_not_intersect_both_edges <- not_intersect_both_edges(blocks)
    if (length(index_not_intersect_both_edges) == 0) break
    blocks <- merge_blocks(blocks, index_not_intersect_both_edges,
                           method = "smallest")
  }
  return(blocks)
}

#' Merge a set of blocks to adjacent ones
#'
#' Adjacent blocks are defined as the blocks that are neighbours to the blocks
#' that need to be merged, and that intersect them via a (Multi)LineString. We
#' consider the blocks to merge one by one, from the smallest to the largest,
#' merging them to the other blocks recursively.
#'
#' @param blocks Simple feature geometry representing all the blocks
#' @param to_merge Indices of the blocks to merge
#' @param method Strategy for merging, see [merge_block()]
#'
#' @return Blocks merged to the specified ones as a simple feature geometry
merge_blocks <- function(blocks, to_merge, method = "longest-intersection") {
  if (length(to_merge) == 0) {
    return(blocks)
  }
  # Pick the first block to merge, i.e. the smallest one, and the targets
  index_smallest <- find_smallest(blocks[to_merge])
  index_current <- to_merge[index_smallest]
  current <-  blocks[index_current]
  targets <- blocks[!seq_along(blocks) %in% index_current]
  # Keep track of the geometries of the blocks that still need merging: their
  # position in the list of blocks might change after merging the current one!
  index_others <- to_merge[!seq_along(to_merge) %in% index_smallest]
  others <- blocks[index_others]
  # Merge current block with one of the targets
  merged <- merge_block(targets, current, method = method)
  # Determine the new indices of the other blocks that need to be merged
  is_equal <- sf::st_equals(merged, others, sparse = TRUE)
  index_others <- which(apply(is_equal, any, MARGIN = 1))
  # Continue merging other blocks, recursively
  merge_blocks(blocks = merged, to_merge = index_others, method = method)
}

#' Merge a block to one of the target geometries
#'
#' @param targets Sequence of target blocks as a simple feature geometry
#' @param block Block to merge as a simple feature geometry
#' @param method Strategy for merging, choose between "smallest" (merge to
#'   smallest adjacent block) and "longest-intersection" (merge to block which
#'   it shares the longest intersection with)
#'
#' @return Blocks merged to the specified one as a simple feature geometry
merge_block <- function(targets, block, method = "longest-intersection") {
  index_adjacent <- find_adjacent(targets, block)
  if (method == "longest-intersection") {
    intersections <- sf::st_intersection(targets[index_adjacent], block)
    index_longest_intersection <- find_longest(intersections)
    index_to_merge <- index_adjacent[index_longest_intersection]
  } else if (method == "smallest") {
    index_smallest <- find_smallest(targets[index_adjacent])
    index_to_merge <- index_adjacent[index_smallest]
  } else {
    stop(sprintf("Method '%s' unknown", method))
  }
  merged <- sf::st_union(targets[index_to_merge], block)
  others <- targets[!seq_along(targets) %in% index_to_merge]
  return(c(others, merged))
}
