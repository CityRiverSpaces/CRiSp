#' Split a river corridor into segments
#'
#' @param corridor The river corridor as a simple feature geometry
#' @param network The spatial network to be used for the segmentation
#' @param river_centerline The river centerline as a simple feature geometry
#' @param angle_threshold Only angles above this threshold will be included
#'
#' @return Segment polygons as a simple feature geometry
#' @export
segments <- function(corridor, network, river_centerline, angle_threshold) {

  crossings <- get_intersecting_edges(network, river_centerline)
  crossings_strokes <- strokes(network, crossings, angle_threshold)
  crossings_clustered <- cluster_crossings(crossings_strokes, river_centerline)
}


#' Identify network edges that are intersecting a geometry.
#'
#' @param network A spatial network object
#' @param river A simple feature geometry
#'
#' @return Indices of the edges intersecting the geometry as a vector
get_intersecting_edges <- function(network, geometry) {
  edges <- sf::st_as_sf(network, "edges")
  which(sf::st_intersects(edges, geometry, sparse = FALSE))
}

#' Group the river crossingns into clusters.
#'
#' Create groups of edges that are crossing the river in nearby locations,
#' using a density-based clustering method (DBSCAN). This is to make sure that
#' edges representing e.g. different lanes of the same street are treated as
#' part of the same crossing.
#'
#' @param crossings Crossing edge geometries as a simple feature object
#' @param river The river geometry as a simple feature object
#' @param eps DBSCAN parameter referring to the size (radius) distance of the
#'   neighborhood. Should approximate the distance between edges that we want
#'   to consider as a single river crossing
#'
#' @return A simple feature geometry where the `cluster` column labels crossings
#'   that are part of the same group
cluster_crossings <- function(crossings, river, eps = 100) {
  intersections <- sf::st_intersection(crossings, river)
  # By computing centroids we make sure we only have POINT geometries here
  intersections_centroids <- sf::st_centroid(intersections)
  intersections_coords <- sf::st_coordinates(intersections_centroids)
  # We should not enforce a min mumber of elements - one-element clusters are OK
  db <- dbscan::dbscan(intersections_coords, eps = eps, minPts = 1)

  crossings_clustered <- sf::st_as_sf(crossings)
  crossings_clustered$cluster <- db$cluster
  crossings_clustered
}
