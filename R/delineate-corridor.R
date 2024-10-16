#' Define an area of interest (AoI) from a bounding box and a
#' coordinate reference system (CRS).
#'
#' @param bb A bounding box as a matrix with 4 elements: xmin, ymin, xmax, ymax
#' @param crs A coordinate reference system as an epsg code, e.g. 4326 for WGS84
#' @param buffer_dist A numeric value to buffer the area of interest
#'
#' @return An area of interest as a simple feature geometry
#' @export
define_aoi <- function(bb, crs, buffer_dist = 0) {
  bbox <- as.vector(bb)
  names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  aoi <- sf::st_bbox(bbox, crs = sf::st_crs(4326)) |>
    sf::st_as_sfc() |>
    sf::st_transform(crs)

  if (buffer_dist != 0) {
    if (sf::st_is_longlat(aoi)) {
      warning("The area of interest is in longlat coordinates. ",
              "Consider using a projected CRS when buffering the AoI.")
    }
    aoi <- sf::st_buffer(aoi, buffer_dist)
  } else {
    aoi
  }
}

#' Split the area of interest (AoI) by a river.
#'
#' @param aoi Area of interest as a simple feature
#' @param river River as a simple feature
#'
#' @return A simple feature geometry set of two areas of interest
#' @export
split_aoi <- function(aoi, river) {
  aoi |>
    lwgeom::st_split(river) |>
    sf::st_collection_extract()
}

#' Trim a network to an area of interest (AoI) and a river corridor.
#'
#' @param net A network object
#' @param area An area of interest as a simple feature
#' @param river_corridor A river corridor as a simple feature
#'
#' @return A network object
#' @export
trim_network <- function(net, area, river_corridor) {
  net |>
    sfnetworks::activate("nodes") |>
    sf::st_filter(area, .predicate = sf::st_intersects) |>
    sf::st_filter(river_corridor, .predicate = CRiSp::not_intersects)
}

#' Simplify a street network by removing multiple edges and loops.
#'
#' @param net A network object
#'
#' @return A simplifed network object
#' @export
simplify_network <- function(net) {
  net |>
    sfnetworks::activate("edges") |>
    # TODO incorporate this comment in the function description
    # reorder the edges so that the shortest is kept
    dplyr::arrange(sfnetworks::edge_length()) |>
    dplyr::filter(!tidygraph::edge_is_multiple()) |>
    dplyr::filter(!tidygraph::edge_is_loop())
}

#' Clean a street network by subdividing edges and removing pseudo-nodes.
#'
#' @param net A network object
#'
#' @return A cleaned network object
#' @export
clean_network <- function(net) {
  net |>
    CRiSp::simplify_network() |>
    tidygraph::convert(sfnetworks::to_spatial_subdivision) |>
    tidygraph::convert(sfnetworks::to_spatial_smooth)
}

#' Determine the end vertices of the initial river corridor.
#'
#' Determine the "vertices" of the initial river corridor as the intersections
#' of the initial river corridor with the AoI boundary.
#'
#' @param aoi Area of interest as a simple feature
#' @param corridor_initial Initial river corridor as a simple feature
#'
#' @return A simple feature geometry set of two points
#' @export
get_vertices <- function(aoi, corridor_initial) {
  aoi |>
    sf::st_boundary() |>
    sf::st_intersection(corridor_initial) |>
    # this should consists of two linestring components, determine the endpoints
    sf::st_cast("POINT")
}

#' Get start and end points of corridor edge on the network.
#'
#' TODO add description
#'
#' @param vertices A simple feature geometry set of two points
#' @param area An area of interest as a simple feature
#' @param threshold A numeric value
#'
#' @return A simple feature geometry set of two points
#' @export
get_target_points <- function(vertices, area, threshold = 0.001) {
  vertices |>
    sf::st_as_sf() |>
    # TODO incorporate this comment into the function description
    # keep threshold to check which points intersect the polygons
    sf::st_filter(area,
                  .predicate = sf::st_is_within_distance,
                  dist = threshold) |>
    sf::st_geometry()
}

#' Determine the corridor edge on the network.
#'
#' Find the corridor edge on one side of the river by using a
#' shortest path algorithm.
#'
#' @param net A network object
#' @param area An area of interest as a simple feature
#' @param vertices A simple feature geometry set of two points
#'
#' @return A simple feature geometry
#' @export
get_corridor_edge <- function(net, area, vertices) {
  target_points <- CRiSp::get_target_points(vertices, area)

  paths <- sfnetworks::st_network_paths(
    net,
    from = target_points[1],
    to = target_points[2],
    weights = "weight",
    type = "shortest"
  )

  edges <- net |> sfnetworks::activate("edges") |> sf::st_geometry()
  edge_path <- paths |> dplyr::pull("edge_paths") |> unlist()
  edges[edge_path]
}

#' Cap corridor edges with a city boundary.
#'
#' @param corridor_edges Edge of the corridor as a simple feature
#' @param river River centerline as a simple feature
#' @param crs A coordinate reference system as an epsg code, e.g. 4326 for WGS84
#' @param bb A bounding box as a matrix with 4 elements: xmin, ymin, xmax, ymax
#' @param cap Character string with the type of cap to be used.
#' Default is "city"
#'
#' @return A simple feature geometry
#' @export
cap_corridor <- function(corridor_edges, river, crs, bb, cap = "city") {

  if (cap == "city") {
    cap <- CRiSp::osmdata_as_sf("place", "city", bb)
    cap <- cap$osm_multipolygons |>
      sf::st_geometry()
  } else {
    cap <- cap
  }
  cap <- cap |> sf::st_transform(crs)

  capped_corridor <- cap |>
    lwgeom::st_split(corridor_edges) |>
    sf::st_collection_extract("POLYGON") |>
    sf::st_as_sf() |>
    sf::st_filter(river, .predicate = sf::st_intersects)

  capped_corridor
}

#' Delineate a corridor around a river.
#'
#' @param place A place name as a string
#' @param river A river name as a string
#' @param crs A coordinate reference system as an epsg code, e.g. 4326 for WGS84
#'
#' @return A simple feature geometry
#' @export
delineate_corridor <- function(place, river, crs) {
  bb <- osmdata::getbb(place)

  highways_value <- c("motorway", "primary", "secondary", "tertiary")
  net <- CRiSp::osmdata_as_sf("highway", highways_value, bb) |>
    CRiSp::merge_streets() |>
    CRiSp::create_network()

  areas <- CRiSp::define_aoi(bb, crs) |> CRiSp::split_aoi(river)

  vertices <- CRiSp::get_vertices(areas, river)

  corridor_edge_1 <- CRiSp::get_corridor_edge(net, areas[1], vertices)
  corridor_edge_2 <- CRiSp::get_corridor_edge(net, areas[2], vertices)
  corridor_edges <- sf::st_union(corridor_edge_1, corridor_edge_2)

  CRiSp::cap_corridor(corridor_edges, river, crs)
}
