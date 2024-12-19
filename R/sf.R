#' @noRd
as_linestring <- function(points) {
  points_union <- sf::st_union(points)
  sf::st_cast(points_union, "LINESTRING")
}

#' @noRd
as_polygon <- function(lines) {
  lines_union <- sf::st_union(lines)
  sf::st_line_merge(lines_union) |>
    sf::st_polygonize() |>
    sf::st_collection_extract()
}

#' @noRd
find_smallest <- function(geometry) {
  area <- sf::st_area(geometry)
  return(which.min(area))
}

#' @noRd
find_adjacent <- function(geometry, target) {
  index_neighbour <- find_intersects(geometry, target)
  intersections <- sf::st_intersection(geometry[index_neighbour], target)
  is_adjacent_intersections <- sf::st_is(intersections,
                                         c("MULTILINESTRING", "LINESTRING"))
  return(index_neighbour[is_adjacent_intersections])
}

#' @noRd
find_longest <- function(geometry) {
  length <- sf::st_length(geometry)
  return(which.max(length))
}

#' @noRd
find_intersects <- function(geometry, target) {
  instersects <- sf::st_intersects(geometry, target, sparse = FALSE)
  return(which(instersects))
}

#' Split a geometry along a (multi)linestring.
#'
#' @param geometry Geometry to split
#' @param line Dividing (multi)linestring
#' @param boundary Whether to return the split boundary instead of the regions
#'
#' @return A simple feature object
split <- function(geometry, line, boundary = FALSE) {
  regions <- lwgeom::st_split(geometry, line) |>
    sf::st_collection_extract()
  if (!boundary) {
    return(regions)
  } else {
    boundaries <- sf::st_boundary(regions)
    split_boundary <- sf::st_difference(boundaries, line)
    return(split_boundary)
  }
}
