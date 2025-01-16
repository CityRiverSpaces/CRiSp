get_riverspace <- function(occluders, river, rayno = 41, raylen = 100) {
  vpoints <- visor::get_viewpoints(river)
  isovists <- lapply(vpoints, \(vpoint) {
    visor::get_isovist(occluders, vpoint, rayno, raylen)
  })
  sf::st_union(isovists)
}

#' Get viewpoints along a line
#'
#' @param x object of class sf, sfc or sfg
#' @param density number of points per distance unit
#'
#' @return object of class sfc_POINT
#' @export
get_viewpoints <- function(x, density = 1/50) {
  sf::st_line_sample(x, density = density) |> sf::st_cast("POINT")
}

#' Calculate isovist from a viewpoint
#'
#' @param occluders object of class sf, sfc or sfg
#' @param vpoint object of class sf, sfc or sfg
#' @param rayno number of rays
#' @param raylen length of rays
#'
#' @return object of class sfc_POLYGON
#' @export
#'
#' @examples
#' set.seed(32635)
#' vpoint <- sample(get_viewpoints(bucharest_osm$river_centerline |> sf::st_cast("LINESTRING")), 1)
#' get_isovist(occluders = bucharest_osm$buildings, vpoint = vpoint)
get_isovist <- function(occluders, vpoint, rayno = 41, raylen = 100) {
  visor::get_isovist(occluders, vpoint, rayno, raylen)
}
