#' Delineate the space surrounding a river
#'
#' @param occluders Geometry of occluders
#' @param river List with river surface and centerline
#' @param density Density of viewpoints
#' @param ray_num Number of rays
#' @param ray_length Length of rays in meters
#'
#' @return Polygon geometry with the riverspace
#' @export
#'
#' @examples
#' \dontrun{
#' delineate_riverspace(bucharest_osm$buildings, bucharest_osm$river_surface)
#' }
delineate_riverspace <- function(occluders, river, density = 1 / 50,
                                 ray_num = 41, ray_length = 100) {
  vpoints <- visor::get_viewpoints(river, density = density)
  isovists <- vector(mode = "list", length = length(vpoints))
  for (i in seq_along(vpoints)) {
    isovists[i] <-
      visor::get_isovist(occluders, vpoints[i], ray_num, ray_length)
  }
  riverspace <- sf::st_union(do.call(c, lapply(isovists, sf::st_sfc))) |>
    sfheaders::sf_remove_holes()
  sf::st_crs(riverspace) <- sf::st_crs(river)
  riverspace
}
