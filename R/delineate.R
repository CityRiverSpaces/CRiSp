#' Delineate a corridor around a river.
#'
#' @param city_name A place name as a string
#' @param river_name A river name as a string
#' @param crs The projected Coordinate Reference System (CRS) to use. If not
#'   provided, the suitable Universal Transverse Mercator (UTM) CRS is selected
#' @param bbox_buffer Add a buffer region to the city boundary to avoid edge
#'   effects close to its limits
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param initial_buffer Buffer region to add to the river geometry to setup the
#'   initial corridor (only used if `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param max_iterations Maximum number of iterations employed to refine the
#'   corridor edges (see [`corridor_edge()`]).
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#' @param angle_threshold Only network edges forming angles above this threshold
#'   (in degrees) are considered when forming segment edges. See
#'  [get_segments()]  and [rcoins::stroke()]. Only used if `segments` is TRUE.
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#' @param force_download Download data even if cached data is available
#' @param ... Additional (optional) input arguments for retrieving the DEM
#'   dataset (see [get_dem()]). Only relevant if `initial_method` is `"valley"`
#'   and `dem` is NULL
#'
#' @return A simple feature geometry
#' @export
delineate_corridor <- function(
  city_name, river_name, crs = NULL, bbox_buffer = NULL,
  initial_method = "valley", initial_buffer = NULL, dem = NULL,
  max_iterations = 10, capping_method = "direct", angle_threshold = 90,
  segments = FALSE, riverspace = FALSE, force_download = FALSE, ...
) {
  # Define the area of interest and (if not provided) the CRS
  bbox <- get_osm_bb(city_name)
  if (!is.null(bbox_buffer)) bbox <- buffer_bbox(bbox, buffer = bbox_buffer)
  if (is.null(crs)) crs <- get_utm_zone(bbox)

  # Retrieve all relevant OSM datasets within the area of interest
  osm_data <- get_osmdata(bbox, city_name, river_name, crs = crs,
                          force_download = force_download)

  # If using the valley method, and the DEM is not provided, retrieve dataset
  if (initial_method == "valley" && is.null(dem)) {
    dem <- get_dem(bbox, crs = crs, force_download = force_download, ...)
  }

  # Set up the combined street and rail network for the delineation
  network_edges <- dplyr::bind_rows(osm_data$streets, osm_data$railways)
  network <- as_network(network_edges)

  # Run the corridor delineation on the spatial network
  bbox_repr <- reproject(bbox, crs)
  corridor <- get_corridor(
    network, osm_data$river_centerline, osm_data$river_surface, bbox_repr,
    initial_method = initial_method, buffer = initial_buffer, dem = dem,
    max_iterations = max_iterations, capping_method = capping_method
  )

  if (segments) {
    # Select the relevant part of the network
    buffer_corridor <- 100  # TODO should this be an additional input parameter?
    corridor_buffer <- sf::st_buffer(corridor, buffer_corridor)
    network_filtered <- filter_network(network, corridor_buffer)

    segments <- get_segments(corridor, network_filtered,
                             osm_data$river_centerline, angle_threshold)
  } else {
    segments <- NULL
  }

  if (riverspace) {
    riverspace <- delineate_riverspace(osm_data$buildings,
                                       osm_data$river_surface)
  } else {
    riverspace <- NULL
  }

  list(corridor, segments, riverspace)
}

#' Delineate the space surrounding a river
#'
#' @param occluders Geometry of occluders
#' @param river List with river surface and centerline
#' @param rayno Number of rays
#' @param raylen Length of rays
#'
#' @return Polygon geometry with the riverspace
#' @export
#'
#' @examples
#' \dontrun{
#'   delineate_riverspace(bucharest_osm$buildings, bucharest_osm$river_surface)
#' }
delineate_riverspace <- function(occluders, river, density = 1 / 50,
                                 rayno = 41, raylen = 100) {
  vpoints <- visor::get_viewpoints(river, density = density)
  isovists <- vector(mode = "list", length = length(vpoints))
  for (i in seq_along(vpoints)) {
    isovists[i] <- visor::get_isovist(occluders, vpoints[i], rayno, raylen)
  }
  sf::st_union(do.call(c, lapply(isovists, sf::st_sfc))) |>
    # Drop inner polygons from delineated riverspace
    sf::st_cast("LINESTRING") |>
    # The first item is the outer boundary
    dplyr::first() |>
    sf::st_cast("POLYGON")
}
