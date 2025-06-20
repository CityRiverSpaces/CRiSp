#' Delineate a corridor around a river.
#'
#' @param city_name A place name as a string
#' @param river_name A river name as a string
#' @param crs The projected Coordinate Reference System (CRS) to use. If not
#'   provided, the suitable Universal Transverse Mercator (UTM) CRS is selected
#' @param network_buffer Add a buffer (an integer in meters) around
#'   river to retrieve additional data (streets, railways, etc.).
#'   Default is 3000 m.
#' @param buildings_buffer Add a buffer (an integer in meters) around the
#'   river to retrieve additional data (buildings). Default is 100 m.
#' @param corridor_init How to estimate the initial guess of the river corridor.
#'   It can take the following values:
#'   * "valley": use the river valley boundary, as estimated from a Digital
#'     Elevation Model (DEM) (for more info see [delineate_valley()])
#'   * numeric or integer: use a buffer region of the given size (in meters)
#'     around the river centerline
#'   * An [`sf::sf`] or [`sf::sfc`] object: use the given input geometry
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `corridor_init` is `"valley"`)
#' @param dem_buffer Size of the buffer region (in meters) around the
#'   river to retrieve the DEM  (only used if `corridor_init` is `"valley"` and
#'   `dem` is NULL).
#' @param max_iterations Maximum number of iterations employed to refine the
#'   corridor edges (see [`corridor_edge()`]).
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#' @param angle_threshold Only network edges forming angles above this threshold
#'   (in degrees) are considered when forming segment edges. See
#'  [delineate_segments()] and [rcoins::stroke()]. Only used if `segments` is
#'  TRUE.
#' @param corridor Whether to carry out the corridor delineation
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#' @param force_download Download data even if cached data is available
#' @param ... Additional (optional) input arguments for retrieving the DEM
#'   dataset (see [get_dem()]). Only relevant if `corridor_init` is `"valley"`
#'   and `dem` is NULL
#'
#' @return A list with the corridor, segments, and riverspace geometries
#' @export
#' @examplesIf interactive()
#' delineate("Bucharest", "Dâmbovița")
delineate <- function(
  city_name, river_name, crs = NULL, network_buffer = NULL,
  buildings_buffer = NULL, corridor_init = "valley", dem = NULL,
  dem_buffer = 2500, max_iterations = 10, capping_method = "shortest-path",
  angle_threshold = 100, corridor = TRUE, segments = FALSE,
  riverspace = FALSE, force_download = FALSE, ...
) {

  delineations <- list()

  if (segments && !corridor) stop("Segmentation requires corridor delineation.")

  # set default values for network_buffer and buildings_buffer
  if (corridor && is.null(network_buffer)) {
    network_buffer <- 3000
    message(sprintf(
      "The default `network_buffer` of %d m is used for corridor delineation.",
      network_buffer
    ))
  }

  if (riverspace && is.null(buildings_buffer)) {
    buildings_buffer <- 100
    message(sprintf(
      paste(
        "The default `buildings_buffer` of %d m is used",
        "for riverspace delineation."
      ),
      buildings_buffer
    ))
  }

  # Retrieve all relevant OSM datasets within the buffer_distance
  osm_data <- get_osmdata(
    city_name, river_name, network_buffer = network_buffer,
    buildings_buffer = buildings_buffer, crs = crs,
    city_boundary = FALSE, force_download = force_download
  )

  # If not provided, determine the CRS
  if (is.null(crs)) crs <- get_utm_zone(osm_data$bb)

  if (corridor) {
    # If using the valley method, and the DEM is not provided, retrieve dataset
    # on a larger aoi to limit edge effects while determining the valley
    if (corridor_init == "valley") {
      if (is.null(dem)) {
        aoi_dem <- buffer(osm_data$aoi_network, dem_buffer)
        dem <- get_dem(aoi_dem, crs = crs, force_download = force_download, ...)
      }
      corridor_init <- delineate_valley(dem, osm_data$river_centerline)
      delineations$valley <- corridor_init
    }

    # Set up the combined street and rail network for the delineation
    network_edges <- dplyr::bind_rows(osm_data$streets, osm_data$railways)
    network <- as_network(network_edges)

    # Run the corridor delineation on the spatial network
    delineations$corridor <- delineate_corridor(
      network, osm_data$river_centerline, max_width = network_buffer,
      corridor_init = corridor_init, max_iterations = max_iterations,
      capping_method = capping_method
    )
  }

  if (segments) {
    # Select the relevant part of the network
    buffer_corridor <- 100  # TODO should this be an additional input parameter?
    corridor_buffer <- sf::st_buffer(delineations$corridor, buffer_corridor)
    network_filtered <- filter_network(network, corridor_buffer)

    delineations$segments <- delineate_segments(delineations$corridor,
                                                network_filtered,
                                                osm_data$river_centerline,
                                                angle_threshold)
  }

  if (riverspace) {
    river_centerline_clipped <- sf::st_intersection(
      osm_data$river_centerline, osm_data$aoi_buildings |> sf::st_transform(crs)
    )
    river_combined <- combine_river_features(river_centerline_clipped,
                                             osm_data$river_surface)
    delineations$riverspace <- delineate_riverspace(river_combined,
                                                    osm_data$buildings)
  }

  delineations
}
