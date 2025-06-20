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
#' @param dem_buffer Add a buffer (an integer in meters) for retrieving the DEM.
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param dem_buffer Add a buffer (an integer in meters) around the
#'   river to retrieve the DEM.
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
#'  [delineate_segments()] and [rcoins::stroke()]. Only used if `segments` is
#'  TRUE.
#' @param valley Whether to return the initial valley (only possible if
#'   `initial_method` is `"valley"`).
#' @param corridor Whether to carry out the corridor delineation
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#' @param force_download Download data even if cached data is available
#' @param ... Additional (optional) input arguments for retrieving the DEM
#'   dataset (see [get_dem()]). Only relevant if `initial_method` is `"valley"`
#'   and `dem` is NULL
#'
#' @return A list with the corridor, segments, and riverspace geometries
#' @export
#' @examplesIf interactive()
#' delineate("Bucharest", "Dâmbovița")
delineate <- function(
  city_name, river_name, crs = NULL, network_buffer = NULL,
  buildings_buffer = NULL, dem_buffer = 2500, initial_method = "valley",
  initial_buffer = NULL, dem = NULL, max_iterations = 10,
  capping_method = "shortest-path", angle_threshold = 100, valley = FALSE,
  corridor = TRUE, segments = FALSE, riverspace = FALSE,
  force_download = FALSE, ...
) {

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

  if (valley) {
    if (initial_method == "valley") {
      if (is.null(dem)) {
        aoi_dem <- buffer(osm_data$aoi_network, dem_buffer)
        dem <- get_dem(aoi_dem, crs = crs, force_download = force_download, ...)
      }
      valley <- delineate_valley(dem, osm_data$river_centerline)
    } else {
      stop('The valley can only be returned if `initial_method` is `"valley"`')
    }
  }

  if (corridor) {
    # If using the valley method, and the DEM is not provided, retrieve dataset
    # on a larger aoi to limit edge effects while determining the valley
    if (initial_method == "valley" && is.null(dem) && valley == FALSE) {
      aoi_dem <- buffer(osm_data$aoi_network, dem_buffer)
      dem <- get_dem(aoi_dem, crs = crs, force_download = force_download, ...)
    }

    # Set up the combined street and rail network for the delineation
    network_edges <- dplyr::bind_rows(osm_data$streets, osm_data$railways)
    network <- as_network(network_edges)

    # Run the corridor delineation on the spatial network
    corridor <- delineate_corridor(
      network, osm_data$river_centerline, max_width = network_buffer,
      initial_method = initial_method, buffer = initial_buffer, dem = dem,
      max_iterations = max_iterations, capping_method = capping_method
    )
  } else {
    corridor <- NULL
  }

  if (segments) {
    # Select the relevant part of the network
    buffer_corridor <- 100  # TODO should this be an additional input parameter?
    corridor_buffer <- sf::st_buffer(corridor, buffer_corridor)
    network_filtered <- filter_network(network, corridor_buffer)

    segments <- delineate_segments(corridor, network_filtered,
                                   osm_data$river_centerline, angle_threshold)
  } else {
    segments <- NULL
  }

  if (riverspace) {
    river_centerline_clipped <- sf::st_intersection(
      osm_data$river_centerline, osm_data$aoi_buildings |> sf::st_transform(crs)
    )
    river_combined <- combine_river_features(river_centerline_clipped,
                                             osm_data$river_surface)
    riverspace <- delineate_riverspace(river_combined, osm_data$buildings)
  } else {
    riverspace <- NULL
  }

  list(valley = valley,
       corridor = corridor,
       segments = segments,
       riverspace = riverspace)
}
