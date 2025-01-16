#' CRiSp example OSM data for Bucharest
#'
#' Data extracted from OpenStreetMap for examples used in the CRiSp package.
#' All datasets are provided in a projected coordinate reference system
#' (UTM 35), with exception for the bounding box, which is provided as latitude/
#' longitude coordinates (WGS84).
#'
#' @format A list of sf objects representing:
#' \describe{
#'  \item{bb}{The city bounding box.}
#'  \item{boundary}{The administrative boundary of Bucharest.}
#'  \item{river_centerline}{The Dâmbovița river centerline.}
#'  \item{river_surface}{The Dâmbovița river area.}
#'  \item{streets}{The street network.}
#'  \item{railways}{The railway network.}
#'  \item{buildings}{The buildings in the corridor.}
#' }
#' @source <https://www.openstreetmap.org/about>
"bucharest_osm"

#' CRiSp example DEM data for Bucharest
#'
#' Copernicus GLO-30 Digital Elevation Model (DEM) cropped and retiled to cover
#' the city of Bucharest. Used for examples and vignettes in the CRiSp package.
#'
#' @format A PackedSpatRaster object. Run [`terra::unwrap()`] to extract the
#'   DEM as a SpatRaster object
# nolint start
#' @source <https://spacedata.copernicus.eu/collections/copernicus-digital-elevation-model>
# nolint end
"bucharest_dem"
