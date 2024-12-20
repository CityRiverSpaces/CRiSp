% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valley.R
\name{get_stac_asset_urls}
\alias{get_stac_asset_urls}
\title{Retrieve asset urls for the intersection of a bounding box with a
remote STAC endpoint}
\usage{
get_stac_asset_urls(
  bb,
  endpoint = "https://earth-search.aws.element84.com/v1",
  collection = "cop-dem-glo-30"
)
}
\arguments{
\item{bb}{A bounding box, provided either as a matrix (rows for "x", "y",
columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")}

\item{endpoint}{url of (remote) STAC endpoint}

\item{collection}{STAC collection to be queried}
}
\value{
A list of urls for the assets in the collection
overlapping with the specified bounding box
}
\description{
Retrieve asset urls for the intersection of a bounding box with a
remote STAC endpoint
}
