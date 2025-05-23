% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corridor.R
\name{initial_corridor}
\alias{initial_corridor}
\title{Draw the initial geometry of a river corridor.}
\usage{
initial_corridor(river, method = "valley", buffer = NULL, dem = NULL)
}
\arguments{
\item{river}{A simple feature geometry representing the river}

\item{method}{The method employed to draw the initial river corridor:
\itemize{
\item "buffer": add a fixed buffer region to the river geometry (see
\code{\link[=river_buffer]{river_buffer()}})
\item "valley" (default): use the river valley boundary, as estimated from the
provided digital elevation model (DEM, see \code{\link[=delineate_valley]{delineate_valley()}} for
details on the implementation)
}}

\item{buffer}{Buffer region to add to the river geometry (only used if
\code{initial_method} is \code{"buffer"})}

\item{dem}{Digital elevation model (DEM) of the region (only used if
\code{initial_method} is \code{"valley"})}
}
\value{
A simple feature geometry
}
\description{
Draw the initial geometry of a river corridor.
}
\keyword{internal}
