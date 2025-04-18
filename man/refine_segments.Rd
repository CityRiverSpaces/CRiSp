% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segments.R
\name{refine_segments}
\alias{refine_segments}
\title{Refine candidate segments via recursive merging}
\usage{
refine_segments(blocks, river_centerline, corridor)
}
\arguments{
\item{blocks}{Candidate segments as a simple feature geometry}

\item{river_centerline}{The river centerline as a simple feature geometry}

\item{corridor}{The river corridor as a simple feature geometry}
}
\value{
Refined corridor segments as a simple feature geometry
}
\description{
Recursively merge the candidate segments provided ("blocks"), until they all
intersect the river centerline and both sides of the corridor.
}
\keyword{internal}
