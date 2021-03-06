% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rs_election.R
\name{rs_voronoi}
\alias{rs_voronoi}
\title{Creates voronoi for polling booths by electorate and cuts each of them by the electorate}
\usage{
rs_voronoi(x, crs)
}
\arguments{
\item{x}{A list of 2 Spatial Polygons Data Frame(s). The first contains the electorate outline, the second contains the locations of the polling places.}

\item{crs}{The CRS that the voronoi polygons will be made in. Typically a projected CRS for the relevant area.}
}
\value{
A list with a voronoi diagram for each electorate
}
\description{
Creates voronoi for polling booths by electorate and cuts each of them by the electorate
}
