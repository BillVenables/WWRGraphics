% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_dist.R
\name{geo_dist}
\alias{geo_dist}
\alias{geo_dist_pairwise}
\title{Geographical Distances}
\usage{
geo_dist(lat1, lon1, lat2, lon2)

geo_dist_pairwise(lat1, lon1, lat2 = lat1, lon2 = lon1)
}
\arguments{
\item{lat1, lon1}{Coordinates of origins in decimal degrees}

\item{lat2, lon2}{Coordinates of destinations in decimal degrees}
}
\value{
A numeric vector of distances, in kilometers, or a matrix of such distances
}
\description{
Great circle shortest distances in kilometres between two sets of points on the globe,
or distances between all pairs of locations
}
\examples{
Cns_Bris_Syd_Mel <- data.frame(lat = c(-16.9186, -27.4698, -33.8688, -37.8136),
                               lon = c(145.7781, 153.0251, 151.2093, 144.9631))
with(Cns_Bris_Syd_Mel, geo_dist(lat[1], lon[1], lat[-1], lon[-1]))
with(Cns_Bris_Syd_Mel, geo_dist_pairwise(lat, lon))
}
