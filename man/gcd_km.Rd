% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{gcd_km}
\alias{gcd_km}
\alias{gcd_km,numeric,numeric-method}
\alias{gcd_km,xy,xy-method}
\title{Great circle distances}
\usage{
gcd_km(x, y, ...)

\S4method{gcd_km}{numeric,numeric}(x, y, x1, y1, ..., R = 6371)

\S4method{gcd_km}{xy,xy}(x, y, ...)
}
\arguments{
\item{x, y}{pair of numeric vectors or single \code{xy} object for Longitude (decimal degrees)}

\item{...}{extra parameters passed on to methods}

\item{x1, y1}{pair of numeric vectors or a single \code{"xy"} object for Latitude (decimal degrees)}

\item{R}{radius of the sphere, default the radius of the earth in kilometres}
}
\value{
distances between point pairs, usually in km
}
\description{
Calculate great circle distances between pairs of points on sphere,
by default, the earth.
}
\examples{
Brisbane <- list(x = 153.0251, y = -27.4698)
Cairns <- list(x = 145.7781, y = -16.9186)
gcd_km(Brisbane, Cairns)
}
