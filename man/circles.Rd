% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{circles}
\alias{circles}
\alias{circles,numeric-method}
\alias{circles,xy-method}
\alias{circles,formula-method}
\title{Draw circles on a graphical display}
\usage{
circles(x, ...)

\S4method{circles}{numeric}(
  x,
  y,
  radii = 1,
  maxradius = 0.75,
  fg = colour,
  colour = color,
  color = "black",
  bg = fill,
  fill = "transparent",
  add = TRUE,
  xpd = NA,
  ...
)

\S4method{circles}{xy}(x, ...)

\S4method{circles}{formula}(x, data = parent.frame(2), radii = 1, ...)
}
\arguments{
\item{x, y}{any of the forms in which points in a scatterplot may be specified}

\item{...}{additional arguments passed on to methods}

\item{radii}{numeric: radii of the circles (relative, must be non-negative)}

\item{maxradius}{absolute value of the maximum radius of the circles, in centimetres}

\item{fg, colour, colour}{alternative specification names for circumference colours; may be a palette function}

\item{bg, fill}{alternative names for the interior colours to use for the circles; may be a palette}

\item{add}{logical: add to an existing plot, or make a new one?}

\item{xpd}{graphics parameter}
}
\value{
invisible null value
}
\description{
A specialised front-end to the traditional graphics function, \code{symbols}
}
\examples{
z <- with(roundTrip, setNames(complex(real = Longitude, imaginary = Latitude), Locality))
plot(z, asp = 1, pch = 20, cex = 0.7, xlab = "Longitude", ylab = "Latitude")
lines(Oz, col = alpha("dark green", 0.5))
text(z, labels = names(z), pos = avoid(z), cex = 0.7)
circles(Latitude ~ Longitude, roundTrip, radii = sqrt(Population), fill = "pink")
}
