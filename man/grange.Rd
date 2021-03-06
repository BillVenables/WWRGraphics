% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{grange}
\alias{grange}
\alias{xrange}
\alias{yrange}
\title{Guarded range}
\usage{
grange(..., component)

xrange(...)

yrange(...)
}
\arguments{
\item{...}{Objects, all of which have a numerical component with the same name}

\item{component}{Character string specifying the name of the component in common}
}
\value{
A 2-component numerical vector specifying a common range
}
\description{
Convenience function.  Finds the range of a numerical
component with a name common to a list of objects,
omitting any missing values.  \code{xrange()} and
\code{yrange()} are front-ends for the cases \code{component = "x"}
and \code{component = "y"} respectively.
}
\examples{
d <- density(~log(crim), MASS::Boston)
h <- density_histogram(~log(crim), MASS::Boston)
plot(d, xlim = xrange(h,d), ylim = yrange(h,d))
lines(h)
}
