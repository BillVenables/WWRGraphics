#' Polygon area
#'
#' Calculate the area of a non-intersecting polygon given its vertices
#'
#' @param x,y numeric vectors defining vertices in order.  Any method
#'     recognized by grDevices::xy.coords() is accepted.
#'     Or a formula for other methods.  Or an object of class "area_polygon"
#' @param data a data frame
#' @param ... Additional arguments, currently not used
#'
#' @return An object of class "area_polygon". A single numerical value with the polygon area
#'     with an attribute giving the sign, as determined by the orientation of the vertices.
#' @export
#'
#' @examples
#' z <- complex(real = rnorm(200), imaginary = rnorm(200))
#' hz <- z[chull(z)]
#' plot(hz)
#' polygon(hz)
#' points(z, pch = ".", cex = 2, col = "red")
#' text(mean(hz), labels=paste("area =", area_polygon(hz)))
area_polygon <- function(x, ...) {
  UseMethod("area_polygon")
}

#' @rdname area_polygon
#' @export
area_polygon.default <- function(x, y = NULL, ...) {
  a <- with(xy.coords(x, y), sum(x*c(y[-1], y[1]) - y*c(x[-1], x[1])))
  structure(abs(a)/2, sign = sign(a), class = "area_polygon")
}

#' @rdname area_polygon
#' @export
area_polygon.formula <- function(x, data = parent.frame(), ...) {
  environment(x) <- as.environment(data)
  data <- NULL
  NextMethod()
}

#' @rdname area_polygon
#' @export
print.area_polygon <- function(x, ...) {
  y <- x
  x <- as.vector(x)
  NextMethod()
  invisible(y)
}
