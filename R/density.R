
#' Density class
#'
#' Promotion of an S3 class to virtual S4
#'
#' @name density
#' @export
setOldClass("density")



#' Kernel density estimation
#'
#' @param x numeric object or formula
#' @param ... additional arguments to be passed on to methods
#' @param data data frame or environment from which to get the data
#' @param n integer: number of points to use in the output
#' @param from,to lower and upper values of x to use in the output
#' @param lower,upper lower and upper known limits to the support, and hence points at which to fold the density
#' @param normalise logical: should the result be re-normalized to have total area unity?
#' @param eps just the tiniest bit, so tiny you would never see it in a million years...
#'
#' @return a \code{density} object
#' @export
#'
#' @examples
#' set.seed(123)
#' oldPar <- par(bg = "beige", font = 2, mar = c(5,5,4,1)+0.1, las = 1)
#' for(N in c(50, 500, 5000)) {
#'   x <- rexp(N, 2)
#'   dx <- density(x)
#'   dx0 <- update(dx, lower = 0)
#'   plot(dx0, col = "red", lwd = 2, bty = "n", main = "KDE Estimation",
#'        xlab = "x", sub = paste("Sample size =", N),
#'        ylab = expression(f[X](x)*',  '*X*' ~ '*exponential(2)),
#'        xlim = xrange(dx, dx0),
#'        ylim = yrange(dexp(0, 2), dx, dx0))
#'   lines(dx, col = "grey", lwd = 2)
#'   curve(dexp(x, 2), add = TRUE, n = 1000, col = "dark green", xpd = NA)
#'   lines(density_histogram(x, binWidth(0.125)), col = alpha("blue", 0.5))
#'   rug(x, col = alpha("red", (order(order(x))/N)^0.25))
#'   grid()
#'   legend("center", text.font = 2,
#'          c("naive KDE", "KDE folded at 0", "naive density histogram", "true density"),
#'          col = c("grey", "red", alpha("blue", 0.5), "dark green"),
#'          lty = "solid", lwd = 2, cex = 0.8, bty = "n",
#'          title = "Method")
#' }
#' par(oldPar)
#'
setGeneric("density", function(x, ...) {
  dim(x) <- NULL
  dens <- standardGeneric("density")
  dens$call <- match.call()
  # as(dens, "Density")
  dens
})

#' @rdname density
#' @export
setMethod("density", signature(x = "numeric"),
          function(x, n = 4096, from, to, ..., lower = -Inf, upper = Inf,
                   normalize = TRUE, eps = .Machine$double.eps) {
            stopifnot(is.numeric(x), length(x) > 1,
                      is.numeric(n), length(n) == 1, n > 1, n %% 1 == 0,
                      is.numeric(lower), length(lower) == 1,
                      is.numeric(upper), length(upper) == 1,
                      all(lower <= x & x <= upper))
            fromPresent <- !missing(from)
            if(fromPresent) stopifnot(is.numeric(from), length(from) == 1)
            toPresent <- !missing(to)
            if(toPresent) stopifnot(is.numeric(to), length(to) == 1)
            d0 <- stats::density(x, n = n, ...)
            k <- which(sapply(sys.calls(), `[[`, 1) == quote(density))[1]
            d0$call <- match.call(density, sys.calls()[[k]])
            d0$data.name <- deparse(d0$call$x)
            N <- length(d0$x)
            hasLow <- !(missing(lower) | is.infinite(lower))
            hasUpp <- !(missing(upper) | is.infinite(upper))
            if(hasLow || hasUpp) {
              low <- max(d0$x[1], min(lower, upper))
              upp <- min(d0$x[N], max(lower, upper))
              d0 <- within.list(d0, {
                d0f <- approxfun(x, y, yleft = 0, yright = 0, rule = 1)
                x <- seq(low, upp, length = N)
                y <- d0f(x) + d0f(low + (low - x)) + d0f(upp + (upp - x))
                rm(d0f)
              })
            }
            within.list(d0, {
              if(hasLow) {
                x <- c(x[1]-eps, x)
                y <- c(0, y)
               }
              if(hasUpp) {
                x <- c(x, x[length(x)]+eps)
                y <- c(y, 0)
              }
              if(normalize) {
                y <- y/sum(diff(x) * (y[-1] + y[-length(y)])/2)
              }
               if(fromPresent) {
                y <- y[x >= from]
                x <- x[x >= from]
              }
              if(toPresent) {
                y <- y[x <= to]
                x <- x[x <= to]
              }
              n <- length(x)
         })
          })

#' @rdname density
#' @export
setMethod("density", signature(x = "formula"),
          function(x, data = parent.frame(2), ...) {
            x <- eval(x[[length(x)]], data)
            callGeneric(x, ...)
          })

############ mixtures
############

#' Density to function
#'
#' Coercion function for kernel density object into R functions
#'
#' @param x A kernel density object as produced by \code{\link{density}}
#' @param ... ignored
#'
#' @return An R function of one argument providing the KDE at any specific set of values
#' @export
as.function.density <- function(x, ...) {
  with(x, stats::approxfun(x, y, yleft = 0, yright = 0))
}

## #' Arithmetic operations on density estimates
## #'
## #' This facility is mainly intended for generating mixtures of KDEs
## #'
## #' @param e1 either \code{"numeric"} or \code{"Density"} object
## #' @param e2 either \code{"numeric"} or \code{"Density"} object
## #' @name DensityArith
## #' @return A KDE, or KDE-like object representing the operation.
## #' @export
## #'
## #' @examples
## #' d1 <- density(rnorm(50000))
## #' d2 <- density(rnorm(5000, 3, 0.5))
## #' d12 <- 0.95*d1 + 0.05*d2  ## 5% contaminated normal
## #' plot(d12, main = "Normal with 5% contamination")
## #' lines(0.95*d1, col = "steel blue")
## #' lines(0.05*d2, col = "red")
## setMethod("Arith", signature(e1 = "numeric", e2 = "Density"),
##           function(e1, e2) {
##             e2$y <- callGeneric(e1, e2$y)
##             e2$data.name <- paste0(e1, .Generic, "(",e2$data.name,")")
##             e2
##           })
##
## #' @rdname DensityArith
## #' @export
## setMethod("Arith", signature(e1 = "Density", e2 = "numeric"),
##           function(e1, e2) {
##             e1$y <- callGeneric(e1$y, e2)
##             e1$data.name <- paste0("(",e1$data.name,")", .Generic, e2)
##             e1
##           })
##
## #' @rdname DensityArith
## #' @export
## setMethod("Arith", signature(e1 = "Density", e2 = "Density"),
##           function(e1, e2) {
##             x0 <- sort(unique(c(e1$x, e2$x)))
##             y0 <- callGeneric(as.function(e1)(x0), as.function(e2)(x0))
##             out <- structure(list(x = x0, y = y0, bw = (e1$bw + e2$bw)/2, n = e1$n + e2$n,
##                                   call = quote(density(x)),
##                                   data.name = paste0(e1$data.name, .Generic, e2$data.name),
##                                   has.na = e1$has.na || e2$has.na),
##                              class = "density")
##             as(out, "Density")
##           })

#' Arithmetic operations on density estimates
#'
#' This facility is mainly intended for generating mixtures of KDEs
#'
#' @param e1 either \code{"numeric"} or \code{"density"} object
#' @param e2 either \code{"numeric"} or \code{"density"} object
#' @name DensityOps
#' @return A KDE, or KDE-like object representing the operation.
#' @export
#'
#' @examples
#' set.seed(1234)
#' y1 <- rnorm(90000, 0.0, 1.0)
#' y2 <- rnorm(10000, 2.5, 0.5)
#' d1 <- density(y1)
#' d2 <- density(y2)
#' d12 <- 0.9*d1 + 0.1*d2  ## 5% contaminated normal
#' plot(d12, main = "Normal with 5% contamination")
#' grid(lty = "dashed")
#' lines(0.9*d1, col = "dark green", lwd = 1.5)
#' lines(0.1*d2, col = "red", lwd = 1.5)
#' curve(0.9*dnorm(x, 0, 1) + 0.1*dnorm(x, 2.5, 0.5),
#'       col = "purple", lty = "dashed", add = TRUE)
#' dyy <- density(c(y1, y2))
#' plot(dyy, type = "n",
#'      main = "Density of mixture -vs- mixture of densities")
#' grid(lty = "dashed")
#' polygon(density_histogram(c(y1, y2), binWidth(0.3)),
#'         fill = alpha("sky blue", 0.5),
#'         colour = "transparent")
#' lines(dyy, col = "dark green")
#' lines(d12, col = "red", lty = "dashed", lwd = 2)
Ops.density <- function(e1, e2) {
  if(is.numeric(e1)) {
    out <- e2
    out$data.name <- paste0(e1, .Generic, "(", e2$data.name, ")")
    e2 <- e2$y
    out$y <- NextMethod(.Generic)
    return(out)
  }
  if(is.numeric(e2)) {
    out <- e1
    out$data.name <- paste0("(", e1$data.name, ")", .Generic, e2)
    e1 <- e1$y
    out$y <- NextMethod(.Generic)
    return(out)
  }
  x0 <- sort(unique(c(e1$x, e2$x)))
  bw <- (e1$bw + e2$bw)/2
  n <- e1$n + e2$n
  call <- quote(density(x))
  data.name <- paste0(e1$data.name, .Generic, e2$data.name)
  has.na <- e1$has.na || e2$has.na
  e1 <- as.function(e1)(x0)
  e2 <- as.function(e2)(x0)
  structure(list(x = c(x0[1] - .Machine$double.eps, x0,
                       x0[length(x0)] + .Machine$double.eps),
                 y = c(0, NextMethod(.Generic), 0),
                 bw = bw, n = n, call = call,
                 data.name = data.name, has.na = has.na),
            class = "density")
}
