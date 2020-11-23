#' An S4 class to represent alternavive complex, matrix or list input forms.
#' @export
setClassUnion("xy", c("complex", "matrix", "list"))

setValidity("xy", function(object) {
  if(is.complex(object)) return(TRUE)
  if(is.list(object)) {
    if(all(c("x", "y") %in% names(object)) && is.numeric(object$x) && is.numeric(object$y)) {
      return(TRUE)
    } else {
      return("Invalid list argument.")
    }
  }
  if(is.matrix(object)) {
    if(is.numeric(object) && ncol(object) == 2) {
      return(TRUE)
    } else {
      return("Invalid matrix object")
    }
  }
})

#' Coerce to complex
#'
#' Utility function to create complex vectors from arguments
#' specified as in grDevices::xy.coords() or otherwise
#'
#' @param x A numeric vector or missing, or an object inheriting from class "xy"
#' @param y If x is a numeric an optional numeric vector, or missing. If x or y are
#'    missing they are taken as 0, but only one may be missing.
#'
#' @return A complex vector specifying 2-dimensional coordinates
#' @export
#'
#' @examples
#' as_complex(cbind(1:3, 3:1))
#' as_complex(y = 1:3)  ## real parts all zero
setGeneric("as_complex", function(x, y) {
  standardGeneric("as_complex")
})

#' @rdname as_complex
#' @export
setMethod("as_complex", signature(x = "xy", y = "missing"),
          function(x) {
            if(isTRUE(check <- validObject(x, TRUE))) {
              with(grDevices::xy.coords(x, recycle = TRUE), complex(real = x, imaginary = y))
            } else {
              stop(check)
            }
          })

#' @rdname as_complex
#' @export
setMethod("as_complex", signature(x = "numeric", y = "numeric"),
          function(x, y) {
            complex(real = x, imaginary = y)
          })

#' @rdname as_complex
#' @export
setMethod("as_complex", signature(x = "numeric", y = "missing"),
          function(x, y) {
            complex(real = x, imaginary = 0)
          })

#' @rdname as_complex
#' @export
setMethod("as_complex", signature(x = "missing", y = "numeric"),
          function(x, y) {
            complex(real = 0, imaginary = y)
          })


### great circle distances on the earth (approximate)

### Based on an original article by Mario Pineda-Krch, 2010-11-23, in
### https://www.r-bloggers.com/great-circle-distance-calculations-in-r/

#' Great circle distances
#'
#' Calculate great circle distances between pairs of points on sphere,
#' by default, the earth.
#'
#' @param x,y  pair of numeric vectors or single \code{xy} object for Longitude (decimal degrees)
#' @param x1,y1 pair of numeric vectors or a single \code{"xy"} object for Latitude (decimal degrees)
#' @param ... extra parameters passed on to methods
#' @param R radius of the sphere, default the radius of the earth in kilometres
#'
#' @return distances between point pairs, usually in km
#' @export
#'
#' @examples
#' Brisbane <- list(x = 153.0251, y = -27.4698)
#' Cairns <- list(x = 145.7781, y = -16.9186)
#' gcd_km(Brisbane, Cairns)
setGeneric("gcd_km", function(x, y, ...) {
  standardGeneric("gcd_km")
})

#' @rdname gcd_km
#' @export
setMethod("gcd_km", signature(x = "numeric", y = "numeric"),
          function(x, y, x1, y1, ..., R = 6371) { ## R = Earth mean radius [km]
            oldWarn <- options(warn = -1)  ## can barf on very short distances => zero
            on.exit(options(oldWarn))
            xy <- grDevices::xy.coords(x, y, recycle = TRUE)
            xy1 <- grDevices::xy.coords(x1, y1, recycle = TRUE)
            d2r <- base::pi/180 ## convert degrees to radians
            lon1 <- xy$x  * d2r
            lat1 <- xy$y  * d2r
            lon2 <- xy1$x * d2r
            lat2 <- xy1$y * d2r
            d <- acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon2 - lon1)) * R
            if(any(k <- (is.nan(d) | (x == x1 & y == y1)))) d[k] <- 0
            pmin(d, 2 * base::pi * R - d) # Distance in km
          })

#' @rdname gcd_km
#' @export
setMethod("gcd_km", signature(x = "xy", y = "xy"),
          function(x, y, ...) {
            x0 <- grDevices::xy.coords(x, recycle = TRUE)
            y0 <- grDevices::xy.coords(y, recycle = TRUE)
            callGeneric(x0$x, x0$y, y0$x, y0$y, ...)
          })


### interface to plotrix::thigmophobe()

#' Avoid overlaps
#'
#' Generate a vector of positions to use to minimise text overlaps in labelled scatterplots
#'
#' @param x,y any of the forms that the coordinates of a scatterplot may be specified
#' @param ... additional arguments for methods
#' @param xlog,ylog logicals: are the x- and/or y-scales logarithmic?
#' @param usr,pin graphics parameters \code{par("usr"), par("pin")} (or replacements)
#' @param eps numeric: a zero tolerance
#' @param pi numeric: the value of the arithmetic constant of the same name
#'
#' @return a vector of integers all of which are 1, 2, 3, or 4, indicating placement positions.
#' @export
#'
#' @examples
#' set.seed(123)
#' z <- complex(real = runif(50), imaginary = runif(50))
#' mz <- mean(z)
#' z <- z[order(Arg(z - mz))]
#' plot(z, axes = FALSE, ann = FALSE)
#' abline(h = Im(mz), v = Re(mz), lwd = 0.5)
#' arrows(z, length=2.5, lwd = 0.5, gap = 1)
#' box()
#' text(z, pos = avoid(z), cex = 0.7, offset = 0.25,
#'      col = "red", font = 2, xpd = NA)
setGeneric("avoid", function(x, ...)
           standardGeneric("avoid"))

#' @rdname avoid
#' @export
setMethod("avoid", signature(x = "numeric"),
          function (x, y, ..., xlog = par("xlog"), ylog = par("ylog"),
                    usr = par("usr"), pin = par("pin"),
                    eps = .Machine$double.eps, pi = base::pi) {
            n <- max(length(x), length(y))
            z <- complex(real =      rep_len(x, length.out = n),
                         imaginary = rep_len(y, length.out = n))
            z <- usr2in(z, usr = usr, pin = pin)
            xydist <- outer(z, z, function(x, y) Mod(x - y))
            diag(xydist) <- Inf
            nearby <- apply(xydist, 2, which.min)
            zdiff <- z - z[nearby]
            pos <- findInterval(-(Arg(zdiff) + pi/4) %% (2*pi),
                                pi/2*(0:4), all.inside = TRUE)
            for(k in which(Mod(zdiff) <= eps)) {
              pos[sort(c(k, nearby[k]))] <- c(3, 1)
            }
            setNames(pos, seq_along(pos))
          })

#' @rdname avoid
#' @export

setMethod("avoid", signature(x = "xy"),
          function(x, ...) {
            xy <- grDevices::xy.coords(x, recycle = TRUE)
            out <- callGeneric(xy$x, xy$y, ...)
            if(length(names(x)) == length(out)) names(out) <- names(x)
            out
          })

### unit changing functions

#' Conversion functions for plotting
#'
#' Convert user coordinates to inch-based cordinates for the open display,
#' and back again
#'
#' @param x,y any of the forms that the coordinates of a scatterplot may be specified
#' @param ... additional arguments for methods
#' @param xlog,ylog logicals: are the x- and/or y-scales logarithmic?
#' @param usr,pin graphics parameters \code{par("usr"), par("pin")} (or replacements)
#'
#' @return a \code{complex} vector of converted coordinates
#' @export
setGeneric("usr2in", function(x, ...) {
  standardGeneric("usr2in")
})

#' @rdname usr2in
#' @export
setMethod("usr2in", signature(x = "numeric"),
          function(x, y, usr = par("usr"), pin = par("pin"),
                   xlog = par("xlog"), ylog = par("ylog"), ...) {
            xy <- grDevices::xy.coords(x, y, recycle = TRUE) ## safety
            with(xy, {
              x <- ((if(xlog) log(x) else x) - usr[1])/diff(usr[1:2])*pin[1]
              y <- ((if(ylog) log(y) else y) - usr[3])/diff(usr[3:4])*pin[2]
              complex(real = x, imaginary = y)
            })
          })

#' @rdname usr2in
#' @export
setMethod("usr2in", signature(x = "xy"),
          function(x, ...) {
            xy <- grDevices::xy.coords(x, recycle = TRUE)
            callGeneric(xy$x, xy$y, ...)
          })

#' @rdname usr2in
#' @export
setGeneric("in2usr", function(x, ...)
  standardGeneric("in2usr"))

#' @rdname usr2in
#' @export
setMethod("in2usr", signature(x = "numeric"),
          function(x, y, usr = par("usr"), pin = par("pin"),
                   xlog = par("xlog"), ylog = par("ylog"), ...) {
            xy <- grDevices::xy.coords(x, y, recycle = TRUE) ## safety
            with(xy, {
              x <- usr[1] + x/pin[1]*diff(usr[1:2])
              y <- usr[3] + y/pin[2]*diff(usr[3:4])
              complex(real =      if(xlog) exp(x) else x,
                      imaginary = if(ylog) exp(y) else y)
            })
          })

#' @rdname usr2in
#' @export
setMethod("in2usr", signature(x = "xy"),
          function(x, ...) {
            xy <- grDevices::xy.coords(x, recycle = TRUE)
            callGeneric(xy$x, xy$y, ...)
          })

#' Unit change functions
#'
#' Convert imperial to metric units, and vice versa.
#'
#' @param cm,in,mm numeric vectors in the appropriate units
#'
#' @return a numeric vector of values in the new units
#' @name unitChange
NULL

#' @rdname unitChange
#' @export
cm2in <- function(cm) {
  cm / 2.54
}

#' @rdname unitChange
#' @export
mm2in <- function(mm) {
  mm / 25.4
}

#' @rdname unitChange
#' @export
in2cm <- function(inch) {
  inch * 2.54
}

#' @rdname unitChange
#' @export
in2mm <- function(inch) {
  inch * 25.4
}

#' Draw circles on a graphical display
#'
#' A specialised front-end to the traditional graphics function, \code{symbols}
#'
#' @param x,y any of the forms in which points in a scatterplot may be specified
#' @param ... additional arguments passed on to methods
#' @param radii numeric: radii of the circles (relative, must be non-negative)
#' @param maxradius absolute value of the maximum radius of the circles, in centimetres
#' @param fg,colour,colour alternative specification names for circumference colours; may be a palette function
#' @param bg,fill alternative names for the interior colours to use for the circles; may be a palette
#' @param add logical: add to an existing plot, or make a new one?
#' @param xpd graphics parameter
#'
#' @return invisible null value
#' @export
#'
#' @examples
#' z <- with(roundTrip, setNames(complex(real = Longitude, imaginary = Latitude), Locality))
#' plot(z, asp = 1, pch = 20, cex = 0.7, xlab = "Longitude", ylab = "Latitude")
#' lines(Oz, col = alpha("dark green", 0.5))
#' text(z, labels = names(z), pos = avoid(z), cex = 0.7)
#' circles(Latitude ~ Longitude, roundTrip, radii = sqrt(Population), fill = "pink")
setGeneric("circles", function(x, ...)
  standardGeneric("circles"))

#' @rdname circles
#' @export
setMethod("circles", signature(x = "numeric"),
          function(x, y, radii = 1, maxradius = 0.75, ## in cm
                   fg = colour, colour = color, color = "black",
                   bg = fill,   fill = "transparent",
                   add = TRUE, xpd = NA, ...) {
            stopifnot(is.numeric(radii) && all(radii >= 0))
            n <- length(x)
            radii <- rep_len(radii, length.out = n)
            if(is.function(fg)) {
              fg <- fg(n)
            } else {
              fg <- rep_len(fg, length.out = n)
            }
            if(is.function(bg)) {
              bg <- bg(n)
            } else {
              bg <- rep_len(bg, length.out = n)
            }
            graphics::symbols(x, y, circles = radii, inches = cm2in(maxradius),
                              fg = fg, bg = bg, add = add, xpd = xpd)
          })

#' @rdname circles
#' @export
setMethod("circles", signature(x = "xy"),
          function(x, ...) {
            xy <- grDevices::xy.coords(x, recycle = TRUE)
            callGeneric(xy$x, xy$y, ...)
          })

#' @rdname circles
#' @export
setMethod("circles", signature(x = "formula"),
          function(x, data = parent.frame(2), radii = 1, ...) {
            x0 <- eval(x[[3]], data)
            y0 <- eval(x[[2]], data)
            radii <- eval(substitute(radii), data)
            callGeneric(x0, y0, radii, ...)
          })

#################
### additional methods
#################

#' Segments
#'
#' Front end to the \code{graphics::segments} function allowing
#' simplified specification of end points
#'
#' @param x0,y0 two numeric vectors of a single object of class \code{"xy"}
#' @param x1,y1 two numeric vectors of a single object of class \code{"xy"}
#' @param ... additional arguments passed on to \code{graphics::segments}
#' @param circular logical: should the line link up with the initial point?  (Single location argument only.)
#'
#' @return invisible null value
#' @export
#' @examples
#' nam <- c("mai", "mar", "pin", "plt", "usr", "xaxp", "yaxp", "bg")
#' oldPar <- par(nam)
#' par(mar = c(4,4,1,1)+0.1, bg = alpha("sky blue", 0.25))
#' z <- with(roundTrip, setNames(complex(real = Longitude, imaginary = Latitude), Locality))
#' plot(z, asp = 1, pch = 20, cex = 0.7, bty = "n", cex.axis = 0.8,
#'   xlab = "Longitude", ylab = "Latitude", ylim = -c(50,5))
#' lines(Oz, col = alpha("dark green", 0.5))
#' text(z, labels = names(z), pos = avoid(z), cex = 0.7)
#' segments(z, cyc(z), col = "red")
#' if(lazyData::requireData(PBSmapping)) {
#'   plotPolys(worldLLhigh, xlim = c(110, 155), ylim = c(-45, -9),
#'     col = alpha("sandy brown", 0.5), projection = "LL", axes = FALSE,
#'     border = alpha("navy", 0.25), xlab = "", ylab = "", bty = "n")
#'   axis(2, at = 10*(-4:-1), cex.axis = 0.7, line = 2)
#'   axis(1, at = 10*(12:15), cex.axis = 0.7, line = 2)
#'   points(z, pch = 20, cex = 0.7)
#'   text(z, labels = names(z), pos = avoid(z), cex = 0.7)
#'   segments(z, cyc(z), col = "red")
#' }
#'   par(oldPar)
setGeneric("segments", function(x0, y0, ...)
  graphics::segments(x0, y0, ...))

#' @rdname segments
#' @export
setMethod("segments", signature(x0 = "xy", y0 = "xy"),
          function(x0, y0, ...) {
            xy0 <- grDevices::xy.coords(x0)
            xy1 <- grDevices::xy.coords(y0)
            graphics::segments(x0 = xy0$x, y0 = xy0$y, x1 = xy1$x, y1 = xy1$y, ...)
          })

#' @rdname segments
#' @export
setMethod("segments", signature(x0 = "xy", y0 = "missing"),
          function(x0, y0, ..., circular = FALSE) {
            z <- with(grDevices::xy.coords(x0), complex(real = x, imaginary = y))
            if(length(z) < 2) {
              return(invisible(z))
            }
            if(circular) {
              z0 <- z
              z1 <- c(z[-1], z[1])
            } else {
              z1 <- z[-1]
              z0 <- z[-length(z)]
            }
            callGeneric(Re(z0), Im(z0), Re(z1), Im(z1), ...)
          })


#' Arrows
#'
#' Front end to the \code{graphics::arrows} function allowing
#' simplified specification of end points
#'
#' @param x0,y0 two numeric vectors of a single object of class \code{"xy"}
#' @param x1,y1 two numeric vectors of a single object of class \code{"xy"}
#' @param length length of the arrow head barb, in MILLIMETRES, see \code{\link{in2mm}}
#' @param angle as for \code{graphics::arrows}
#' @param gap numeric of length 1 or 2 giving the size of any gap to be left
#'        between the arrow and the points which it connects, in MILLIMETRES
#' @param circular logical: should the arrows link up with the initial point? (Single location argument only.)
#' @param ... additional arguments passed on to \code{graphics::segments}
#'
#' @return invisible null value
#' @export
#' @examples
#' z <- with(roundTrip, setNames(complex(real = Longitude, imaginary = Latitude), Locality))
#' plot(z, asp = 1, pch = 20, cex = 0.7, xlab = "Longitude", ylab = "Latitude")
#' arrows(z, cyc(z), col = "red", gap = c(0,1.5))
setGeneric("arrows", function(x0, y0, ...)
  standardGeneric("arrows"))

#' @rdname arrows
#' @export
setMethod("arrows", signature(x0 = "numeric", y0 = "numeric"),
          function(x0, y0, x1, y1, length = 4, angle = 15, gap, ...) {
            length <- mm2in(length)
            if(!missing(gap)) {
              stopifnot(is.numeric(gap) && length(gap) > 0 && length(gap) < 3 && all(gap >= 0))
              gap <- mm2in(gap)
              gap <- rep_len(gap, length.out = 2)
              if(any(gap > 0)) {
                z0 <- usr2in(x0, y0)
                z1 <- usr2in(x1, y1)
                gp0 <- gp1 <- z1 - z0
                Mod(gp0) <- pmin(gap[1], Mod(gp0)/3)
                Mod(gp1) <- pmin(gap[2], Mod(gp1)/3)
                xy0 <- in2usr(z0 + gp0)
                xy1 <- in2usr(z1 - gp1)
                x0 <- Re(xy0)
                y0 <- Im(xy0)
                x1 <- Re(xy1)
                y1 <- Im(xy1)
              }
            }
            graphics::arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                             length = length, angle = angle, ... )
          })

#' @rdname arrows
#' @export
setMethod("arrows", signature(x0 = "xy", y0 = "xy"),
          function(x0, y0, ...) {
            xy0 <- grDevices::xy.coords(x0)
            xy1 <- grDevices::xy.coords(y0)
            callGeneric(xy0$x, xy0$y, xy1$x, xy1$y, ...)
          })

#' @rdname arrows
#' @export
setMethod("arrows", signature(x0 = "xy", y0 = "missing"),
          function(x0, y0, ..., circular = FALSE) {
            z <- with(grDevices::xy.coords(x0), complex(real = x, imaginary = y))
            if(length(z) < 2) {
              return(invisible(z))
            }
            if(circular) {
              z0 <- z
              z1 <- c(z[-1], z[1])
            } else {
              z1 <- z[-1]
              z0 <- z[-length(z)]
            }
            callGeneric(Re(z0), Im(z0), Re(z1), Im(z1), ...)
          })

#' Text
#'
#' Interface to \code{grpahics::text} allowing more flexibility.  The
#' character string method allows text to be placed in defined regions
#' of the plotting surface in a way similar to that allowed by
#' \code{graphics::legend}
#'
#' @param x main argument: either numeric, of class \code{"xy"}, or a character string
#' @param labels text string(s) to be used as labels, ax for \code{graphics::text}
#' @param adj,pos as for \code{graphics::text}
#' @param guard fraction of the axis length left clear for the character case of \code{x}
#' @param ...
#'
#' @return as for \code{graphics::text}
#' @export
#'
#' @examples
#' plot(1:10)
#' text("top", "(a)", font = 2, family = "serif", cex = 1.5)
#' text("top left", c("Some text", "to go in", "this corner."), family = "mono")
setGeneric("text", function(x, ...)
  graphics::text(x, ...))

#' @rdname text
#' @export
setMethod("text", signature(x = "xy"),
          function(x, ...) {
            xy <- grDevices::xy.coords(x)
            callGeneric(xy$x, xy$y, ...)
          })

#' @rdname text
#' @export
setMethod("text", signature(x = "character"),
          function(x, labels, ..., adj, pos, guard = 0.0125) {
            if(!missing(adj) || !missing(pos))
              warning("Supplied arguments ", sQuote("adj"), " or ",
                      sQuote("pos"), " will not be used.", call. = FALSE)
            labels <- if(inherits(labels, "character")) {
              paste(format(unlist(strsplit(labels, "\n")),
                           justify = "left"), collapse = "\n")
            } else labels
            choices <- c("topleft",    "top",    "topright",
                         "left",       "center", "right",
                         "bottomleft", "bottom", "bottomright")
            xadj <- x0 <- structure(rep(0:2/2, 3), names = choices)
            yadj <- y0 <- structure(rep(2:0/2, each = 3), names = choices)
            x <- sub("centre", "center", gsub("[^[:alpha:]]", "", tolower(x)))
            where <- match.arg(x, choices)
            pu <- par("usr")
            X <- pu[1:2]
            Y <- pu[3:4]
            Xn <- Yn <- c(-guard, 1 + guard)
            x0 <- X[1] + diff(X)/diff(Xn)*(x0 - Xn[1])
            y0 <- Y[1] + diff(Y)/diff(Yn)*(y0 - Yn[1])
            callGeneric(x0[where], y0[where], labels = labels, ...,
                        adj = c(xadj[where], yadj[where]))
          })


#' Bin Widths for Histograms
#'
#' Generates a function to be used as the \code{breaks=} argument of
#' \code{hist}, to ensure bins of a given, fixed width will be used
#'
#' @param w the required bin width in x-units
#' @param align an optional value at which some bin boundary will align.
#' @param ... additional parameters, currently not used
#'
#' @return a breaks function to be used by \code{hist}
#' @export
#'
#' @examples
#' hist(~log(crim), MASS::Boston, binWidth(0.35, align = 0), fill = pal_desert)
binWidth <- function(w, align = NULL, ...) {
  if(!missing(w)) {
    if(!is.numeric(w))
      stop("non-numeric bin width.  Only numeric data is handled at this stage.")
    if(length(w) > 1)
      stop(sprintf("length(w) = %d. Only one fixed bin width can be used.",
                   length(w)))
    w <- abs(w)
  } else {
    w <- NULL
  }
  if(length(align) > 1)
    stop("only one alignment position can be set.")
  dots <- list(...)

  function(x) {
    if(!is.numeric(x))
      stop(sprintf("unable to set widths for objects of class %s.",
                   sQuote(class(x))))
    x <- na.omit(x)
    if(length(x) == 0)
      stop("all values are missing!")
    rx <- range(x)
    px <- pretty(x)
    if(is.null(align)) { ## default case - pick a pretty value
      align <- px[ceiling(length(px)/2)]
    }
    if(is.null(w)) {
      w0 <- diff(rx)/grDevices::nclass.FD(x)
      f0 <- mean(diff(px))/w0
      w <- w0 * f0/max(1, floor(f0))
    }
    if(is.na(align)) { ## symmetrically displayed
      wx <- diff(rx)
      k <- max(1, ceiling(wx/w))
      W <- k*w
      overflow <- (W - wx)/2
      return(seq(rx[1]-overflow, rx[2]+overflow, length=k+1))
    }
    stopifnot(is.numeric(align))
    while(align > rx[1])   align <- align - w
    while(align < rx[1]-w) align <- align + w
    breaks <- seq(align, rx[2], by = w)
    if((bl <- breaks[length(breaks)]) < rx[2])
      breaks <- c(breaks, bl+w)
    breaks
  }
}

#' Bin numbers for histograms
#'
#' Generates a function to be used as the \code{breaks=} argument of
#' \code{hist}, to ensure that a fixed number of bins will be used, precisely.
#' In this case specifying a nice alignment is not available.
#'
#' @param n The required precise number of bins to be used, unaligned.
#' @param ... extra arguments, currently unused
#'
#' @return a breaks function to be used by \code{hist}
#' @export
#'
#' @examples
#' hist(~medv/rm, MASS::Boston, binNumber(25), fill = pal_green2brown)
#' with(MASS::Boston, rug((medv/rm), col = "red"))
#' hist(~log10(medv/rm), MASS::Boston, binNumber(25), fill = pal_green2brown)
#' with(MASS::Boston, rug(log10(medv/rm), col = "red"))
binNumber <- function(n, ...) {
  if(!missing(n)) {
    stopifnot(is.numeric(n) && length(n) == 1 && n %% 1 == 0 && n > 0)
  } else {
    n <- NULL
  }
  dots <- list(...)

  function(x) {
    if(!is.numeric(x))
      stop(sprintf("unable to set widths for objects of class %s.",
                   sQuote(class(x))))
    x <- na.omit(x)
    if(length(x) == 0)
      stop("all values are missing!")
    rx <- range(x)
    dx <- diff(rx)
    if(dx < .Machine$double.eps) {
      dx <- 1
      # stop("data has only one distinct value")
    }
    if(is.null(n)) {
      n <- if(length(x) < 2) 1 else grDevices::nclass.FD(x)
    }
    step <- dx/n
    seq(rx[1] - step/2, rx[2] + step/2, length.out = n+1)
  }
}

#' Barplot
#'
#' Interface to \code{graphics::barplot} with extra flexibility
#'
#' @param height As for \code{graphics::barplot}
#' @param ... additional arguments sent to \code{graphics::barplot}
#' @param border,colour,color alternative argument names for the border; may be a colour palette
#' @param col,fill alternative argument names for the fill colour; may be a colour palette
#'
#' @return as for \code{graphics::barplot}
#' @export
#'
#' @examples
#' ## Poor man's flashy histogram
#' rnorm(5000) %>% cut(c(-Inf,(-5:5)/2,Inf)) %>%
#'    table %>% barplot(fill=pal_cool, space=0, las=2)
barplot <- function(height, ...,
                    border = colour,
                    colour = color,
                    color = NULL,
                    col = fill,
                    fill = NULL) {
  if(is.function(border)) {
    border <- border(length(height))
  }
  if(is.function(col)) {
    col = col(length(height))
  }
  graphics::barplot(height = height, ..., col = col, border = border)
}

#' Draw Rectangles
#'
#' Possibly filled rectangles
#'
#' @param xleft spedifier for the bottom left corner
#' @param ybottom spedifier for the top right corner
#' @param ... additional arguments, as for \code{\link[graphics]{rect}}
#' @param border colour, as for \code{\link[graphics]{rect}}
#' @param colour,color alternative names for \code{border}
#' @param col colour, as for \code{\link[graphics]{rect}}
#' @param fill alternative name for \code{col}
#'
#' @details The primary function in the \code{graphics} package
#'     requiers the corners of the rectangle to be specified by
#'     four separate real numbers.  This interface allows the
#'     corners, in addition, to be specified by two \code{"xy"}
#'     specifiers for the opposite corners, or single \code{"xy"}
#'     object to specify both.  See the example for a typical case.
#' @return as for \code{\link[graphics]{rect}}
#' @export
#'
#' @examples
#' bluish <- alpha("sky blue", 0.5)
#' curve(dnorm, xlim = c(-4,4), n = 1000,  ## establish the plotting coordinate system
#'       xlab = expression(italic(z)),
#'       ylab = expression(phi(italic(z))))
#' corners <- complex(real = par("usr")[1:2], imaginary = par("usr")[3:4])
#' rect(corners, fill = bluish, colour = bluish)
#' curve(dnorm, xlim = c(-4, 4), n = 1000, add = TRUE, col = "red")
#' abline(h = 0, v = 0, lty = "dashed")
#' z <- c(1.96, seq(1.96, 4, length.out = 500))
#' z <- complex(real = z, imaginary = c(0, dnorm(z)))
#' polygon(z, density = 15, angle = -45, colour = "red")
#' arrows(3, 0.05, 2.5, dnorm(2.5)*0.4, length = 2, gap = 2)
#' text(3, 0.05, expression(area == alpha/2), pos = 3, cex = 0.8, offset = 0)
#' Re(z) <- -Re(z)
#' polygon(z, density = 15, angle = 45, colour = "red")
#' arrows(3, 0.05, 2.5, dnorm(2.5)*0.4, length = 2, gap = 2)
#' text(-3, 0.05, expression(area == alpha/2), pos = 3, cex = 0.8, offset = 0)
#' arrows(-3, 0.05, -2.5, dnorm(-2.5)*0.4, length = 2, gap = 2)
#' text(-1.25, 0.05, expression(zeta[  alpha/2] == -zeta[1-alpha/2]), pos = 3, offset = 0)
#' arrows(-1.25, 0.05, -1.96, 0, length = 2, gap = 2)
#' text( 1.25, 0.05, expression(zeta[1-alpha/2]), pos = 3, offset = 0)
#' arrows( 1.25, 0.05,  1.96, 0, length = 2, gap = 2)
#' text("top left",
#'   expression(phi(italic(z)) == frac(exp(-italic(z)^2/2), sqrt(2*pi))), cex = 0.8)
#' text("top right",
#'   expression(Phi(z) == integral(phi(italic(t))*italic(dt), -infinity, z)))
setGeneric("rect", function(xleft, ybottom, ...) {
  standardGeneric("rect")
})

#' @rdname rect
#' @export
setMethod("rect", signature(xleft = "numeric", ybottom = "numeric"),
           function(xleft, ybottom, ...,
                            border = colour,
                            colour = color,
                            color = NULL,
                            col = fill,
                            fill = NA) {
  graphics::rect(xleft, ybottom, ..., col = col, border = border)
})

#' @rdname rect
#' @export
setMethod("rect", signature(xleft = "xy", ybottom = "xy"),
          function(xleft, ybottom, ...) {
            xleft <- grDevices::xy.coords(xleft)
            ybottom <- grDevices::xy.coords(ybottom)
            callGeneric(xleft$x, xleft$y, ybottom$x, ybottom$y, ...)
          })

#' @rdname rect
#' @export
setMethod("rect", signature(xleft = "xy", ybottom = "missing"),
          function(xleft, ybottom, ...) {
            z <- with(grDevices::xy.coords(xleft), complex(real = x, imaginary = y))
            callGeneric(Re(z[1]), Im(z[1]), Re(z[2]), Im(z[2]), ...)
          })


#' Pie charts
#'
#' Alternative interface to \code{graphics::pie} with extra flexibility
#'
#' @param x as for \code{graphics::pie}
#' @param ... additional arguments sent to \code{graphics::pie}
#' @param border,colour,color alternative argument names for the border; may be a colour palette
#' @param col,fill alternative argument names for the fill colour; may be a colour palette
#'
#' @return as for \code{graphics::pie}
#' @export
#'
#' @examples
#' ## A random number of pie slices
#' pie(rep(1, rpois(1, 15)), fill = pal_blue2red, colour = "grey")
pie <- function(x, ...,
                border = colour,
                colour = color,
                color = NULL,
                col = fill,
                fill = NULL) {
  if(is.function(border)) {
    border <- border(length(x))
  }
  if(is.function(col)) {
    col = col(length(x))
  }
  graphics::pie(x = x, ..., col = col, border = border)
}

#' Polygon
#'
#' An alternative interface to the \code{graphics::polygon} function
#'
#' @param x as for \code{graphics::polygon}
#' @param ... additional arguments sent to \code{graphics::polygon}
#' @param border,colour,color alternative argument names for the border
#' @param col,fill alternative argument names for the fill colour
#'
#' @return as for \code{graphics::polygon}
#' @export
#'
#' @examples
#' set.seed(1234)
#' z <- complex(,runif(50), rnorm(50))
#' oz <- ojaMedian(z)
#' z <- z[order(Arg(z - oz))]
#' plot(z, type = "n", axes = FALSE, ann = FALSE)
#' polygon(z, fill = "sandy brown", colour = "grey")
#' points(oz, pch=3, cex = 2)
#' segments(oz, z, lwd = 0.5)
polygon <- function(x, ...,
                    border = colour,
                    colour = color,
                    color = NULL,
                    col = fill,
                    fill = NULL) {
  graphics::polygon(x = x, ..., col = col, border = border)
}

#' Histograms
#'
#' Front-end to \code{\link[graphics]{hist}} allowing a formula interface alternative
#'
#' @param x either a numerical vector or a one-sided formula specifying the response variable
#' @param data a data frame or environment
#' @param ... additional arguments sent to \code{graphics::hist}
#' @param border,colour,color alternative argument names for the border
#' @param col,fill alternative argument names for the fill colour; may be a colout palette function
#' @param main,xlab as for \code{graphics::hist}
#'
#' @return as for \code{graphics::hist}
#' @export
#'
#' @examples
#' hist(~log10(crim), MASS::Boston, binWidth(0.125, align = 0),
#'      fill = pal_sea2sand, colour = "grey", lwd = 0.5, prob = TRUE)
#' rug(~log10(crim), MASS::Boston, col = "hot pink")
#' with(MASS::Boston, lines(density(log10(crim)), col="brown"))
setGeneric("hist", function(x, ...) {
  graphics::hist(x, ...)
})

#' @rdname hist
#' @export
setMethod("hist", signature(x = "numeric"),
          function(x, ...,
                   border = colour,
                   colour = color,
                   color = NULL,
                   col = fill,
                   fill = NULL, main, xlab) {
            #
            #
            fcol <- is.function(col)
            fbor <- is.function(border)
            if(fcol || fbor) {
              dots <- list(x = x, ...)
              ndots <- names(dots)
              if(!("breaks" %in% ndots)) {
                if(any(b <- ndots == "")) {
                  names(dots)[which(b)[1]] <- "breaks"
                } else {
                  dots$breaks <- "Sturges"
                }
              }
              dots$plot <- FALSE
              h <- do.call(graphics::hist, dots[c("x", "breaks", "plot")])
              np <- length(h$mids)
              N <- max(np, ceiling(sum(h$counts)/np))
              k <- with(h, pmax(1, pmin(ceiling(N*density/max(density)), N)))
              if(fcol) {
                col <- col(N)[k]
              }
              if(fbor) {
                border <- border(N)[k]
              }
            }
            #
            #
            xname <- paste(deparse(evalq(substitute(x), parent.frame()),
                                   500), collapse = "\n")
            if(missing(main)) {
              main <- paste("Histogram of", xname)
            }
            if(missing(xlab)) {
              xlab <- xname
            }
            if(!hasArg("plot") || (hasArg("plot") && list(...)$plot)) {
              graphics::hist(x, ..., col = col, border = border,
                             main = main, xlab = xlab)
            } else {
              graphics::hist(x, ...)
            }
          })

#' @rdname hist
#' @export
setMethod("hist", signature(x = "formula"),
          function(x, data = parent.frame(2), ..., main, xlab) {
            xname <- paste(deparse(x[[2]], 500), collapse = "\n")
            if(missing(main)) {
              main <- paste("Histogram of", xname)
            }
            if(missing(xlab)) {
              xlab <- xname
            }
            x <- eval(x[[2]], data, parent.frame(2))
            callGeneric(x, ..., main = main, xlab = xlab)
          })

### density histogram
###

#' Density histogram
#'
#' Construct a density histogram (i.e. \code{probability = TRUE}) as an xy-list
#' in a form suitable for superimposing on other graphics, e.g. density plots.
#' NOTE: this is NOT a plotting function.  Use \code{hist(x, probability = TRUE)}
#' for direct plots.
#'
#' @param x,...  As for \code{hist}, or an object of class \code{"density_histogram"} for the \code{print} method.
#' @param polygon logical: suitable for use with \code{polygon()} rather than \code{lines()}?
#' @param plot Ignored. Plotting is always suppressed
#'
#' @return A list with components including \code{x} and \code{y} suitable for superimposing on other graphics with \code{lines()}
#' @export
#'
#' @examples
#' d <- density(~log(crim), MASS::Boston)
#' h <- density_histogram(~log(crim), MASS::Boston, binWidth(1/3))
#' plot(d, xlim = xrange(d, h), ylim = yrange(d, h),
#'      main = "", xlab = "log(crim)", col = "blue")
#' lines(update(h, polygon = FALSE), col = alpha("red", 0.5))
#' polygon(h, fill = alpha("red", 0.5),
#'         colour = "transparent")
density_histogram <- function(x, ..., polygon = TRUE, plot) {
  Call <- match.call()
  h <- hist(x, ..., plot = FALSE)
  with(h, {
    n <- length(breaks)
    step <- mean(diff(breaks))/2
    down <- pmin(density, c(density[-1], 0))
    x <- c(as.vector(rbind(breaks[-n], breaks[-n], breaks[-1], breaks[-1], NA_real_)),
           breaks[n], breaks[n], NA_real_,
           breaks[1] - step, breaks[n] + step)
    y <- c(as.vector(rbind(0, density, density, down, NA_real_)),
           density[n-1], 0, NA_real_, 0, 0)
    out <- if(polygon) {
      list(x = na.omit(x), y = na.omit(y),
           breaks = breaks, density = density, call = Call)
    } else {
      list(x = x, y = y, breaks = breaks, density = density, call = Call)
    }
    class(out) <- "density_histogram"
    out
  })
}

#' @rdname density_histogram
#' @export
print.density_histogram <- function(x, ..., xlab = deparse(x$call$x), ylab = "Density",
                                    fill = alpha("sky blue", 0.5),
                                    colour = alpha("steel blue", 0.5)) {
  with(x, plot(x, y, type = "n", xlab = xlab, ylab = ylab))
  polygon(x, fill = fill, colour = colour)
  invisible(x)
}


#' Guarded range
#'
#' Convenience function.  Finds the range of a numerical
#' component with a name common to a list of objects,
#' omitting any missing values.  \code{xrange()} and
#' \code{yrange()} are front-ends for the cases \code{component = "x"}
#' and \code{component = "y"} respectively.
#'
#' @param ... Objects, all of which have a numerical component with the same name
#' @param component Character string specifying the name of the component in common
#'
#' @return A 2-component numerical vector specifying a common range
#' @export
#'
#' @examples
#' d <- density(~log(crim), MASS::Boston)
#' h <- density_histogram(~log(crim), MASS::Boston)
#' plot(d, xlim = xrange(h,d), ylim = yrange(h,d))
#' lines(h)
grange <- function(..., component) {
  vals <- sapply(list(...), function(x) range(if(is.numeric(x)) x else x[[component]],
                                              na.rm = TRUE))
  range(vals)
}

#' @rdname grange
#' @export
xrange <- function(...) {
  grange(..., component = "x")
}

#' @rdname grange
#' @export
yrange <- function(...) {
  grange(..., component = "y")
}

#' Rug
#'
#' Front-end to \code{\link[graphics]{rug}} allowing a formula interface
#'
#' @param x A numeric vector or one-sided formula
#' @param data A data frame or environment used if \code{x} is a formula
#' @param ... additional arguments for \code{\link[graphics]{rug}}
#'
#' @return As for \code{\link[graphics]{rug}}.
#' @export
#'
#' @examples
#' hist(~medv/rm, MASS::Boston)
#' rug(~medv/rm, MASS::Boston, col = "red")
setGeneric("rug", function(x, ...) {
  graphics::rug(x, ...)
})

#' @rdname rug
#' @export
setMethod("rug", signature(x = "formula"),
          function(x, data = sys.parent(2), ...) {
            x <- eval(x[[2]], data, .GlobalEnv)
            callNextMethod(x, ...)
          })

#' Boxplot
#'
#' Alternative interface to \code{graphics::boxplot}
#'
#' @param x as for \code{graphics::boxplot}
#' @param ... additional arguments passed on to \code{graphics::boxplot}
#' @param fill alternative argument name to \code{col} for the fill colour; may be a colour palette
#' @param color,colour alternative argument names for \code{border} for the colour of the borders; may be a colour palette
#'
#' @return as for \code{graphics::boxplot}
#' @export
#'
#' @examples
#' ## Poor man's faceting
#' boxplot(Days ~ Age+Sex, MASS::quine, fill = pal_green2brown(4),
#'     axes = FALSE, ann = FALSE)
#' axis(2)
#' box()
#' with(MASS::quine, legend("topleft", levels(Age),
#'      fill = pal_green2brown(4), title = "Age group", bty = "n"))
#' pu <- par("usr")[1:2]
#' M <- matrix(c(3, 1, 1, 3)/4, ncol = 2)
#' abline(v = mean(pu), lwd = 2)
#' mtext(c("Female", "Male"), line = 0.5, at = M %*% pu)
setGeneric("boxplot", function(x, ..., fill, color, colour) {
  standardGeneric("boxplot")
})

#' @rdname boxplot
#' @export
setMethod("boxplot", signature(fill = "missing", color = "missing", colour = "missing"),
          function(x, ...) {
            graphics::boxplot(x, ...)
          })

#' @rdname boxplot
#' @export
setMethod("boxplot", signature(fill = "ANY", color = "ANY", colour = "ANY"),
          function(x, ..., fill, color, colour) {
  hc <- hasArg("col")
  hb <- hasArg("border")
  jc <-  hc && !hb  ## just 'col'
  jb <- !hc &&  hb  ## just 'border'
  bc <-  hb &&  hc  ## 'border' and 'col', both.
  b <- graphics::boxplot(x, ..., plot = FALSE)
  if(bc) return(invisible(graphics::boxplot(x, ...)))
  n <- length(b$n)

  col <- "transparent"
  if(!missing(fill)) {
    col <- if(is.function(fill)) fill(n) else rep(fill, length.out = n)
  }
  if(jb) return(callGeneric(x, ..., col = col))

  border <- "black"
  if(!missing(color)) {
    border <- if(is.function(color)) color(n) else rep(color, length.out = n)
  }
  if(!missing(colour)) {
    border <- if(is.function(colour)) colour(n) else rep(colour, length.out = n)
  }

  if(jc) {
    callGeneric(x, ..., border = border)
  } else {
    callGeneric(x, ..., border = border, col = col)
  }
})

#' Convex Hull
#'
#' Find the convex hull of a planar set of points allowing for peeling.
#'
#' @param x,y  Numeric vectors of equal length or any specification of a set of
#'     points as allowed by grDevices::xy.coords()
#' @param peel The number of convex hulls to peel off the set before taking the
#'     convex hull of the remaining points
#'
#' @return An index vector giving the points in the original set forming the
#'     convex hull after peeling
#' @export
#'
#' @examples
#' z <- complex(real = rnorm(50), imaginary = rnorm(50))
#' plot(z)
#' polygon(z[chull(z)])
#' polygon(z[chull(z, peel = 1)], border = "red")
#' polygon(z[chull(z, peel = 2)], border = "blue")
chull <- function(x, y = NULL, peel = 0) {
  ## the convex hull after "peeling off" peel hulls beforehand.
  ## x, y : coordinates of the points.  May be two separate numeric vectors, a single
  ##        complex vector, a 2-column matrix or a list with two of its components "x" and "y"
  ## peel : an integer value indicating how may hulls to discard first
  ##
  ## returned value: an index vector giving the points of the (internal) hull in clockwise order
  ##                 as for the original chull itself
  z <- with(grDevices::xy.coords(x, y), complex(real = x, imaginary = y))
  if(length(z) == 0) return(integer(0))
  stopifnot(is.numeric(peel) && length(peel) == 1 && peel >= 0)
  names(z) <- seq_along(z)
  while(peel > 0) {
    z <- z[- grDevices::chull(z)]
    peel <- peel - 1
    if(length(z) == 0) return(integer(0))
  }
  hz <- z[grDevices::chull(z)]
  return(as.integer(names(hz)))
}

