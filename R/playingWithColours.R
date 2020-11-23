### find nearest colour
###


#' Random Colours
#'
#' Generate a set of random colours for demonstration purposes
#'
#' @param n integer: the number of colours required
#'
#' @return A character string vector of the colours in hex format
#' @export
#'
#' @examples
#' set.seed(1234)
#' showColours(random_colours(20))
random_colours <- function(n) {
  hx <- c(0:9, LETTERS[1:6])
  do.call(paste0, c("#", replicate(6, sample(hx, n, rep=TRUE), simplify = FALSE)))
}

#' Colours to Hex Format
#'
#' Convert a vector of colours in any form to hex
#'
#' @param colours The character string vector of colours
#' @param names_ The names to be given to the components of the result
#'
#' @return A character string vector of the colours in hex format
#' @export
#'
#' @examples
#' col2hex(colors()[5:15])
col2hex <- function(colours, names_ = nam) {
  num <- col2rgb(colours)
  nam <- names(colours)
  if(is.null(nam)) nam <- colours
  rgb(red = num[1, ], green = num[2, ], blue = num[3, ],
      names = names_, maxColorValue = 255)
}

#' Find Closest Colours
#'
#' Find the colours in a reference set closest to a specified set of
#' colours (in RGB space with L1 norm).
#'
#' @param colours The search colours, in any form but usually in hex
#' @param reference The reference set of colours
#'
#' @return A set of colours in the reference set closest to those in the search set;
#'         for \code{colour_distance} a \code{dist} object of pairwise distances.
#' @export
#'
#' @examples
#' set.seed(1234)
#' rc <- random_colours(10)
#' cols <- as.vector(rbind(rc, closest_colours(rc)))
#' showColours(cols)
#' d <- dist_lab(cols)
#' ds <- cmdscale(d)/1000
#' plot(ds, asp = 1, type = "n", bty = "n",
#'      xlab = "First coordinate", ylab = "Second coordinate")
#' text(ds, labels = cols, pos = avoid(ds), cex = 0.7,
#'      offset = 0.35, xpd = NA)
#' i <- 2*(1:10)
#' segments(ds[i-1, ], ds[i, ])
#' points(ds, pch = 19, col = cols)
closest_colours <- function(colours, reference = colors()) {
  labr <- convertColor(t(col2rgb(reference)), from = "sRGB", to = "Lab")
  labc <- convertColor(t(col2rgb(colours)),   from = "sRGB", to = "Lab")
  d <- matrix(0, length(reference), length(colours))
  for(j in 1:3) {
    d <- d + outer(labr[, j], labc[, j], function(x, y) (x - y)^2)
  }
  reference[apply(d, 2, which.min)]
}

#' @rdname closest_colours
#' @export
dist_rgb <- function(colours) {
  rcol <- col2rgb(colours)
  rbar <- outer(rcol[1, ], rcol[1, ], function(x, y) (x+y)/2)
  d <- matrix(0, ncol(rcol), ncol(rcol))
  d <- d +         (2 + rbar/256)*outer(rcol[1,], rcol[1,], function(x, y) (x-y)^2)
  d <- d +                      4*outer(rcol[2,], rcol[2,], function(x, y) (x-y)^2)
  d <- d + (2 + (255 - rbar)/256)*outer(rcol[3,], rcol[3,], function(x, y) (x-y)^2)
  stats::as.dist(sqrt(d))
}

#' @rdname closest_colours
#' @export
dist_lab <- function(colours) {
  rcol <- convertColor(t(col2rgb(colours)), from = "sRGB", to = "Lab")
  d <- matrix(0, length(colours), length(colours))
  for(j in 1:3) {
    d <- d + outer(rcol[, j], rcol[, j], function(x, y) (x - y)^2)
  }
  stats::as.dist(sqrt(d))
}
