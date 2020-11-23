#' @import methods
NULL

#' @importFrom magrittr "%>%"
#' @export "%>%"
NULL

#' @importFrom scales alpha
#' @export alpha
NULL

### aide d'typing
###

#' Unquited character concatenation
#'
#' Allows character strings to be concatenated without quotes, unless
#' the string itself is a non-standard R name
#'
#' @param ... names, normally unquoted, used as strings to be concatenated.  Strings must be quoted if non-standard R names.
#'
#' @return A character string vector
#' @export
#'
#' @examples
#' vars <- cq(alpha, beta, gamma, "_delta_")
cq <- function(...) {  ## quoted concatenation:  any non-standard name must be in "..."
  as.character(sys.call()[-1])
}

#' Check colour string
#'
#' Check if character strings can be used as colour specifications
#'
#' @param x A character string vector
#'
#' @return A logical vector indicating validity
#' @export
#'
#' @examples
#' is.colour(c("dark grey", "horrible yellow", 1))
#' is.colour(1)
is.colour <- function(x) {
  if(is.character(x)) {
    sapply(x, function(x) !inherits(try(col2rgb(x), silent = TRUE), "try-error"))
  } else {
    structure(FALSE, names = class(x))
  }
}

#' @rdname is.colour
#' @export
is.color <- is.colour

## Miscellaneous bits and pieces of general usefulness
##
#' Colour palette functions
#'
#' @param n desired number of colours
#'
#' @return character string vector of colour strings
#' @name palettes
#' @examples
#' barplot(1:15, fill = pal_sea2sand, colour = "red", axes = FALSE) -> bp
#' z <- complex(real = bp,imaginary = seq_along(bp))
#' points(z, xpd = NA)
#' arrows(z[-length(z)], z[-1], gap = 1.5, col = "black")
#' labels <- c("sea", gsub(" ", "0", format(2:14)), "sand")
#' text(z, labels, pos = 3, xpd = NA, col = pal_blue2red(15))
NULL

#' @rdname palettes
#' @export
pal_cool <- colorRampPalette(c("#FFFFBFFF", "#FFFF40FF", "#FFFF00FF", "#FFCC00FF",
                               "#FF9900FF", "#FF6600FF", "#FF3300FF", "#FF0000FF"))


#' @rdname palettes
#' @export
pal_heat_hcl <- colorRampPalette(c("#D33F6A", "#D54865", "#D85161", "#DB595C", "#DD6157", "#DF6852",
                                   "#E1704C", "#E37747", "#E57E41", "#E6853B", "#E78C36", "#E89330",
                                   "#E99A2C", "#E9A129", "#EAA828", "#EAAF29", "#E9B62D", "#E9BD34",
                                   "#E8C33C", "#E7CA45", "#E6D050", "#E5D75D", "#E4DD6B", "#E3E37C",
                                   "#E2E6BD"))

#' @rdname palettes
#' @export
pal_cool_hcl <- colorRampPalette(c("#E2E6BD", "#E3E37C", "#E4DD6B", "#E5D75D", "#E6D050", "#E7CA45",
                                   "#E8C33C", "#E9BD34", "#E9B62D", "#EAAF29", "#EAA828", "#E9A129",
                                   "#E99A2C", "#E89330", "#E78C36", "#E6853B", "#E57E41", "#E37747",
                                   "#E1704C", "#DF6852", "#DD6157", "#DB595C", "#D85161", "#D54865",
                                   "#D33F6A"))

#' @rdname palettes
#' @export
pal_pastels <- colorRampPalette(c("#A7FF7E", "#45FFB2", "#00FFE4", "#00FFFF", "#00FFFF",
                                  "#86FBFF"))

#' @rdname palettes
#' @export
pal_blue2red <- colorRampPalette(c("steel blue", "beige", "orange red"))

#' @rdname palettes
#' @export
pal_sea2sand <- colorRampPalette(c("cadet blue", "lemon chiffon", "sandy brown"))


#' @rdname palettes
#' @export
pal_desert <- colorRampPalette(c("dark green", "dark sea green", "#FFDD74",
                                 "sandy brown", "saddle brown"))

#' @rdname palettes
#' @export
pal_green2brown <- colorRampPalette(c("#418A78", "#548F98", "#6899B1", "#79A7C5",
                                      "#89B6D2", "#ECEBA4", "#F2E27F", "#F6D054",
                                      "#FAAC2F", "#FC7115"))

#' @rdname palettes
#' @export
pal_green2pink <- colorRampPalette(c(`Jungle green` = "#29AB87", `Neon carrot` = "#FFA343",
                                     `Neon fuchsia` = "#FE4164", `Deep pink 4` = "#8B0A50"))
#' @rdname palettes
#' @export
pal_grey <- function(n) {
  rev(grDevices::grey.colors(n))
}

#' @rdname palettes
#' @export
pal_gray <- function(n) {
  rev(grDevices::grey.colors(n))
}


#' Extended colour sets
#'
#' Search an estended database for more colours using a regular
#' expression on their given names
#' @param description A regular expression fot the colour name
#' @param database Either \code{"Set1"} (the default) or \code{"Set2"}.
#' @param ignore.case Logical.  Should case be ignored in the \code{description}?
#'
#' @return A vector of colours matching the \code{description}, or \code{character(0)}.
#' @export
#'
#' @examples
#' getColors("french")  ## Any colour names with "French" as a component.
getColors <- function(description, database = c("Set1", "Set2", "set1", "set2", "1", "2"),
                      ignore.case = TRUE) {
  stopifnot(is.character(description) && length(description) == 1)
  database <- as.character(database)
  database <- match.arg(database)
  choices <- c("Set1" = "Set1", "Set2" = "Set2",
               "set1" = "Set1", "set2" = "Set2",
                  "1" = "Set1",    "2" = "Set2")
  database <- choices[database]
  with(get(database), {
    k <- grep(description, Color, ignore.case = ignore.case)
    if(length(k) == 0)
      return(character())
    structure(Hex[k], names = Color[k])
  })
}

#' @rdname getColors
#' @export
getColours <- getColors

#' Complex vector property replacement functions
#'
#' @param x a complex vector to be altered
#' @param value the numerical value vector to be used in the alteration
#'
#' @return An appropriately modified complex vector
#' @name complexReplacement
NULL

#' @rdname complexReplacement
#' @export
`Re<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(real = value, imaginary = Im(as.complex(x)))), attributes(x)))
}

#' @rdname complexReplacement
#' @export
`Im<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(real = Re(as.complex(x)), imaginary = value)), attributes(x)))
}

#' @rdname complexReplacement
#' @export
`Mod<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(modulus = value, argument = Arg(as.complex(x)))), attributes(x)))
}

#' @rdname complexReplacement
#' @export
`Arg<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(modulus = Mod(as.complex(x)), argument = value)), attributes(x)))
}

### cycle function

#' Circular cycle function
#'
#' Permute the components of a vector or vector-like object in a circular pattern
#'
#' @param z Any object with indexed components
#' @param k integer. The number of steps to cycle: positive: first-to-last, negative: last-to-first
#'
#' @return a structure similar to \code{z} with its components re-ordered by cycling
#' @export
#'
#' @examples
#' cyc(letters, -3)
cyc <- function(z, k = 1) {
  stopifnot(is.numeric(k) && length(k) == 1)
  if(k == 0)
    return(z)
  m <- ifelse(k > 0, 1, -length(z))
  for(j in seq_len(abs(k) %% length(z)))
    z <- c(z[-m], z[m])
  z
}

#' Bivariate medians
#'
#' @param x,y  any of the known ways to specify bivariate points in traditional graphics
#' @param na.rm logical: should any missing values be removed prior to the computation?
#'
#' @return a complex number giving the bivariate median as its real and complex parts
#' @name bivariateMedian
NULL

#' @rdname bivariateMedian
#' @export
coordinateMedian <- function(x, y = NULL, na.rm = FALSE) {
  with(grDevices::xy.coords(x, y), {
    if(na.rm)
      if(any(k <- is.na(x) | is.na(y))) {
        x <- x[!k]
        y <- y[!k]
      }
    complex(real = median(x), imaginary = median(y))
  })
}

#' @rdname bivariateMedian
#' @export
spatialMedian <- function(x, y=NULL, na.rm = FALSE) {
  z <- with(grDevices::xy.coords(x, y), complex(real = x, imaginary = y))
  if(na.rm) z <- na.omit(z)
  fun <- function(theta)
    sum(Mod(z - complex(real = theta[1], imaginary = theta[2])))
  t0 <- c(median(Re(z)), median(Im(z)))
  with(optim(t0, fun, method = "BFGS"),
       complex(real = par[1], imaginary = par[2]))
}

#' @rdname bivariateMedian
#' @export
ojaMedian <- function(x, y = NULL, na.rm = FALSE) {
  xy <- grDevices::xy.coords(x, y, recycle = TRUE)[c("x", "y")]
  if(na.rm) xy <- na.omit(as.data.frame(xy))
  B <- with(xy, {
    ij <- utils::combn(length(x), 2)
    x1 <- x[ij[1,]]
    x2 <- x[ij[2,]]
    y1 <- y[ij[1,]]
    y2 <- y[ij[2,]]
    cbind(x1*y2 - x2*y1, -(y2 - y1), (x2 - x1))
  })
  oja <- function(M)
      sum(abs(B %*% c(1, M)))
  M0 <- sapply(xy, median)
  with(optim(M0, oja, method = "BFGS"),
       complex(real = par[1], imaginary = par[2]))
}

#' Display colours in a collection
#'
#' @param fill either a palette function or a vector of colours to display
#' @param ... additional arguments passed on to methods
#' @param border,colour,color alternative names to use for the colour of the borders
#' @param edges integer: no of edges to use for the inner circle
#' @param radius numeric: value to use for the radius of the circular display
#' @param ... arguments to be passed on to methods
#' @param n number of colours to use for a palette display
#'
#' @return the vector of colours used in the display, invisibly.
#' @export
#'
#' @examples
#' showColors(pal_desert)
#' pal <- myPalette(pink, beige, 'sky blue')
#' showColours(pal)
#' showColours(pal, n = 50)
showColors <- function (fill, ...) {
  UseMethod("showColors")
}

#' @rdname showColors
#' @export
showColors.default <- function(fill,
                               labels = if(length(fill) <= 25) {
                                 if(!is.null(names(fill)))
                                   names(fill) else fill
                               } else NA,
                               border = colour,
                               colour = color,
                               color = if(length(fill) <= 20) "grey" else
                               "transparent",
                               edges = 1000, radius = 1, ...,
                               n = length(fill)) {
  oldPar <- par(bg = "white", mar = rep(1,4))
  on.exit(par(oldPar))
  if(length(fill) == 0) {
    fill <- structure("white", names = " ")
  }
  v <- rep(1, length(fill))
  pie(v, col = fill, labels = labels, edges = edges,
      radius = radius, border = border, ...)
  z <- complex(modulus = 0.3*radius,
               argument = seq(-pi, pi, length=edges))
  polygon(z, col = par("bg"))
  invisible(fill)
}


#' @rdname showColors
#' @export
showColors.function <- function (fill, ..., n = 20) {
  fill <- structure(fill(n),
                    names = paste0(abbreviate(deparse(substitute(fill))),
                                   "_", gsub(" ", "0", format(1:n))))
  NextMethod()
}

#' @rdname showColors
#' @export
showColours <- showColors

#' Generate a colour palette
#'
#' @param ... colour names for the palette, possibly unquoted
#'
#' @return a colour palette function
#' @export
#'
#' @examples
#' pal <- myPalette(pink, beige, 'sky blue')
#' showColours(pal, n = 50)
myPalette <- function(...) {
  colorRampPalette(as.character(sys.call()[-1]))
}


#' Surface colours for rgl
#'
#' Generate colours for use in a surface display with package rgl
#'
#' @param z A numeric matrix of heights
#' @param pal a colour palette to use
#' @param no the number of colours to use
#' @param col,fill Alternative names for the colours to use
#'
#' @return a colour vector suitable for use with rgl surface displays
#' @export
#'
#' @examples
#' cols <- rglColors(volcano)
rglColors <- function(z, pal = pal_pastels, no = length(unique(z)),
                      col = fill, fill = pal(no)) {
  fill[1 + floor((z-min(z))/diff(range(z))*length(col))]
}


#' Surface colours for perspective plots
#'
#' Generates a suitable colour vector for use with \code{graphics::persp}
#'
#' @param z matrix or list structure to specify surface
#' @param pal a palette function to use
#' @param no the number of colours to use
#' @param col,fill alternative names for the vector of colours
#' @param rgl logical: should \code{rgl} protocols be used rather than \code{persp}?
#' @param ... additional arguments passed on to methods (currently not used)
#'
#' @return a suitable vector of colours
#' @export
#'
#' @examples
#' cols <- surfaceColors(volcano, pal = pal_blue2red)
surfaceColors <- function(z, ...) {
  UseMethod("surfaceColors")
}

#' @rdname surfaceColors
#' @export
surfaceColors.list <- function(z, ...)
  if("z" %in% names(z) && is.matrix(z$z)) {
    surfaceColors(z$z, ...)
  } else {
    stop("no matrix component named 'z' found")
  }

#' @rdname surfaceColors
#' @export
surfaceColors.matrix <- function(z,
                                 pal = colorRampPalette(c("steel blue",
                                 "beige", "orange red")),
                                 no = min(250, length(unique(z))),
                                 col = fill, fill = pal(no), rgl = FALSE, ...) {
  nr <- nrow(z)
  nc <- ncol(z)
  if(rgl) {
    matrix(col[cut(z, length(col), include.lowest = TRUE, labels = FALSE)],
           nr, nc)
  } else {
    az <- z[-1, -1] + z[-1, -nc] + z[-nr, -1] + z[-nr, -nc]
    matrix(col[cut(az, length(col), include.lowest = TRUE, labels = FALSE)],
           nr - 1, nc - 1)
  }
}

#' @rdname surfaceColors
#' @export
surfaceColours <- surfaceColors
