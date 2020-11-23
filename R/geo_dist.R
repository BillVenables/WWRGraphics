deg <- function (x) {
  (x/(2 * pi)) * 360
}
pi <- base::pi
rad <- function (x) {
  (x/360) * 2 * pi
}

#' Geographical Distances
#'
#' Great circle shortest distances in kilometres between two sets of points on the globe,
#' or distances between all pairs of locations
#'
#' @param lat1,lon1 Coordinates of origins in decimal degrees
#' @param lat2,lon2 Coordinates of destinations in decimal degrees
#'
#' @return A numeric vector of distances, in kilometers, or a matrix of such distances
#' @export
#'
#' @examples
#' Cns_Bris_Syd_Mel <- data.frame(lat = c(-16.9186, -27.4698, -33.8688, -37.8136),
#'                                lon = c(145.7781, 153.0251, 151.2093, 144.9631))
#' with(Cns_Bris_Syd_Mel, geo_dist(lat[1], lon[1], lat[-1], lon[-1]))
#' with(Cns_Bris_Syd_Mel, geo_dist_pairwise(lat, lon))
geo_dist  <- function (lat1, lon1, lat2, lon2) {
  gd <- (sin(rad(lat1)) * sin(rad(lat2))) +
    (cos(rad(lat1)) * cos(rad(lat2)) * cos(rad(abs(lon2 - lon1))))
  gd <- ifelse(lat1 == lat2 & lon1 == lon2, 0, deg(acos(gd)))
  111.194926644559 * gd
}

#' @rdname geo_dist
#' @export
geo_dist_pairwise <- function(lat1, lon1, lat2 = lat1, lon2 = lon1) {
  z1 <- complex(real = lon1, imaginary = lat1)
  z2 <- complex(real = lon2, imaginary = lat2)
  outer(z1, z2, function(z1, z2) {
    geo_dist(Im(z1), Re(z1), Im(z2), Re(z2))
  })
}
