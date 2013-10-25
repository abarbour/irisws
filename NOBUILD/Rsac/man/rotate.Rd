\name{rotate}
\alias{rotate}
\title{ROTATE HORIZONTAL COMPONENTS}
\description{
  This function will rotate horizontal components. This is useful for
  getting the horizontal components into the tangential and radial
  components by rotating to the back azimuth from the station to the
  event. The data is truncated so that only times when values are
  defined in both components are returned.
}
\usage{
rotate(s, gcp = FALSE, phi = NULL)
}
\arguments{
  \item{s}{an object of class \code{"rsac"}. Further, it must have
    length 2, both must have the  same sampling rate (dt), and be
    orthogonal horizontal components (cmpinc = 90) at the same station
    (sta).}
  \item{gcp}{logical for if components should be rotated to the "great
    circle path". If \code{TRUE}, then stla, stlo, evla, and evlo must
    be defined so that the back azimuth can be calculated.}
  \item{phi}{if gcp is \code{FALSE} then the components will be rotated
    clockwise by this angle (in radians). This is equivalent to the SAC
    command \code{rotate through v}.}
}
\value{same as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)

srosa[[1]]$cmpinc
srosa[[2]]$cmpinc
srosa[[3]]$cmpinc

HC <- c(srosa[2], srosa[3])
HC[[1]]$evla <- 38.393
HC[[1]]$evlo <- -122.633
HC[[1]]$stla
HC[[1]]$stlo

TR <- rotate(HC, gcp = TRUE)

plot(TR)

}

