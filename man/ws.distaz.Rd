\name{ws.distaz}
\alias{distaz.ws}
\alias{ws.distaz}
\title{Access the 'distaz' Web Service for distance/azimuth computations}
\usage{
  ws.distaz(station.latlon = c(0, 0),
    event.latlon = c(0, 0), verbose = FALSE)

  distaz.ws(station.latlon = c(0, 0),
    event.latlon = c(0, 0), verbose = FALSE)
}
\arguments{
  \item{station.latlon}{numeric; the decimal latitude and
  longitude of the station}

  \item{event.latlon}{numeric; the decimal latitude and
  longitude of the event (source)}

  \item{verbose}{logical; should messages be given?}
}
\value{
  list
}
\description{
  From [1]: \emph{The distance-azimuth service will
  calculate the great-circle angular distance, azimuth, and
  backazimuth between two geographic coordinate pairs. All
  results are reported in degrees, with azimuth and
  backazimuth measured clockwise from North.}
}
\details{
  \code{\link{distaz.ws}} is simply a pointer to
  \code{\link{ws.distaz}}
}
\examples{
\dontrun{
ws.distaz()
ws.distaz(c(0.,0.),c(30.,-100.))
}
}
\author{
  AJ Barbour
}
\references{
  [1] \url{http://service.iris.edu/irisws/distaz/1/}
}
\seealso{
  Other WebServices: \code{\link{timeseries.ws}},
  \code{\link{traveltime}}, \code{\link{ttDeg.ws}},
  \code{\link{ttKm.ws}}, \code{\link{ttStaSrc.ws}},
  \code{\link{ws.timeseries}}, \code{\link{ws.ttDeg}},
  \code{\link{ws.ttDistances}}, \code{\link{ws.ttKm}},
  \code{\link{ws.ttStaSrc}}
}

