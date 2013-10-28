\name{flinnengdahl}
\alias{flinnengdahl}
\alias{flinnengdahl.ws}
\alias{ws.flinnengdahl}
\title{Access the 'flinnengdahl' Web Service for Flinn-Engdahl region information}
\usage{
  ws.flinnengdahl(lat, lon, output = c("code", "region"),
    verbose = FALSE)

  flinnengdahl.ws(lat, lon, output = c("code", "region"),
    verbose = FALSE)
}
\arguments{
  \item{lat}{numeric; North latitude, in decimal degrees.}

  \item{lon}{numeric; East longitude, in decimal degrees.}

  \item{output}{character; the information contained in the
  output}

  \item{verbose}{logical; should messages be given?}
}
\value{
  A list (invisibly) with the query string, and data from
  the result
}
\description{
  From [1]: \emph{ The Flinn-Engdahl webservice returns
  either the Flinn-Engdahl region code or region name for a
  specified latitude and longitude.

  Returned values are text strings, e.g. region code “014”
  which corresponds to region name “Kenai Peninsula,
  Alaska”. }
}
\examples{
\dontrun{
flinnengdahl.ws(10,10) # '755'
flinnengdahl.ws(10,10,output="region") # 'NIGERIA'
}
}
\author{
  AJ Barbour
}
\references{
  [1] \url{http://service.iris.edu/irisws/flinnengdahl/2/}
}
\seealso{
  Other WebServices: \code{\link{.llpair}},
  \code{\link{distaz}}, \code{\link{distaz.ws}},
  \code{\link{irisws-webservices}},
  \code{\link{PS_time.distances}},
  \code{\link{timeseries}}, \code{\link{timeseries.ws}},
  \code{\link{traveltime}}, \code{\link{ttDeg.ws}},
  \code{\link{ttKm.ws}}, \code{\link{ttStaSrc.ws}},
  \code{\link{webservices}}, \code{\link{ws.distaz}},
  \code{\link{ws.timeseries}}, \code{\link{ws.ttDeg}},
  \code{\link{ws.ttDistances}}, \code{\link{ws.ttKm}},
  \code{\link{ws.ttStaSrc}}
}

