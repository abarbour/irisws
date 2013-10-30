\name{traveltime}
\alias{.llpair}
\alias{PS_time.distances}
\alias{PS_time.stations}
\alias{traveltime}
\alias{ttDeg.ws}
\alias{ttKm.ws}
\alias{ttStaSrc.ws}
\alias{ws.ttDeg}
\alias{ws.ttDistances}
\alias{ws.ttKm}
\alias{ws.ttStaSrc}
\title{Access the 'traveltime' Web Service for traveltime computations}
\usage{
  ws.ttDistances(distances,
    distance.units = c("degrees", "kilometers"), depth = 0,
    model = c("iasp91", "prem", "ak135"), phases = NULL,
    filename = NULL, no.header = FALSE,
    traveltime.only = FALSE, rayparam.only = FALSE,
    mintime.only = FALSE, verbose = TRUE)

  PS_time.distances(distances, agg.fun = mean, ...)

  ws.ttDeg(distances, ...)

  ttDeg.ws(distances, ...)

  ws.ttKm(distances, ...)

  ttKm.ws(distances, ...)

  ws.ttStaSrc(event.latlon, station.latlons, depth = 0,
    model = c("iasp91", "prem", "ak135"), phases = NULL,
    filename = NULL, no.header = FALSE,
    traveltime.only = FALSE, rayparam.only = FALSE,
    mintime.only = FALSE, verbose = TRUE)

  ttStaSrc.ws(event.latlon, station.latlons, depth = 0,
    model = c("iasp91", "prem", "ak135"), phases = NULL,
    filename = NULL, no.header = FALSE,
    traveltime.only = FALSE, rayparam.only = FALSE,
    mintime.only = FALSE, verbose = TRUE)

  PS_time.stations(event.latlon, station.latlons,
    agg.fun = mean, ...)

  .llpair(X, verbose = TRUE)
}
\arguments{
  \item{distances}{numeric; great-circle distance from
  source to station. Specify multiple distances as a vector
  (e.g., \code{c(1,20,30)})}

  \item{distance.units}{character; the units of
  \code{distances}, either in decimal degrees or
  kilometers.}

  \item{depth}{numeric; the depth of the event, in
  kilometers.}

  \item{model}{character; Name of 1-D earth velocity model
  to be used. Available models include: \code{iasp91} by
  the Int'l Assoc of Seismology and Physics of the Earth's
  Interior, \code{prem } the Preliminary Reference Earth
  Model, and \code{ak135} by Kennett B.L.N., Engdahl E.R.
  and Buland R. (1995). (See [2].)}

  \item{phases}{character; Comma separated list of phases,
  defaulting to
  \code{p,s,P,S,Pn,Sn,PcP,ScS,Pdiff,Sdiff,PKP,SKS,PKiKP,SKiKS,PKIKP,SKIKS}}

  \item{no.header}{logical; suppresses header from the
  resulting table}

  \item{traveltime.only}{logical; returns a space-separated
  list of travel times, in seconds. \emph{Travel times are
  produced in ascending time order regardless of the order
  in which the phases are specified.}}

  \item{rayparam.only}{logical; will return a
  space-separated list of ray parameters, in sec/deg.}

  \item{mintime.only}{logical; will only retrieve the first
  arrival of each phase for each distance}

  \item{verbose}{logical; should messages be given?}

  \item{...}{additional parameters to
  \code{\link{ws.ttDistances}}}

  \item{event.latlon}{numeric; the lat/lon of the
  eqarthquake epicenter}

  \item{station.latlons}{numeric; the lat(s)/lon(s) of the
  stations.  See \code{X}.}

  \item{agg.fun}{function for data aggregation: i.e.
  \code{\link{mean}}, \code{\link{min}}, or
  \code{\link{max}}}

  \item{X}{numeric; the lat(s)/lon(s) of the stations;
  these can be specified as a single vector with latitudes
  and longitudes concatenated; or (preferrably), a list or
  data.frame. \code{\link{.llpair}} is used to form the
  lat/lon pairs in the format acceptable to IRIS WS}
}
\value{
  A list (invisibly) with the query string, and data from
  the result
}
\description{
  From [1]: \emph{The traveltime webservice calculates
  travel-times for seismic phases using a 1-D spherical
  earth model.} There are three ways to do this
  computation: (1) \code{\link{ws.ttDeg}} for epicentral
  distances specified in decimal degrees; (2)
  \code{\link{ws.ttKm}}, for epicentral distances in
  kilometers; and, (3) \code{\link{ws.ttStaSrc}} for
  epicentral distances specified by the latitudes and
  longitudes of the station/source pair.
}
\details{
  The query is generated with \code{\link{constructor2}},
  and executed with \code{\link{query.iris}}, which thows
  errors based on \code{\link{check.query}}.

  For distance calculations \code{\link{ttDeg.ws}} is the
  primary function; both \code{\link{ws.ttDeg}} and
  \code{\link{ws.ttKm}} are wrapper functions for
  calculations using degrees and kilometers, respectively.
  Note that \code{\link{ttDeg.ws}} is simply a pointer to
  \code{\link{ws.ttDeg}} and similarly for
  \code{\link{ttKm.ws}} and \code{\link{ttStaSrc.ws}}

  \subsection{Utility functions}{ \code{PS_time}: returns
  P- and S-wave data and the difference between the S and P
  information. More specifically,
  \code{\link{PS_time.distances}} does this using
  \code{\link{ws.ttDistances}} with the defaults, except
  for \code{phases=c("P","S")}, and \code{verbose=FALSE};
  hence, distance units are in decimal degrees but can be
  modified through \code{...}. XXX. }

  \subsection{Notes}{ \itemize{ \item{It is advisable
  \emph{not} to turn on \code{traveltime.only=TRUE} or
  \code{rayparam.only=TRUE} \emph{unless} a vector of
  \code{phases} is given; this is because the IRIS WS does
  not return the phase list if these options are enabled,
  so the numbers returned will be essentially meaningless
  (that is, if \code{phases} is not set).  }
  \item{Parameter descriptions were adapted from [1], and
  the defaults used follow [1].} } }
}
\examples{
\dontrun{
#
# In epicentral degrees
wsdeg1 <- ws.ttDistances(c(0,10,20,30,40), verbose=TRUE)
wsdeg2 <- ws.ttDeg(c(0,10,20,30,40), verbose=TRUE)
all.equal(wsdeg1, wsdeg2)
#
# In kilometers
ws.ttKm(c(0,10,20,30,40), verbose=TRUE)
#
# Get P/S wave times for distances
PS_time.distances(1:20)
PS_time.distances(1:20, distance.units="kilometers")
#
# Stations pairs:
# (fake some Lats/lons)
(ws.ttStaSrc(c(1,2),c(20,10,1:10), verbose=FALSE))
(ws.ttStaSrc(c(1,2),list(lats=1:10, lons=11:20), verbose=FALSE))
(ws.ttStaSrc(c(1,2), data.frame(lats=1:10, lons=11:20), verbose=FALSE))
#
PS_time.stations(c(10,20),1:10)
#
# Here's how the lat lon pairs are combined...
.llpair(1:10)
try(.llpair(1:11)) # success, but only because of value recycling
.llpair(data.frame(x=1:5,y=6:10))
.llpair(list(x=1:5,y=6:10))
.llpair(matrix(1:12,ncol=3))
try(.llpair(list(x=1:5,y=6:11))) # failure
}
}
\author{
  AJ Barbour
}
\references{
  [1] \url{http://service.iris.edu/irisws/traveltime/1/}

  [2]
  \url{http://www.iris.edu/dms/products/emc-referencemodels/}
}
\seealso{
  Other WebServices: \code{\link{distaz}},
  \code{\link{distaz.ws}}, \code{\link{flinnengdahl}},
  \code{\link{flinnengdahl.ws}},
  \code{\link{irisws-webservices}},
  \code{\link{timeseries}}, \code{\link{timeseries.ws}},
  \code{\link{webservices}}, \code{\link{ws.distaz}},
  \code{\link{ws.flinnengdahl}},
  \code{\link{ws.timeseries}}
}

