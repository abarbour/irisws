\name{ws.timeseries}
\alias{timeseries.ws}
\alias{ws.timeseries}
\title{Access to the 'timeseries' Web Service for obtaining continuous data}
\usage{
  ws.timeseries(net, sta, loc, cha, startdate, duration,
    ...)

  timeseries.ws(net, sta, loc, cha, startdate, duration,
    ...)
}
\arguments{
  \item{net}{character; the network code}

  \item{sta}{character; the station code}

  \item{loc}{character; the location code}

  \item{cha}{character; the channel code}

  \item{startdate}{character; the beginning of the record}

  \item{duration}{numeric; the length of the record, in
  seconds}

  \item{...}{additional query parameters}
}
\value{
  list
}
\description{
  From [1]: \emph{The ws-timeseries service provides access
  to individual channels of time series data for a
  specified time range. Requested segments may be up to 30
  days in length and optional signal processing may be
  applied to return data. The time series may be returned
  in a variety of formats.}
}
\details{
  Signal processing options: \itemize{ \item{high, low and
  band-pass filter (\code{})} \item{remove mean value
  (\code{})} \item{scaling by constant value (\code{})}
  \item{deconvolution of instrument response (with
  frequency limits and unit conversion) (\code{})}
  \item{differentiation and integration (\code{})}
  \item{decimation to lower sample rates (\code{})} }

  Output format options : \itemize{ \item{miniSEED}
  \item{SAC Alpha (ASCII)} \item{SAC binary (big & little
  endian)} \item{Audio (WAV format)} \item{PNG Plot}
  \item{ASCII (data column)} \item{ASCII (data column, time
  column)} } The output options can be specifiec with
  (\code{output=}) \code{miniseed}, \code{saca},
  \code{sacbb}, \code{sacbl}, \code{plot}, \code{ascii},
  \code{ascii1}, \code{ascii2}, \code{audio}
}
\author{
  AJ Barbour
}
\references{
  [1] \url{http://service.iris.edu/irisws/timeseries/1/}
}
\seealso{
  Other WebServices: \code{\link{distaz.ws}},
  \code{\link{traveltime}}, \code{\link{ttDeg.ws}},
  \code{\link{ttKm.ws}}, \code{\link{ttStaSrc.ws}},
  \code{\link{ws.distaz}}, \code{\link{ws.ttDeg}},
  \code{\link{ws.ttDistances}}, \code{\link{ws.ttKm}},
  \code{\link{ws.ttStaSrc}}
}

