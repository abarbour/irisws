\name{section}
\alias{section}
\title{SEISMIC SECTION}
\description{
  Plots a seismic section given an array of seismic records. The
  recordings are offset by distance, which can be user supplied, or
  calculated from the epicenter and station locations in the header of
  each recording. 
}
\usage{
section(x, dist = NULL, vertdist = TRUE, ampnorm = TRUE, mfactor = 1,
        xlim = NULL, tracelabs = NULL, lty = NULL, col = NULL,
        timeStyle = "axis", offsets = NULL)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
  \item{dist}{vector of distances for each trace; if provided it must
    have the same lenght as \sQuote{x}, otherwise it will be calculated
    from the header info if the station and epicenter locations are
    give.}
  \item{vertdist}{logical; should the vertical axis be distance.}
  \item{ampnorm}{logical; if \sQuote{TRUE} the the amplitudes of all the
    traces will be adjusted to have the same maximum amplitude,
    and the amplitudes scale will not be consistent between the
    different traces.}
  \item{mfactor}{a multiplicative factor to be applied to all the traces
    after scaling.}
  \item{xlim}{time extents of the plot.}
  \item{tracelabs}{a vector of characters to label the traces; if
    provided its length must equal the length of \sQuote{x}.}
  \item{lty, col}{if a single value is given then it is used for all
    traces. If a vector then its length must equal the number of traces
    in \sQuote{x}.}
  \item{timeStyle}{how the time scale should be indicated. Accepts
    \sQuote{axis} and \sQuote{bar}.}
  \item{offsets}{a distance to offset the trace from it's default
    location; this is useful when comparing multiple traces
    (e.g. synthetics) that are located at the same epicentral
    distance. Must equal the number of traces in \sQuote{x}.}
}
\value{a list containing the following elements:
  \item{C}{azimuth, in degrees.}
  \item{B}{back azimuth, in degrees.}
  \item{a}{arc distance, in degrees.}
  \item{lat}{latitude defining the path.}
  \item{lon}{longitude defining the path.}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
data(sumatra)
section(sumatra)
}

