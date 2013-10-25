\name{getunits}
\alias{getunits}
\title{SAC TIME SERIES UNITS}
\description{
  Looks up the units of the SAC record. 
  The units in many seismic headers are notoriously
  unreliable, so care should be taken to find an independent source
  to confirm the units. 
}
\usage{
getunits(x)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
}
\value{a character vector that describe the units of the SAC files.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)
getunits(srosa)


}

