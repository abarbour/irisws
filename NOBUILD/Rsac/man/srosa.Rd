\name{srosa}
\docType{data}
\alias{srosa}
\title{EXAMPLE RSAC DATA}
\description{Example object of class \code{"rsac"} data containing 3
  components of motion.
}
\usage{
data(srosa)
}
\format{Vector containing 3 seismic recordings. Each is a different
  component of the seismogram. 
} 
\source{\url{http://www.ncedc.org/ncedc/station.info.html}}
\section{Background}{A 1999 earthquake in California recorded by the BRK
  station of the BK network.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\keyword{datasets}
\examples{

data(srosa)

plot(srosa)

}
