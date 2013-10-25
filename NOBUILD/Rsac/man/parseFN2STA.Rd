\name{parseFN2STA}
\alias{parseFN2STA}
\title{PARSE STATION NAME}
\description{
  Gets the station name and the component name from the file
  name. Assumes that the station name and the component name are the
  last  items on the finle name seperated by a period.
}
\usage{
parseFN2STA(fn)
}
\arguments{
  \item{fn}{file name.}
}
\value{a list containing the station name info. The
  specific elements of the list are:
  \item{sta}{station name.}
  \item{comp}{component.}
}
\seealso{\code{\link{GET.seis}}
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\keyword{internal}
