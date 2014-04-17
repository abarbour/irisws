\name{irisws-wadl}
\alias{constructor2.iriswadl}
\alias{describe}
\alias{describe.iriswadl}
\alias{irisws-wadl}
\alias{is.iriswadl}
\alias{waddler}
\alias{wadl}
\title{Find the WADL file for an IRIS WS}
\usage{
waddler(service, ...)

wadl(service, ...)

is.iriswadl(x, ...)

\method{constructor2}{iriswadl}(x, ...)

describe(x, ...)

\method{describe}{iriswadl}(x, ...)
}
\arguments{
  \item{service}{character}

  \item{x}{object to test, describe, or query-construct
  with}

  \item{...}{additional objects}
}
\description{
Find the WADL file for an IRIS WS
}
\author{
A.J. Barbour
}
\references{
[1] \url{http://www.w3.org/Submission/wadl/}
}
\seealso{
\code{\link{iris.query}} to query IRIS WS

\code{\link{irisws-package}}

Other Utilities: \code{\link{ISOdatetime.j}},
\code{\link{ISOtimestring}},
\code{\link{irisws-timeutilities}},
\code{\link{timestring}}; \code{\link{check.query}},
\code{\link{iris.query}}, \code{\link{query.iris}};
\code{\link{constructor}}, \code{\link{constructor2}},
\code{\link{constructor2.default}},
\code{\link{irisws-queryconstruction}},
\code{\link{params2queryparams}}; \code{\link{irisws}},
\code{\link{irisws-package}}
}

