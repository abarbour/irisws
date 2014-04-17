\name{irisws-wadl}
\alias{constructor2.iriswadl}
\alias{describe}
\alias{describe.iriswadl}
\alias{irisws-wadl}
\alias{is.iriswadl}
\alias{waddler}
\alias{wadl}
\title{Accessing IRIS WS services with .wadl protocol}
\usage{
waddler(service, ...)

wadl(u, ...)

is.iriswadl(x, ...)

\method{constructor2}{iriswadl}(x, ...)

describe(x, ...)

\method{describe}{iriswadl}(x, ...)
}
\arguments{
  \item{service}{character}

  \item{u}{the full url}

  \item{x}{object to test, describe, or query-construct
  with}

  \item{...}{additional objects}
}
\description{
WADL is the Web Applications Description Language, which is
essentially a way to specify WS parameters through dressed
up XML
}
\examples{
wd <- waddler("timeseries")
class(wd)
is.iriswadl(wd)
# print some information and return query parameters
describe(wd)
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

