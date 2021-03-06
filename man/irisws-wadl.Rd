% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wadl.R
\name{irisws-wadl}
\alias{describe}
\alias{describe.iriswadl}
\alias{irisws-wadl}
\alias{is.iriswadl}
\alias{parameters}
\alias{parameters.iriswadl}
\alias{waddler}
\alias{wadl}
\title{Accessing IRIS WS services with .wadl protocol}
\usage{
waddler(service, ...)

wadl(u, ...)

is.iriswadl(x, ...)

parameters(x, ...)

\method{parameters}{iriswadl}(x, ...)

describe(x, ...)

\method{describe}{iriswadl}(x, ...)
}
\arguments{
\item{service}{character}

\item{...}{additional objects}

\item{u}{the full url}

\item{x}{object to test, describe, or query-construct with}
}
\description{
WADL is the Web Applications Description Language, which
is essentially a way to specify WS parameters 
through dressed up XML

These functions can only be used to inspect the 
query protocol for a given webservice.
}
\examples{
# simply parse some wadl-xml:
wadl("http://service.iris.edu/irisws/distaz/1/application.wadl")

# shortcut to generating the url, with attributes:
wd <- waddler("timeseries")
class(wd)
is.iriswadl(wd)
# print some information about the webservice
# and return query parameters:
describe(wd)

# Try another webservice
wd <- waddler("traveltime")
# return a data.frame showing the parameters acceptable
# for a query, and indicating if
# they are required and/or have default values:
print(p <- parameters(wd))
}
\author{
A.J. Barbour
}
\references{
[1] \url{http://www.w3.org/Submission/wadl/}
}
\seealso{
\code{\link{constructor}}, which generates the appropriate url

\code{\link{iris.query}} to query IRIS WS

\code{\link{irisws-package}}

Other Utilities: \code{\link{irisws-package}},
  \code{\link{irisws-queryconstruction}},
  \code{\link{irisws-timeutilities}},
  \code{\link{query.iris}}
}

