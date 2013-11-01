\name{query.iris}
\alias{check.query}
\alias{iris.query}
\alias{query.iris}
\title{Perform a query to IRIS-WS}
\usage{
  query.iris(iquery, filename = "iris.query.results",
    is.binary = FALSE, check = TRUE, verbose = TRUE, ...)

  iris.query(iquery, filename = "iris.query.results",
    is.binary = FALSE, check = TRUE, verbose = TRUE, ...)

  check.query(iquery)
}
\arguments{
  \item{iquery}{character; the web service query}

  \item{filename}{character; the file to save query results
  to. If this is \code{NULL} a temporary file from
  \code{\link{tempfile}} is used.}

  \item{is.binary}{logical; will the output be binary?
  (e.g., \code{TRUE} for SAC binary, and \code{FALSE} for a
  plot)}

  \item{check}{logical; should \code{\link{check.query}} be
  used to check the quality of \code{iquery}}

  \item{verbose}{logical; should messages be given by this
  function, and \code{\link{curlPerform}}?}

  \item{...}{additional arguments to
  \code{\link{curlPerform}}}
}
\value{
  A list (invisibly) with the filename, the query string,
  and a success flag
}
\description{
  Perform a query to IRIS-WS
}
\details{
  This function uses \code{\link{curlPerform}} in the
  \strong{RCurl} package to query the IRIS WS. Firstly, it
  checks \code{query} for internal consistency, with
  \code{\link{check.query}}; then, \code{query} is checked
  externally with \code{\link{url.exists}}.  If
  \code{query} passes the checks, then it is sent to the
  IRIS WS and successfull results are written to
  \code{filename} (which is instantiated with
  \code{\link{CFILE}}).

  \code{\link{iris.query}} is simply a pointer to
  \code{\link{query.iris}}
}
\examples{
\dontrun{
# This will create ANMO.png
# (duration, or use end=2005.002T00:00:00)
Q <- constructor2(net="IU", sta="ANMO", loc="00", cha="BHZ", starttime="2005.001T00:00:00", duration="1000", output="plot")
query.iris(Q, "ANMO.png")
#
# and this will put it in a temporary file
query.iris(Q, NULL)
#
# this will fail, obviously, unless there's a seismic network
# named 'XXXTHISWILLFAILXXX'
Q <- constructor("net=XXXTHISWILLFAILXXX")
query.iris(Q)
#
# Arbitrary query validation
try(check.query(params2queryparams(a=1, defaults=list(a=TRUE))))  # succeeds
try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=NA)))) # fails
try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=FALSE)))) # succeeds
try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=FALSE),exclude.empty.options=FALSE))) # fails
try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=TRUE)))) # fails
}
}
\author{
  AJ Barbour
}
\seealso{
  \code{\link{irisws-package}}

  Other Utilities: \code{\link{constructor}},
  \code{\link{constructor2}}, \code{\link{irisws-package}},
  \code{\link{irisws-timeutilities}},
  \code{\link{ISOdatetime.j}}, \code{\link{ISOtimestring}},
  \code{\link{params2queryparams}},
  \code{\link{timestring}}
}

