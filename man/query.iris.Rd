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
  to.  If this is \code{NULL} a temporary file from
  \code{\link{tempfile}} is used.}

  \item{is.binary}{logical; will the output be binary?
  (e.g., \code{TRUE} for SAC binary, and \code{FALSE} for a
  plot)}

  \item{check}{logical; should \code{\link{check.query}} be
  used to check the quality of \code{iquery}}

  \item{verbose}{logical; should messages be given?}

  \item{...}{additional arguments to
  \code{\link{curlPerform}}}
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
}
}
\author{
  AJ Barbour
}

