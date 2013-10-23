\name{constructor}
\alias{constructor}
\alias{constructor2}
\alias{params2queryparams}
\title{Constructs a query function}
\usage{
  constructor(..., service = c("timeseries"))

  constructor2(..., service = c("timeseries"),
    list.fields.only = FALSE)

  params2queryparams(..., defaults, exclude.nulls = FALSE)
}
\arguments{
  \item{...}{query parameters. For
  \code{\link{constructor}} these are comma-separated
  character strings (\code{"a=1", "b=2"} etc) For
  \code{\link{constructor2}} these are comma-separated
  arguments (\code{a=1, b=2} etc)}

  \item{service}{character; the type of web service to
  construct for.}

  \item{defaults}{list; the parameters to merge \code{...}
  with}

  \item{list.fields.only}{logical; return names of default
  arguments}
}
\description{
  Constructs a query function
}
\details{
  These functions form correctly formatted queries for IRIS
  WS.

  \code{\link{constructor}} is a basic constructor which
  simply collapses the arguments into a single query.

  \code{\link{constructor2}} also does this, but enforces
  parameter inputs so the query at least has a chance for
  success.

  \code{\link{params2queryparams}} is a function to
  collapse arguments into a single query string
}
\examples{
# Basic construction:
constructor()
constructor("net=1","sta=2")
#
# parameter flattening
params2queryparams(net=1,sta=2)
#
# Constrained construction:
constructor2(net=1,sta=2)
# see what needs to be given:
constructor2(list.fields.only=TRUE)
}
\author{
  AJ Barbour
}
\references{
  \url{http://pages.stern.nyu.edu/~dbackus/BCH/data/FRED/fredplot.R}
  was the motivation for \code{\link{params2queryparams}}.
}
\seealso{
  \code{\link{iris.query}} to query IRIS WS
}

