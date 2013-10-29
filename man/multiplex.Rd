\name{multiplex}
\alias{multiplex}
\title{Multiplex two numeric vectors}
\usage{
  multiplex(x, y)
}
\arguments{
  \item{x}{numeric; the initial vector}

  \item{y}{numeric; the secondary vector to interleave
  within x}
}
\description{
  This is a very simplistic way to multiplex (by two) a
  pair of vectors, merging them into a single vector
}
\details{
  The function uses \code{\link{paste}}

  The results will be x[1], y[1], x[2], y[2], ... and so
  on.

  Values are recycled if the lengths are uneven.

  \code{NA} values are introduced through coercion, and
  \code{NULL} values are ignored
}
\examples{
multiplex(1,c(0,2,4))
multiplex(1:3,4:6)
multiplex(c(1,3,NA,7,NULL),c(2,4,6,8,10))
multiplex(c(1,3,"B"),c(2,4,"A"))
}
\author{
  AJ Barbour
}

