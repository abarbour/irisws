\name{rsac.skeleton}
\alias{rsac.skeleton}
\title{EMPTY RSAC OBJECT}
\description{
  Create an empty "Rsac" object. Note that most values are NA by
  default, including "amp" and "dt", which are the minimum that must be
  supplied to be useful. 
}
\usage{
rsac.skeleton(n)
}
\arguments{
  \item{n}{number of elements desired for the empty "rsac" object.}
}
\value{an empty "rsac" object.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

x <- rsac.skeleton(1)
x[[1]]$amp <- runif(1e3, min = -1)
x[[1]]$dt <- 1

plot(x)

x.t <- taper(x)
lines(x.t, col = "blue")

}

