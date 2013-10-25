\name{taper}
\alias{taper}
\title{TAPER}
\description{
  Applies a symmetric taper to each side of the SAC records.
}
\usage{
taper(x, type = c("hanning", "hamming", "cosine"),
      width = 0.05)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
  \item{type}{Valid values are: \sQuote{hanning}, \sQuote{hamming}, and
    \sQuote{cosine}.} 
  \item{width}{the width of the taper. Values larger than 0.5 should
    probably never be used.}
}
\value{same as input.
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

