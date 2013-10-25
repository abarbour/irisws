\name{rtrend}
\alias{rtrend}
\alias{rmean}
\title{REMOVE LINEAR TREND OR MEAN}
\description{
  Removes the linear trend or mean from SAC data.
  Note that defailt of "spect" is to detrend
  because that is the default for
  \code{\link[stats]{spec.pgram}} (see example below). 
}
\usage{
rtrend(x)
rmean(x)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
}
\value{same as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

x <- rsac.skeleton(1)
x[[1]]$amp <- runif(1e3) 
x[[1]]$dt <- 1

plot(x)

# DC offset:
spect(x, detrend = FALSE)

# Default detrends, removing DC:
spect(x)


}

