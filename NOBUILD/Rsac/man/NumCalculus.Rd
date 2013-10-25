\name{Calculus}
\alias{dif}
\alias{int}
\title{DIFFERENTIATION AND INTEGRATION}
\description{
  Differentiation (by two-point differencing) and integration (by
  rectangular approximation) in the time domain. For differentiation,
  the begining time of the time series is increased by half the sampling
  interval, and the length of the record is decreased by 1. It is also
  usually a good idea to use the \code{\link{rtrend}} or
  \code{\link{rmean}} function before integration.
}
\usage{
dif(s)
int(s)
}
\arguments{
  \item{s}{an object of class \code{"rsac"}.}
}
\value{same as input.
}
\seealso{\code{\link{rtrend}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)
getunits(srosa)
# convert to cm/s/s
srosa.accel <- dif(mul(srosa, 1e-7))

amax <-  function(X)
  X$x <- max(abs(X$amp))

pga <- max(sapply(X = srosa.accel, amax))
# cm/s/s

spect(srosa)

srosa.disp <- int(rtrend(srosa))

plot(srosa, xlim = c(100, 400))
plot(srosa.disp, xlim = c(100, 400))
srosa.disp <- hp(srosa.disp, c = 0.1)
plot(srosa.disp, xlim = c(100, 400))
spect(srosa.disp)
}

