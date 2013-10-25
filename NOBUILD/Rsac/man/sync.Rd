\name{sync}
\alias{sync}
\title{SYNCHRONIZE}
\description{
  Synchronizes the reference times of all files in a vector of SAC
  files. This is done automatically in \code{\link{plot.rsac}}.
  This is useful if you are sending each file to a different plot but
  want the relative time to be consistent between the different plots.
}
\usage{
sync(x)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
}
\value{same as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\seealso{\code{\link{plot.rsac}}}
\examples{

data(srosa)

# Misleading, because t0 is different for each 
# trace:
par(mfrow = c(3, 1), mar = c(3,4,1,1))
xlim = c(0, 400)
plot(srosa[1], axes = FALSE, ylab = "z",
     xlab = "", xlim = xlim)
axis(side = 2); axis(side = 1)
plot(srosa[2], axes = FALSE, ylab = "North-South",
     xlab = "", xlim = xlim)
axis(side = 2); axis(side = 1)
plot(srosa[3], axes = FALSE, ylab = "East-West",
     xlab = "Time", xlim = xlim)
axis(side = 2); axis(side = 1)

# Better:
srosa <- sync(srosa)
par(mfrow = c(3, 1), mar = c(3,4,1,1))
xlim = c(0, 400)
plot(srosa[1], axes = FALSE, ylab = "z",
     xlab = "", xlim = xlim)
axis(side = 2)
plot(srosa[2], axes = FALSE, ylab = "North-South",
     xlab = "", xlim = xlim)
axis(side = 2)
plot(srosa[3], axes = FALSE, ylab = "East-West",
     xlab = "Time", xlim = xlim)
axis(side = 2); axis(side = 1)

}

