\name{plot.rsac}
\alias{plot.rsac}
\alias{lines.rsac}
\alias{[.rsac}
\alias{c.rsac}
\alias{summary.rsac}
\title{METHODS FOR CLASS 'rsac'}
\description{
  Plotting, indexing, and summary methods for objects of class
  \code{"rsac"}.
}
\usage{
\method{plot}{rsac}(x, xlim = NULL, ylim = NULL, xlab = NULL,
    ylab = NULL, axes = TRUE, lty = NULL, col = NULL,
    onerow = FALSE, ...)
\method{lines}{rsac}(x, ...)
\method{[}{rsac}(x, i)
\method{c}{rsac}(...)
\method{summary}{rsac}(object, ...)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
  \item{xlim, ylim, xlab, ylab, axes, lty, col}{see
\code{\link[graphics]{plot.default}}.} 
  \item{onerow}{logical; \code{FALSE}: each trace is plotted on a
separate row (separate y-axes); \code{TRUE}: all traces are plotted on
the same row (same y-axis) and lty/col are automatically incremented for
each trace if not specified.}
  \item{i}{indices.}
  \item{object}{an object of class \code{"rsac"}.}
  \item{\dots}{further arguments.}
}
\value{none.
}
\seealso{\code{\link{spect}}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)

plot(srosa, xlim = c(150, 200))

plot(srosa, onerow = TRUE)

plot(srosa)
plot(srosa, xlim = c(140, 280))

getunits(srosa)

# See ?sync for example of how the time axis
# can be misleading due to how t0 is defined.

summary(srosa)

plot(srosa[2])

}

