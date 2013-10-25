\name{iir}
\alias{iir}
\alias{lp}
\alias{hp}
\alias{bp}
\title{INFINITE IMPULSE RESPONSE FILTERS}
\description{
  Applies an IIR filter to an object of class \code{"rsac"}. 
}
\usage{
iir(x, ord = 3, fl = NA, fh = NA,
    type = c("LP", "HP", "BP", "BR"),
    proto = c("BU", "BE", "C1", "C2"),
    zerophase = TRUE,
    ca = NA, ctr = NA)
lp(x, c, n = 3, proto = "BU", zerophase = TRUE)
hp(x, c, n = 3, proto = "BU", zerophase = TRUE)
bp(x, c, n = 3, proto = "BU", zerophase = TRUE)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
  \item{ord, n}{the number of poles for filter. Must be less than 10.}
  \item{fl, fh}{low/high pass corner frequency.}
  \item{c}{corner frequency/frequencies. Two are required for
    \code{bp}.}
  \item{type}{\sQuote{LP} = lowpass, \sQuote{HP} = highpass, \sQuote{BP}
    = bandpass, \sQuote{BR} = bandreject.}
  \item{proto}{\sQuote{BU} = Butterworth, \sQuote{BE} = Bessel,
    \sQuote{C1} = Chebyshev Type I, \sQuote{C2} = Chebyshev Type II}
  \item{zerophase}{logical; TRUE for zerophase (two-pass) filtering
    should be applied; FALSE for single pass filtering.}
  \item{ca}{Chebyshev stop band attenuation.}
  \item{ctr}{Chebyshev transition bandwidth.}
}
\value{same as input.
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu> and
  Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

# Sine waves plus linear trend:
t <- seq(0, 10, by = 0.02)
x <- sin(2 * pi * 1 * t) + sin(2 * pi * 5 * t) + 0.2 * t
sines <- vector(mode = "list")
sines[[1]] <- list(amp = x, dt = 0.02, N = length(x), nzhour = 0,
                   nzmin = 0, nzsec = 0, nzmsec = 0, b = 0)
class(sines) <- "rsac"
plot(sines)
f1 <- iir(sines, fl = 0.5, fh = 1.5, type = "BP", proto = "BU")
lines(f1, col = "blue")
f2 <- iir(sines, fl = 0.5, fh = 1.5, type = "BP", proto = "BU",
          zerophase = FALSE)
lines(f2, col = "red")

# Real data
data(srosa)
plot(srosa)
spect(srosa, log = "no")
testf <- lp(srosa, c = 2)
spect(testf, log = "no")
testf <- bp(srosa, c = c(2, 4))
spect(testf, log = "no")

}

