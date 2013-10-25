\name{spect}
\alias{spect}
\title{SPECTRAL DENSITY ESTIMATION}
\description{
  This is essentially a wrapper function for \code{spectrum} from
  the \pkg{stats} package. Thus, it is important the user be
  familiar with the documentation of the functions listed in the
  \sQuote{See Also} section. This is simply a quick way to compute the
  spectra of \code{"rsac"} class objects.
}
\usage{
spect(x, plot = TRUE, main = NULL, log = "no", ...)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
  \item{plot}{logical for if the spectrum should be plotted.}
  \item{main}{optional character vector for the plots' titles. If not
    provided, the code creates the character vector from the station,
    component, and network in the header files, but these are not always
    reliable. The length of the vector must equal the length of
    \code{x}.}
  \item{log}{See \code{\link[stats]{plot.spec}}.}
  \item{\dots}{further parameters. See \code{\link[stats]{spectrum}} and
    \code{plot.spec}.}
}
\value{a vector of objects, each object is of \code{"rsac"}.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\seealso{\code{\link[stats]{spectrum}}, \code{\link[stats]{plot.spec}},
  \code{\link[stats]{spec.pgram}}, \code{\link[stats]{spec.ar}}}
\examples{

data(srosa)
data(sumatra)

eqs <- c(srosa[1], sumatra[11])

spect(eqs)

spect(eqs, spans = 101)

spect(eqs, spans = 201)

spect(eqs, spans = c(101, 151))

spect(eqs, spans = c(101, 151),
      log = "yes")

getdelta <- function(X){
  delta <- gcp(s = list(lat = X$stla, lon = X$stlo),
               e = list(lat = X$evla, lon = X$evlo))$a}
del <- sapply(X = sumatra, getdelta)

sum <- sumatra[c(1, 3, 4, 9)]

spect(sum, plot = TRUE, spans = c(101, 151),
      log = "no", xlim = c(0, 0.15))

}

