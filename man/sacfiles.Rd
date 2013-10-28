\name{sacfiles}
\alias{.sacreader.asc}
\alias{.sacreader.bin}
\alias{c.saclist}
\alias{fstart}
\alias{fstart.sac}
\alias{fstart.saclist}
\alias{plot.saclist}
\alias{read_sac}
\alias{read.sac}
\alias{sac}
\alias{sacfile}
\alias{sacfiles}
\alias{sacunits}
\alias{sacunits.sac}
\alias{sacunits.saclist}
\alias{sync}
\alias{sync.saclist}
\title{Read a SAC binary file}
\usage{
  read.sac(files, is.binary,
    endianness = c("auto", "little", "big"), ...)

  .sacreader.asc(fi, na.value = c("-12345", "-12345.00"),
    amp.as.ts = TRUE)

  .sacreader.bin(fi, endi, na.value = -12345,
    amp.as.ts = TRUE)

  \method{c}{saclist} (..., recursive = FALSE)

  \method{plot}{saclist} (x, ncol = 1, ...)

  fstart(x, relative = FALSE)

  \method{fstart}{sac} (x)

  \method{fstart}{saclist} (x, relative = FALSE)

  sacunits(x)

  \method{sacunits}{sac} (x)

  \method{sacunits}{saclist} (x)

  sync(x)

  \method{sync}{saclist} (x)
}
\arguments{
  \item{files}{character; the files to read in}

  \item{is.binary}{logical; are the sac files in
  \code{files} binary or ascii?}

  \item{endianness}{character; specify the endianness of
  \code{file}. \code{'auto'} uses the platform value, or
  \code{'little'} and \code{'big'} can be used to force a
  specific structure.}

  \item{fi}{character; the sac-filename}

  \item{endi}{character; the actual endianness of the
  sac-file}

  \item{na.value}{the \code{NA} representation}

  \item{amp.as.ts}{logical; should the amplitudes be
  converted to a \code{'ts'} object?}

  \item{x}{an object with class \code{'sac'} to operate
  on.}

  \item{ncol}{numeric; the number of columns in the plot
  \code{\link{layout}}}

  \item{...}{additional parameters; For
  \code{\link{read.sac}}: additional objects to the sac
  reader; for \code{\link{c.sac}}: the objects to
  concatenate}

  \item{recursive}{logical; From \code{\link{c}}:\emph{ If
  \code{recursive = TRUE}, the function recursively
  descends through lists (and pairlists) combining all
  their elements into a vector. }}

  \item{relative}{logical; should the start times be
  relative to the minimum of the group?}
}
\value{
  A list of lists, with class \code{'saclist'}, where each
  item corresponds to the contents of each entry in
  \code{files}, each with class \code{'sac'}.
}
\description{
  Loads SAC (Seismic Analysis Code) data files [1], stored
  as either ascii or binary format.

  From [2]: \emph{ [SAC] files contain a fixed length
  header section followed by one or two data sections. The
  header contains floating point, integer, logical, and
  character fields. Evenly spaced data files have only one
  data section which contains the dependent variable.
  Unevenly spaced data and spectral data files contain two
  data sections. For unevenly spaced data, the first data
  section contains the dependent variable and the second
  contains the independent variable. For spectral files the
  first component is either the amplitude or the real
  component and the second component is either the phase or
  imaginary component. }
}
\details{
  The ascii reader (\code{\link{.sacreader.asc}}) is simply
  a series of \code{\link{read.table}} calls, and the
  binary reader (\code{\link{.sacreader.bin}}) uses
  \code{\link{readBin}} with the specified endianness.

  \subsection{Utility functions}{ \code{\link{sync}}: From
  documentation in the last available version of
  \code{Rsac}: \emph{ Synchronizes the reference times of
  all files in a vector of SAC files. [...] This is useful
  if you are sending each file to a different plot but want
  the relative time to be consistent between the different
  plots. }

  \code{\link{sacunits}}: From documentation in the last
  available version of \code{Rsac}: \emph{ Looks up the
  units of the [amplitudes in the] SAC record. The units in
  many seismic headers are notoriously unreliable, so care
  should be taken to find an independent source to confirm
  the units. }

  \code{\link{fstart}}: From documentation in the last
  available version of \code{Rsac}: \emph{ Calculates the
  starting time [of the SAC data]. } }
}
\examples{
\dontrun{
##
## SAC Binary reader
##
sacfi <- system.file("sac/elmayorB084.sac", package="irisws")
#   this is a little-endian sac file, so
#   must specify (your system may be 'big'!)
x1 <- read.sac(sacfi, is.binary=TRUE, endianness="little")
#   returns an object of class 'saclist'
plot(x1)
##
## SAC Ascii reader
##
sacascfi <- system.file("sac/elmayorB084.txt", package="irisws")
x2 <- read.sac(sacascfi, is.binary=FALSE)
plot(x2)
all.equal(x1[1]$amp, x2[1]$amp) # they are equal, as expected
#
# Can also load a series of files:
#
sacfis <- rep(sacfi, 3)
x3 <- read.sac(sacfis, is.binary=TRUE, endianness="little")
plot(x3) # now there are three frames in the plot
#
# Utilities
#
c(x1)
sacunits(x1)

}
}
\author{
  A.J. Barbour modified code from the (now defunct) package
  \code{Rsac}, written originally by E.M. Thompson.
}
\references{
  [1] \url{http://www.iris.edu/software/sac/}

  [2] \url{http://www.iris.edu/files/sac-manual/}
}

