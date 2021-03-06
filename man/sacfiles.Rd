% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sac.R
\name{sacfiles}
\alias{.sacreader.asc}
\alias{.sacreader.bin}
\alias{c.saclist}
\alias{fstart}
\alias{fstart.sac}
\alias{fstart.saclist}
\alias{plot.saclist}
\alias{print.sac}
\alias{print.saclist}
\alias{print.summary.saclist}
\alias{read.sac}
\alias{read_sac}
\alias{sac}
\alias{sacfile}
\alias{sacfiles}
\alias{sacunits}
\alias{sacunits.sac}
\alias{sacunits.saclist}
\alias{str.saclist}
\alias{summary.saclist}
\alias{summaryStats}
\alias{summaryStats.sac}
\alias{summaryStats.saclist}
\alias{sync}
\alias{sync.saclist}
\title{Read a SAC binary file}
\usage{
read.sac(files, is.binary, endianness = c("auto", "little", "big"), ...)

.sacreader.asc(fi, na.value = c("-12345", "-12345.00"), amp.as.ts = TRUE)

.sacreader.bin(fi, endianness = c("little", "big"), na.value = -12345,
  amp.as.ts = TRUE)

\method{c}{saclist}(..., recursive = FALSE)

\method{print}{sac}(x, ...)

\method{print}{saclist}(x, ...)

\method{summary}{saclist}(object, ...)

\method{print}{summary.saclist}(x, ...)

\method{str}{saclist}(object, ...)

\method{plot}{saclist}(x, ncol = 1, stat.annotate = TRUE, trim = 0,
  rel.time = NULL, apply.calib = TRUE, ...)

fstart(x, relative = FALSE)

\method{fstart}{sac}(x, relative = NULL)

\method{fstart}{saclist}(x, relative = FALSE)

sacunits(x)

\method{sacunits}{sac}(x)

\method{sacunits}{saclist}(x)

sync(x)

\method{sync}{saclist}(x)

summaryStats(x, trim = 0, rel.time = NULL)

\method{summaryStats}{sac}(x, trim = 0, rel.time = NULL)

\method{summaryStats}{saclist}(x, trim = 0, rel.time = NULL)
}
\arguments{
\item{files}{character; the file(s) to read in}

\item{is.binary}{logical; are the sac files in \code{files} binary or ASCII?}

\item{endianness}{character; specify the endianness of \code{file}.
\code{'auto'} uses the platform value, or \code{'little'} and \code{'big'}
can be used to force a specific structure.}

\item{...}{additional parameters;
For \code{\link{read.sac}}: additional objects to the sac reader; 
for \code{\link{c.saclist}}: the objects to concatenate}

\item{fi}{character; a single filename}

\item{na.value}{the \code{NA} representation}

\item{amp.as.ts}{logical; should the amplitudes be converted to a \code{'ts'} object?}

\item{recursive}{logical; From \code{\link{c}}:\emph{
If \code{recursive = TRUE}, the function recursively descends 
through lists (and pairlists) combining all their elements into a vector.
}}

\item{x}{an object to operate on.}

\item{object}{an object to operate on.}

\item{ncol}{numeric; the number of columns in the plot \code{\link{layout}}}

\item{stat.annotate}{logical; should statistical annotations be shown?}

\item{trim}{numeric; the fraction of data to trim from the start and end
of the amplitude record for the statistical summary. Can be a two-length
vector, but must be within [0,0.5].}

\item{rel.time}{POSIXct; report the number of seconds relative to this}

\item{apply.calib}{logical; should the calibration factor in the sac header be applied?}

\item{relative}{logical; should the start times be relative to
the minimum of the group?}
}
\value{
A list of lists, with class \code{'saclist'}, where each 
item corresponds to the contents of each entry in
 \code{files}, each with class \code{'sac'}.
}
\description{
Loads SAC (Seismic Analysis Code) data files [1], stored as either 
ASCII or binary format.

From [2]:
\emph{
[SAC] files contain a fixed length header section followed by one or 
two data sections. The header contains floating point, integer, logical, 
and character fields. Evenly spaced data files have only one data section
 which contains the dependent variable. Unevenly spaced data and spectral 
 data files contain two data sections. For unevenly spaced data, the first 
 data section contains the dependent variable and the second contains the 
 independent variable. For spectral files the first component is either 
 the amplitude or the real component and the second component is either 
 the phase or imaginary component.
}
}
\details{
The ASCII reader (\code{\link{.sacreader.asc}}) is simply a series 
of \code{\link{read.table}} calls,
and the binary reader (\code{\link{.sacreader.bin}}) uses
\code{\link{readBin}} with the specified endianness.

\subsection{Utility functions}{
\code{\link{sync}}:
From documentation in the last available version of \code{Rsac}:
\emph{
Synchronizes the reference times of all files in a vector of SAC files. 
[...]
This is useful if you are sending each file to a different plot but 
want the relative time to be consistent between the different plots.
}

\code{\link{sacunits}}:
From documentation in the last available version of \code{Rsac}:
\emph{
Looks up the units of the [amplitudes in the] SAC record. The units in many seismic
 headers are notoriously unreliable, so care should be taken to 
 find an independent source to confirm the units.
}

\code{\link{fstart}}:
From documentation in the last available version of \code{Rsac}:
\emph{
Calculates the starting time [of the SAC data].
}
}
}
\examples{
\dontrun{
##
## Methods for .sac data
##
#
# Load pore pressure at B084 during the El Majoy Cucapah earthquake:
#
sacfi <- system.file("sac/elmayorB084_LDD.sac", package="irisws")
#   this is a little-endian sac file, so
#   must specify (your system may be 'big'!)
x1 <- read.sac(sacfi, is.binary=TRUE, endianness="little")
#   returns an object of class 'saclist'
plot(x1)
##
## SAC ASCII reader
##
sacascfi <- system.file("sac/elmayorB084_LDD.txt", package="irisws")
x2 <- read.sac(sacascfi, is.binary=FALSE)
plot(x2)  
all.equal(x1[1]$amp, x2[1]$amp) # they are equal, as expected
#
# Can also load a series of files:
#
sacfis <- rep(sacfi, 3)
x3 <- read.sac(sacfis, is.binary=TRUE, endianness="little")
plot(x3) # now there are three frames in the plot (all the same data, obviously)
#
# Utilities
#
c(x1)
sacunits(x1)

}
}
\author{
A.J. Barbour modified code from the (now defunct)
package \code{Rsac}, written originally by E.M. Thompson.
}
\references{
[1] \url{http://www.iris.edu/software/sac/}

[2] \url{http://www.iris.edu/files/sac-manual/}
}
\seealso{
\code{\link{irisws-package}}
}

