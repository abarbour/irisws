\name{sacfiles}
\alias{.sacreader.asc}
\alias{.sacreader.bin}
\alias{c.saclist}
\alias{fstart}
\alias{fstart.sac}
\alias{plot.saclist}
\alias{read.sac}
\alias{read_sac}
\alias{sac}
\alias{sacfile}
\alias{sacfiles}
\alias{sync}
\alias{sync.saclist}
\alias{units}
\alias{units.saclist}
\title{Read a SAC binary file}
\usage{
  read.sac(files, is.binary,
    endianness = c("auto", "little", "big"), ...)

  .sacreader.asc(fi, na.value = c("-12345", "-12345.00"),
    amp.as.ts = TRUE)

  .sacreader.bin(fi, endi, na.value = -12345,
    amp.as.ts = TRUE)

  \method{c}{saclist} (...)

  \method{plot}{saclist} (x, ncol = 1, ...)

  fstart(x)

  \method{fstart}{sac} (x)

  units(x)

  \method{units}{saclist} (x)

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
  \code{\link{read.sac}}: additional objects to
  \code{\link{.sacreader}}. For \code{c.sac}: roughly
  equivalent to \code{\link{c}}.}

  \item{i}{indices specifying elements to extract or
  replace.}
}
\value{
  A list of lists, with class \code{'saclist'}, where each
  item corresponds to the contents of each entry in
  \code{files}, each with class \code{'sac'}.
}
\description{
  From [2]: \emph{ Each signal is stored on disk in a
  separate SAC data file. These files contain a fixed
  length header section followed by one or two data
  sections. The header contains floating point, integer,
  logical, and character fields. Evenly spaced data files
  have only one data section which contains the dependent
  variable. Unevenly spaced data and spectral data files
  contain two data sections. For unevenly spaced data, the
  first data section contains the dependent variable and
  the second contains the independent variable. For
  spectral files the first component is either the
  amplitude or the real component and the second component
  is either the phase or imaginary component. }
}
\details{
  \code{\link{read.sac}} uses \code{\link{readBin}} to
  parse the binary files with the specified endianness.

  \code{\link{sync}}: From documentation in the last
  version of \code{Rsac}: \emph{ Synchronizes the reference
  times of all files in a vector of SAC files. [...] This
  is useful if you are sending each file to a different
  plot but want the relative time to be consistent between
  the different plots. }

  \code{\link{units}}: From documentation in the last
  version of \code{Rsac}: \emph{ Looks up the units of the
  SAC record. The units in many seismic headers are
  notoriously unreliable, so care should be taken to find
  an independent source to confirm the units. }

  \code{\link{fstart}}: From documentation in the last
  version of \code{Rsac}: \emph{ Calculates the starting
  time [...] }
}
\author{
  AJ Barbour modified code from the (now removed) package
  \code{Rsac}, written by EM Thompson.
}
\references{
  [1] \url{http://www.iris.edu/software/sac/}

  [2] \url{http://www.iris.edu/files/sac-manual/}
}

