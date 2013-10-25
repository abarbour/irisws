\name{GET.seis}
\alias{GET.seis}
\alias{testgetseis}
\title{READS VARIOUS SEISMIC FILE FORMATS}
\description{
  This fuction calls C-routines to read in \sQuote{segy}, \sQuote{sac},
  \sQuote{AH} formats.
}
\usage{
GET.seis(fnames, kind = 2)
testgetseis(GG)
}
\arguments{
  \item{fnames}{list of file names.}
  \item{kind}{an integer (1:3); 1 = segy, 2 = sac, 3 = AH.}
  \item{GG}{a list returned by \code{GET.seis}.}
}
\value{a list containing the seismic data and header information.
}
\seealso{\code{\link{rsac}}
}
\author{Original code by Jonathan M. Lees <jonathan.lees@unc.edu> with
  edits by Eric M. Thompson <eric.thompson@tufts.edu>. }


