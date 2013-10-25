\name{fromjul}
\alias{fromjul}
\alias{tojul}
\alias{getmoday}
\title{JULIAN DAY CONVERSION FUNCTIONS}
\description{
Converts to and from Julian days.
}
\usage{
fromjul(jul, yy)
tojul(year, month, day)
getmoday(jul, iyear)
}
\arguments{
  \item{jul}{julian day.}
  \item{yy, year, iyear, month, day}{Calendar year, month, and day.}
}
\value{a list containing the date values. The
  specific elements of the list are:
  \item{mo}{month.}
  \item{dom}{day of month.}
  \item{jul}{Julian day.}
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\keyword{internal}
