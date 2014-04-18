\name{irisws-timeutilities}
\alias{ISOdatetime.j}
\alias{ISOtimestring}
\alias{irisws-timeutilities}
\alias{timestring}
\title{Utilities to deal with time}
\usage{
timestring(year, day, hour = 0, min = 0, sec = 0, tz = "UTC",
  month = NULL)

ISOtimestring(tstr, tz = "UTC")

ISOdatetime.j(year, day, hour = 0, min = 0, sec = 0, tz = "UTC",
  month = NULL, tstr = NULL)
}
\arguments{
  \item{year}{numeric; the \emph{full} year A.D. (e.g.,
  2012 \emph{not} 12)}

  \item{day}{numeric; the day, either of-the-year, or
  of-the-month (see \code{month})}

  \item{hour}{numeric; the hour of the day (less than 100)}

  \item{min}{numeric; the minute (less than 100)}

  \item{sec}{numeric; fractional seconds (less than 100;
  will be truncated to 6 decimal places)}

  \item{month}{numeric; the month of the year. If this is
  \code{NULL} then \code{day} is assumed to be the Julian
  day of year.}
}
\description{
\code{\link{timestring}} produces properly formatted
strings for the time-fields of, say,
\code{\link{query.iris}}, acceptable to IRIS WS.

\code{\link{ISOdatetime.j}} produces a \code{POSIXct}
object using the Julian date (day of year), or from an
appropriately formatted character string in \code{tstr} (as
produced by \code{\link{timestring}}).
}
\details{
An IRIS WS time-string can be formatted in two ways: (1a)
using year-month-day, (e.g.,
\code{'1997-01-31T12:04:32.123'}) or (2) using year-day
(e.g.,\code{'1997.031T12:04:32.123'}), where the string
after 'T' corresponds to hour:minute:fractional-seconds.
The string can also be of the form (1b) \code{'1997-01-31'}
(in this case \code{'00:00:00'} is assumed), but we have
found this format can lead to query failures.
\emph{\strong{ In this program the string is always
returned in format (2):
\code{<year>.<day>T<hour>:<min>:<sec>} }}

\code{sec} may be fractional, but is formatted with
\code{\link{sprintf}} (\code{02.06f}) so values less than 1
microsecond will be truncated (\emph{not} rounded).

Note that IRIS WS accepts values for hour, minute, and
second which are less than 100.  Return data will have
times adjusted to account for values in excess of the
normal limits (i.e., 24, 60, 60).

The functionality of \code{\link{ISOdatetime.j}} is roughly
equivalent to \code{\link{ISOdatetime}}, but is somewhat
more flexible.
}
\examples{
\dontrun{
#
# Specify the month
timestring(2012, 15, 32, 12, 12.222, month=12)
# [1] "2012.350T32:12:12.222000"
#
# or not
timestring(2012, 15, 32, 12, 12.222)
# [1] "2012.015T32:12:12.222000"
#
# some errors:
try(timestring(2012, 15, 32, 100, 12.222)) # min too large
try(timestring(2012, 75755, 32, 12, 12.222)) # day too large
try(timestring(2012, 15, 32, 100, 12.222, 13)) # month too large
# etc...
#
ISOdatetime.j(tstr=timestring(2010,111,sec=0.12123))
# or use the wrapper
ISOtimestring(timestring(2010,111,sec=0.12123))
}
}
\author{
AJ Barbour
}
\references{
[1] \url{http://service.iris.edu/irisws/timeseries/1/}
}
\seealso{
\code{\link{irisws-package}}

Other Utilities: \code{\link{check.query}},
\code{\link{iris.query}}, \code{\link{query.iris}};
\code{\link{constructor}}, \code{\link{constructor2}},
\code{\link{constructor2.default}},
\code{\link{irisws-queryconstruction}},
\code{\link{params2queryparams}}; \code{\link{describe}},
\code{\link{describe.iriswadl}}, \code{\link{irisws-wadl}},
\code{\link{is.iriswadl}}, \code{\link{parameters}},
\code{\link{parameters.iriswadl}}, \code{\link{waddler}},
\code{\link{wadl}}; \code{\link{irisws}},
\code{\link{irisws-package}}
}

