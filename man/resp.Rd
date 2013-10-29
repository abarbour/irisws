\name{resp}
\alias{resp}
\alias{resp.ws}
\alias{ws.resp}
\title{Access the 'resp' web service for instrument response information}
\usage{
  ws.resp(network, station, location, channel, time = "",
    starttime = "", endtime = "", filename = NA,
    verbose = FALSE)

  resp.ws(network, station, location, channel, time = "",
    starttime = "", endtime = "", filename = NA,
    verbose = FALSE)
}
\arguments{
  \item{network}{character;}

  \item{station}{charcter;}

  \item{location}{character;}

  \item{channel}{character;}

  \item{time}{character;}

  \item{starttime}{character;}

  \item{endtime}{character;}

  \item{output}{character;}

  \item{verbose}{logical;}
}
\description{
  Access the 'resp' web service for instrument response
  information
}
\examples{
ws.resp("PB","B084","--","LDD")
try(ws.resp("PB","B084","--","LDD", time=timestring(2012,10,0,0,0)))
try(ws.resp("PB","B084","--","LDD", starttime=timestring(2012,10,0,0,0)))
}

