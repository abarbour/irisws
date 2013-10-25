\name{sumatra}
\docType{data}
\alias{sumatra}
\title{EXAMPLE RSAC DATA}
\description{Example of class \code{"rsac"} object  containing 16
  recordings of the 2004 Sumatra-Andaman Earthquake.
}
\usage{
data(sumatra)
}
\format{Vector containing 16 recordings. Each is a LHZ channel at a
  different recording station (L = long period, H = high gain, Z =
  vertical component). 
} 
\source{\url{http://www.iris.edu/}}
\section{Background}{ The 2004 Sumatra-Andaman Earthquake. There is
  extensive literature on this event. I include it here simply as
  an interesting example.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\keyword{datasets}
\examples{

data(sumatra)
getstas <- function(X)
  gsub(" ", "", X$sta)
stas <- sapply(X = sumatra, getstas)
getunits(sumatra)
sumatra <- mul(sumatra, 1e-9)
# Bandpass filter
sumatra <- bp(int(taper(rtrend(sumatra))),
              c = c(0.01, 0.3))
getdelta <- function(X)
{
  delta <- gcp(s = list(lat = X$stla, lon = X$stlo),
               e = list(lat = X$evla, lon = X$evlo))$a
}
del <- sapply(X = sumatra, getdelta)
st <- sapply(sumatra, fstart)
par(las = 1, mar = c(1, 5, 5, 1), xaxs = "i", yaxs = "i",
    cex.main = 1, mfrow = c(1, 1))
plot(1, 1, type = "n", axes = FALSE,
     xlim = c(60, 60 + 350)*60, ylim = c(200, 0),
     xlab = "", ylab = "")

udel <- sort(unique(del))
cols <- (1:length(udel))\%\%6 + 1

for(i in 1:length(sumatra))
{
  x <- 100 * sumatra[[i]]$amp + del[i]
  start <- st[i] + sumatra[[i]]$b
  deltat <- sumatra[[i]]$dt
  time <- seq(from = start, by = deltat, length = length(x))
  lines(time, x, col = cols[del[i] == udel])
}

axis(side = 2, at = seq(0, 180, by = 30))
axis(side = 3, at = (seq(0, 360, by = 60)*60 + 60*60),
     lab = seq(0, 360, by = 60))
title(main = "Time, minutes")
title(ylab = expression(paste(Delta, ", Degrees")))


if(require(maps)){
  par(mar = c(3,7,2,1))
  map(col = grey(0.8))
  for(i in 1:length(stas)){
    GCPATH <- gcp(s = list(lat = sumatra[[i]]$stla, lon = sumatra[[i]]$stlo),
                  e = list(lat = sumatra[[i]]$evla, lon = sumatra[[i]]$evlo)
                  )
    if(any(abs(diff(GCPATH$lon)) > 180)){
      l <- length(GCPATH$lon)
      j <- which(abs(diff(GCPATH$lon)) > 180)
      lines(GCPATH$lon[1:j], GCPATH$lat[1:j], col = "blue")
      lines(GCPATH$lon[(j+1):l], GCPATH$lat[(j+1):l], col = "blue")
    }else{
      lines(GCPATH$lon, GCPATH$lat, col = "blue")
    }
    points(sumatra[[i]]$stlo, sumatra[[i]]$stla, pch = 17)
    text(sumatra[[i]]$stlo, sumatra[[i]]$stla, stas[i], pos = 1)
  }
  points(sumatra[[1]]$evlo, sumatra[[1]]$evla, col = "red", pch = 16)
  axis(side = 1, at = seq(-180, 180, by = 30))
  axis(side = 2, at = seq(-60, 60, by = 30))
  title(main = "Sumatra-Andaman Earthquake",
        xlab = "Longitude", ylab = "Latitude")
}


}
