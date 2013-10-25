"gcp" <- function(s, e)
{
  # Method and notation from Lay and Wallace (1995)
  # Convert to radians:
  s$lat <- s$lat * pi/180
  s$lon <- s$lon * pi/180
  e$lat <- e$lat * pi/180
  e$lon <- e$lon * pi/180
  A <- e$lon - s$lon
  b <- pi/2 - e$lat # co-lat
  c <- pi/2 - s$lat # co-lat
  a <- acos(cos(b) * cos(c) + sin(b) * sin(c) * cos(A))
  # Azimuth:
  C <- acos((cos(c) - cos(a) * cos(b))/(sin(a) * sin(b)))
  # Backazimuth:
  B <- acos((cos(b) - cos(a) * cos(c))/(sin(a) * sin(c)))
  # See if 
  if(e$lon - s$lon > 0)
  {
    if(abs(e$lon - s$lon) > pi) # s is right of e
      B <- 2*pi - B
    else  # s is left of e
      C <- 2*pi - C
  }else
  {
    if(abs(e$lon - s$lon) > pi) # s is left of e
      C <- 2*pi - C
    else  # s is right of e
      B <- 2*pi - B
  }
  
  # Now calculate the lats/lons of the path
  #    for display purposes
  Rearth <- 6372795 # for m
#  Rearth <- 6272.75
  n <- 1000
  dist <- a * Rearth
  Dd <- seq(from = 0, to = dist, length = n) / Rearth
  Cc <- rep(C, n)
  lata <- e$lat
  lona <- e$lon
  latb <- asin(cos(Cc) * cos(lata) * sin(Dd) +
               sin(lata) * cos(Dd))
  dlon <- atan2(cos(Dd) - sin(lata) * sin(latb),
                sin(Cc) * sin(Dd) * cos(lata))
  lonb <- lona - dlon + pi/2
  # - - - - - - - #
  lonb[lonb >  pi] <- lonb[lonb >  pi] - 2 * pi
  lonb[lonb < -pi] <- lonb[lonb < -pi] + 2 * pi
  # - - - - - - - #
  C <- C * 180/pi
  B <- B * 180/pi
  latb <- latb * 180/pi
  lonb <- lonb * 180/pi
  a <- a * 180/pi
  return(list(B = B, C = C, a = a, lon = lonb, lat = latb))
}
