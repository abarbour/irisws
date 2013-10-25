"section" <-
  function(x, dist = NULL, vertdist = TRUE, ampnorm = TRUE, mfactor = 1,
           xlim = NULL, tracelabs = NULL, lty = NULL, col = NULL,
           timeStyle = "axis", offsets = NULL)
{
  # Some useful functions
  getevla <- function(X) X$evla
  getevlo <- function(X) X$evlo
  getstla <- function(X) X$stla
  getstlo <- function(X) X$stlo
  getmax <- function(X) max(abs(X$amp))
  
  timeStyle <- match.arg(timeStyle, c("axis", "bar"))
  # Test for needed values
  if(!is.null(dist))
  { # dist vector is provided
    if(length(dist) != length(x))
      stop("\'length(dist)\' must equal \'length(x)\'.\n")
  }else{ # dist vector is NOT provided
    # Try to use getdelta
    evll <- cbind(sapply(x, getevlo), sapply(x, getevla) )
    stll <- cbind(sapply(x, getstlo), sapply(x, getstla) )
    if(any(is.na(rbind(evll, stll)) ))
      stop("\'dist\' not provided and cannot calculate from headers.\n")
    else{
      dist <- numeric(length(x))
      for(i in 1:length(x)){
        dist[i] <- gcp(s = list(lat = stll[i , 2], lon = stll[i , 1]),
                       e = list(lat = evll[i , 2], lon = evll[i , 1]))$a
      }
    }
  }
  if(!is.null(tracelabs))
    if(length(tracelabs) != length(x))
      stop("\'length(dist)\' must equal \'length(x)\'.\n")

#   x <- sync(x )
  maxes <- sapply(x, getmax )
  if(is.null(xlim ) )
  {
    st <- sapply(x, fstart)
    getdt <- function(X) X$dt
    getn <- function(X) X$N
    ns <- sapply(x, getn)
    dts <- sapply(x, getdt)
    Len <- ns * dts
    st <- st - min(st)
    End <- st + Len
    xlim <- c(0, max(End))
  }
  doffset <- dist/max(dist)
  numfac <- 1/length(x)/2
  if(all(is.null(col)))
    col <- rep(1, length(x))
  else{
    if(length(col) != length(x)){
      if(length(col) == 1)
        col <- rep(col, length(x))
      else
        stop("length of \'col\' does not equal length of \'x\'.\n")
    }
  }
  if(all(is.null(lty)))
    lty <- rep(1, length(x))
  else{
    if(length(lty) != length(x)){
      if(length(lty == 1))
        lty <- rep(lty, length(x))
      else
        stop("length of \'lty\' does not equal length of \'x\'.\n")  
    }
  }
  
  if(ampnorm)
    ampfac <- 1/maxes
  else
    ampfac <- rep(1/max(maxes), length(x))
  
  ###################################
  # Create plot
  if(vertdist == TRUE)
  {
    plot(1, type = "n", axes = FALSE, ylab = "", xlab = "",
         xlim = xlim, ylim = c(1+numfac, -numfac))
    for(i in 1:length(x))
    {
      Time <- seq(from = st[i] + x[[i]]$b, by = dts[i],
                  len = length(x[[i]]$amp))
      lines(Time,x[[i]]$amp*ampfac[i]*numfac*mfactor+doffset[i],
            lty = lty[i], col = col[i])
    }
  }
  # Label distance on y-axis
  dcf <- 1/max(dist)
  ylabs <- pretty(c(0, 1/dcf))
  axis(side = 2, at = ylabs*dcf, lab = ylabs)
  # Add time labels
  tlab <- pretty(xlim)[1:2]           # seconds
  if(tlab[2] > 50){
    tlab <- pretty(xlim/60)[1:2]      # minutes
    if(tlab[2] > 50){
      if(timeStyle == "bar"){
        tlab <- pretty(xlim/3600)[1:2] # hours
        segments(diff(tlab)*3600 + tlab[1]*3600, 0,
                 diff(tlab)*3600 + tlab[2]*3600, 0, lwd = 2)
        text(diff(tlab)*3600 + mean(tlab*3600), 0,
             paste(diff(tlab), "hours"), pos = 1)
      }else{
        axis(side = 3, at = 3600*pretty(xlim/3600),
             lab = pretty(xlim/3600))
      }
    }else{
      if(timeStyle == "bar"){
        segments(diff(tlab)*60 + tlab[1]*60, 0,
                 diff(tlab)*60 + tlab[2]*60, 0, lwd = 2)
        text(diff(tlab)*60 + mean(tlab*60), 0,
             paste(diff(tlab), "minutes"), pos = 1)
      }else{
        axis(side = 3, at = 60*pretty(xlim/60),
             lab = pretty(xlim/60))
      }
    }
  }else{
    if(timeStyle == "bar"){
      segments(diff(tlab) + tlab[1], 0,
               diff(tlab) + tlab[2], 0, lwd = 2)
      text(diff(tlab) + mean(tlab), 0, paste(diff(tlab), "seconds"), pos = 3)
    }else{
      axis(side = 3)
    }
  }
}











