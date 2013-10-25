######################################################
"plot.rsac" <-
  function(x, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL,
           axes = TRUE, lty = NULL, col = NULL, onerow = FALSE, ...)
{
  maxes <- sapply(x, maxx)
  mins <- sapply(x, minx)
  st <- sapply(x, fstart)
  st <- st - min(st)
  if(is.null(xlab))
    xlab <- "Time"
  if(is.null(ylab))
    ylab <- "x"
  n <- length(x)
  if(length(x) == 1)
    onerow <- TRUE
  else
    x <- sync(x)
  if(!onerow)
  {
    par(mfrow = c(n, 1), mar = c(4, 4, 1, 1))
    for(i in 1:n)
    {
      if(is.null(ylim))
        ylim <- range(x[[i]]$amp)
      plot.rsac(x[i], xlim = xlim, ylim = ylim, xlab = xlab,
                ylab = ylab, axes = FALSE, lty = lty, col = col,
                onerow = TRUE, ...)
      ylim <- NULL
      if(i == n)
        title(xlab = "Time")
      if(axes)
      {
        axis(side = 2)
        axis(side = 1)
      }
    }
  }else{
    if(is.null(col))
      col <- rep((1:6), ceiling(n/6))[1:n]
    if(is.null(lty))
      lty <- gl(length = n, n = ceiling(n/6), k = 6)
    for(i in 1:n)
    {
      if(is.null(ylim))
        ylim <- range(x[[i]]$amp)
      xx <- x[[i]]$amp
      start <- st[i] + x[[i]]$b; deltat <- x[[i]]$dt
      time <- seq(from = start, by = deltat, length = length(xx))
      if(i == 1)
      {
        plot(time, xx, type = "l",  xlim = xlim, ylim = ylim,
             xlab = xlab, ylab = ylab, axes = FALSE,
             col = col, ...)
      }else
        lines(time, xx, lty = as.numeric(lty[i]), col = col[i])
      if(axes)
      {
        axis(side = 2)
        axis(side = 1)
      }
    }
  }
}

######################################################
# Some junk:
maxx <- function(X)
  max(X$amp)
minx <- function(X)
  min(X$amp)
fstart <- function(X)
  X$nzhour * 60 * 60 + X$nzmin * 60 + X$nzsec + X$nzmsec * 1e-3

# Need to define indexing for Rsac so that it retains the rsac
# class to plotting will work correctly:
"[.rsac" <- function(x, i)
{
  x <- unclass(x)
  x <- x[i]
  class(x) <- "rsac"
  return(x)
}

######################################################
"c.rsac" <- function(...)
{
  x <- c(unlist(unclass(list(...)), recursive = FALSE))
  class(x) <- "rsac"
  return(x)
}

######################################################
"summary.rsac" <- function(object, ...)
{
  n <- length(object)
  npts <- sapply(object, function(X) X$N)
  dt <- sapply(object, function(X) X$dt)
  comp <- sapply(object, function(X) X$comp)
  sta <- sapply(object, function(X) X$sta)
  units <- getunits(object)
  print(data.frame(npts, dt, comp, sta, units))
}

######################################################
"lines.rsac" <- function(x, ...)
{
  st <- sapply(x, fstart)
  st <- st - min(st)
  for(i in 1:length(x))
  {
    xx <- x[[i]]$amp
    start <- st[i] + x[[i]]$b; deltat <- x[[i]]$dt
    time <- seq(from = start, by = deltat, length = length(xx))
    lines(time, x[[i]]$amp, ...)
  }
}


