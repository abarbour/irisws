"rotate" <- function(s, gcp = FALSE, phi = NULL)
{
  # Only accepts list of length 2:
  if(length(s) != 2)
    stop("Only accepts exactly 2 files.")
  # Must have same station name:
  getnms <- function(X)
    X$sta
  stnms <- sapply(X = s, getnms)
  if(stnms[1] != stnms[2])
    stop("Componenets must be from the same station (sta).")
  # Must have the same sampling rate (but SAC has single precision):
  getdt <- function(X)
    X$dt
  deltas <- sapply(X = s, getdt) # SAC uses single precision
  if(diff(range(deltas)) > 1e-7)
    stop("Components must have the same sampling rate (dt).")
  # Must be horizontal components:
  getincs <- function(X)
    X$cmpinc
  incs <- sapply(X = s, getincs)
  if(!all(incs == 90))
    stop("Componenets must both be horizontal (cmpinc = 90).")
  # Must be orthogonal:
  getaz <- function(X)
    X$cmpaz
  az <- sapply(X = s, getaz) # This is in degrees
  if(abs(diff(az)) - 90 >= 1)
    stop("Components are not an orthogonal pair.")
  # Cut data to times with both components
  s <- sync(s)
  st <- sapply(X = s, fstart)
  getb <- function(X)
    X$b
  bs <- sapply(X = s, getb)
  getnpts <- function(X)
    X$N
  npts <- sapply(X = s, getnpts)
  num2start <- round((max(bs) - bs)/deltas[1], 0) #+ 1
  adjnpts <- min(npts - num2start)
  if(adjnpts < 1)
    stop("Components do not overlap in time.")
  if(gcp)
  {
    # Need stla, stlo, evla, evlo to calc back azimuth
    stla <- NA; stlo <- NA; evla <- NA; evlo <- NA
    if(is.na(s[[1]]$stla)) stla <- s[[2]]$stla
    if(is.na(stla)) stla <- s[[1]]$stla
    if(is.na(s[[1]]$stlo)) stlo <- s[[2]]$stlo
    if(is.na(stlo)) stlo <- s[[1]]$stlo
    
    if(is.na(s[[1]]$evla)) evla <- s[[2]]$evla
    if(is.na(evla)) evla <- s[[1]]$evla
    if(is.na(s[[1]]$evlo)) evlo <- s[[2]]$evlo
    if(is.na(evlo)) evlo <- s[[1]]$evlo
    
    if(is.na(stla))
      stop("stla not provided.")
    if(is.na(stlo))
      stop("stlo not provided.")
    if(is.na(evla))
      stop("evla not provided.")
    if(is.na(evlo))
      stop("evlo not provided.")
    # I THINK that there should only be 0 and 90, but it is possible
    # that there will be other azimuths (ie -90), so this might not always
    # work correctly.
    order <- sort.int(az, index.return = TRUE)$ix
    s <- s[order]
    az <- az[order]
    num2start <- num2start[order]
    # Get the back azimuth
    B <- gcp(s = list(lat = stla, lon = stlo),
             e = list(lat = evla, lon = evlo))$B * pi/180
    # 2D Rotation matrix:
    #   The sign of B is switched to be consistent with azimuth def
    #   the rotation matrix is for rotation the coordinate system
    Rmat <- rbind(c( cos(-B), sin(-B)),
                  c(-sin(-B), cos(-B)))
    X1 <- s[[1]]$amp[seq(num2start[1], by = 1, length = adjnpts)]
    X2 <- s[[2]]$amp[seq(num2start[2], by = 1, length = adjnpts)]
    #          NS  EW
    V <- cbind(X1, X2)
    Vrot <- t(Rmat %*% t(V))
    # Radial component
    s[[1]]$amp <- - Vrot[ , 1]
    # Tangential component
    s[[2]]$amp <- - Vrot[ , 2]
  }else
  {
    if(is.null(phi))
      stop("If gcp == FALSE, must provide phi.")
    phi <- phi * pi/180
    Rmat <- rbind(c( cos(phi), -sin(phi)),
                  c( sin(phi),  cos(phi)))
    X1 <- s[[1]]$amp[seq(num2start[1], by = 1, length = adjnpts)]
    X2 <- s[[2]]$amp[seq(num2start[2], by = 1, length = adjnpts)]
    V <- cbind(X1, X2)
    Vrot <- t(Rmat %*% t(V))
    s[[1]]$amp <- Vrot[ , 1]
    s[[2]]$amp <- Vrot[ , 2]
  }
  s[[1]]$b <- max(bs); s[[2]]$b <- max(bs)
  class(s) <- "rsac"
  return(s)
}
