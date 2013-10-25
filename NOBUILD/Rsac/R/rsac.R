"rsac" <- function(files, endian = .Platform$endian)
{
  if(length(endian) == 1 & length(files) > 1)
    endian <- rep(endian, length(files))
  n <- length(files)
  data <- vector(mode = "list", length = n)
  for(i in 1:n)
  {
    file <- files[i]
    zz <- file(file, "rb")
    h1 <- readBin(con = zz, what = numeric(), n = 70, size = 4,
                  endian = endian[i])
    dim(h1) <- c(5, 14)
    h1 <- aperm(h1)
    # NA values:
    h1[h1 == -12345] <- NA
    h2 <- readBin(con = zz, what = integer(), n = 35, size = 4,
                  endian = endian[i])
    dim(h2) <- c(5, 7)
    h2 <- aperm(h2)
    # NA values:
    h2[h2 == -12345] <- NA
    h3 <- readBin(con = zz, what = logical(), n = 5, size = 4,
                  endian = endian[i])
    h4 <- readBin(con = zz, what = character(), n = 1, size = 4,
                  endian = endian[i])
    # Define header variables:
    dt <- h1[1, 1]
    depmin <- h1[1, 2]
    depmax <- h1[1, 3]
    scale <- h1[1, 4]
    odelta <- h1[1, 5]
    b <- h1[2, 1]
    e <- h1[2, 2]
    o <- h1[2, 3]
    a <- h1[2, 4]
    f <- h1[5, 1]
    stla <- h1[7, 2]
    stlo <- h1[7, 3]
    stel <- h1[7, 4]
    stdp <- h1[7, 5]
    evla <- h1[8, 1]
    evlo <- h1[8, 2]
    evel <- h1[8, 3]
    evdp <- h1[8, 4]
    mag <- h1[8, 5]
    dist <- h1[11, 1]
    az <- h1[11, 2]
    baz <- h1[11, 3]
    gcarc <- h1[11, 4]
    cmpaz <- h1[12, 3]
    cmpinc <- h1[12, 4]
    nzyear <- h2[1, 1]
    nzjday <- h2[1, 2]
    nzhour <- h2[1, 3]
    nzmin <- h2[1, 4]
    nzsec <- h2[1, 5]
    nzmsec <- h2[2, 1]
    norid <- h2[2, 3]
    nevid <- h2[2, 4]
    N <- h2[2, 5]
    idep <- h2[4, 2]
    iztype <- h2[4, 3]
    leven <- h3[1]
    lpspol <- h3[2]
    kstnm <- substr(h4, 1, 8)
    kstnm <- sub("-12345", "      ", kstnm)
    kevnm <- substr(h4, 9, 24)
    kevnm <- sub("-12345", "      ", kevnm)
    khole <- substr(h4, 25, 32)
    khole <- sub("-12345", "      ", khole)
    ko <- substr(h4, 33, 40)
    ko <- sub("-12345", "      ", ko)
    ka <- substr(h4, 41, 48)
    ka <- sub("-12345", "      ", ka)
    kcmpnm <- substr(h4, 161, 168)
    kcmpnm <- sub("-12345", "      ", kcmpnm)
    knetwork <- substr(h4, 169, 176)
    knetwork <- sub("-12345", "      ", knetwork)
    kinst <- substr(h4, 185, 192)
    kinst <- sub("-12345", "      ", kinst)
    seek(con = zz, where = 632)
    x <- readBin(con = zz, what = numeric(), n = N,
                 size = 4, endian = endian[i])
    close(zz)
    data[[i]] <- list(amp = x, dt = dt, depmin = depmin, depmax = depmax,
                      scale = scale, odelta = odelta,
                      b = b, e = e, o = o, a = a, f = f,
                      stla = stla, stlo = stlo, stel = stel, stdp = stdp,
                      evla = evla, evlo = evlo, evel = evel, evdp = evdp,
                      mag = mag, dist = dist, az = az, baz = baz, gcarc = gcarc,
                      cmpaz = cmpaz, cmpinc = cmpinc,
                      nzyear = nzyear, nzjday = nzjday, nzhour = nzhour,
                      nzmin = nzmin, nzsec = nzsec,
                      nzmsec = nzmsec, norid = norid,
                      nevid = nevid, N = N,
                      units = idep, iztype = iztype,
                      leven = leven, lpspol = lpspol,
                      sta = kstnm, kevnm = kevnm, khole = khole,
                      ko = ko, ka = ka,
                      comp = kcmpnm, knetwork = knetwork, kinst = kinst)
  }
  class(data) <- "rsac"
  invisible(data)
}

