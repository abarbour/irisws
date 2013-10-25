#' Read a SAC binary file
#' @description From [2]:
#' \emph{
#' Each signal is stored on disk in a separate SAC data file. 
#' These files contain a fixed length header section followed by one or 
#' two data sections. The header contains floating point, integer, logical, 
#' and character fields. Evenly spaced data files have only one data section
#'  which contains the dependent variable. Unevenly spaced data and spectral 
#'  data files contain two data sections. For unevenly spaced data, the first 
#'  data section contains the dependent variable and the second contains the 
#'  independent variable. For spectral files the first component is either 
#'  the amplitude or the real component and the second component is either 
#'  the phase or imaginary component.
#' }
#' @details 
#' \code{\link{read.sac}} uses
#' \code{\link{readBin}} to parse the binary files 
#' with the specified endianness.
#' 
#' \code{\link{sync}}:
#' From documentation in the last version of \code{Rsac}:
#' \emph{
#' Synchronizes the reference times of all files in a vector of SAC files. 
#' [...]
#' This is useful if you are sending each file to a different plot but 
#' want the relative time to be consistent between the different plots.
#' }
#' 
#' \code{\link{units}}:
#' From documentation in the last version of \code{Rsac}:
#' \emph{
#' Looks up the units of the SAC record. The units in many seismic
#'  headers are notoriously unreliable, so care should be taken to 
#'  find an independent source to confirm the units.
#' }
#' 
#' \code{\link{fstart}}:
#' From documentation in the last version of \code{Rsac}:
#' \emph{
#' Calculates the starting time [...]
#' }
#' 
#' @name sacfiles
#' @aliases sacfile sac read_sac
#' @author AJ Barbour modified code from the (now removed)
#' package \code{Rsac}, written by EM Thompson.
#' 
#' @param files character; the files to read in
#' @param endianness character; specify the endianness of \code{file}.
#' \code{'auto'} uses the platform value, or \code{'little'} and \code{'big'}
#' can be used to force a specific structure.
#' @param fi character;
#' @param endi character;
#' @param x an object with class \code{'sac'} to operate on.
#' @param ... additional objects to XXX
#' @param i indices specifying elements to extract or replace
#' 
#' @return A list of lists, with class \code{'saclist'}, where each 
#' item corresponds to the contents of each entry in
#'  \code{files}, each with class \code{'sac'}.
#' 
#' @references [1] \url{http://www.iris.edu/software/sac/}
#' @references [2] \url{http://www.iris.edu/files/sac-manual/}
NULL

#' @rdname sacfiles
#' @export
read.sac <- function(files, endianness = c("auto","little","big")){
    sacfiles <- as.character(files)
    nsacfi <- length(sacfiles)
    #
    endian <- match.arg(endianness)
    if (endian=="auto"){
        endian <- .Platform[["endian"]]
    }
    #
    sacdat <- vector(mode="list", length=nsacfi)
    for (fi in seq_len(nsacfi)){
        sfi <- sacfiles[fi]
        sacdat[[fi]] <- .sacreader(sfi, endi=endian)
    }
    class(sacdat) <- "saclist"
    return(sacdat)
}

#' @rdname sacfiles
#' @export
.sacreader <- function(fi, endi){
    zz <- file(fi, "rb")
    h1 <- readBin(con = zz, what = numeric(), n = 70, size = 4, endian = endi)
    dim(h1) <- c(5, 14)
    h1 <- aperm(h1) # array transpose
    # NA values:
    h1[h1 == -12345] <- NA
    h2 <- readBin(con = zz, what = integer(), n = 35, size = 4, endian = endi)
    dim(h2) <- c(5, 7)
    h2 <- aperm(h2)
    # NA values:
    h2[h2 == -12345] <- NA
    h3 <- readBin(con = zz, what = logical(), n = 5, size = 4, endian = endi)
    h4 <- readBin(con = zz, what = character(), n = 1, size = 4, endian = endi)
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
    x <- readBin(con = zz, what = numeric(), n = N, size = 4, endian = endi)
    close(zz)
    contents <- list(amp = x, dt = dt, 
                     depmin = depmin, depmax = depmax,
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
                     units = idep, 
                     iztype = iztype,
                     leven = leven, lpspol = lpspol,
                     sta = kstnm, kevnm = kevnm, khole = khole,
                     ko = ko, ka = ka,
                     comp = kcmpnm, knetwork = knetwork, kinst = kinst)
    attr(contents, "sacfile") <- fi
    attr(contents, "endianness") <- endi
    class(contents) <- "sac"
    return(contents)
} # end READER

# Need to define indexing for Rsac so that it retains the rsac
# class to plotting will work correctly:
#' @rdname sacfiles
#' @aliases [.saclist
#' @method [ saclist
#' @S3method [ saclist
"[.saclist" <- function(x, i){
    x <- unclass(x)
    x <- x[i]
    class(x) <- "sac"
    return(x)
}
#' @rdname sacfiles
#' @aliases c.saclist
#' @method c saclist
#' @S3method c saclist
c.saclist <- function(...){
    x <- c(unlist(unclass(list(...)), recursive = FALSE))
    class(x) <- "sac"
    return(x)
}

#' @rdname sacfiles
#' @export
fstart <- function(x) UseMethod("fstart")
#' @rdname sacfiles
#' @aliases fstart.sac
#' @method fstart sac
#' @S3method fstart sac
fstart.sac <- function(x){
    return(x$nzhour*3600 +  x$nzmin*60  +  x$nzsec  +  x$nzmsec*0.001)
}

#' @rdname sacfiles
#' @export
units <- function(x) UseMethod("units")
#' @rdname sacfiles
#' @aliases units.saclist
#' @method units saclist
#' @S3method units saclist
units.saclist <- function(x){
    getint <- function(X){X$units}
    val <- sapply(x, getint)
    if (all(mode(val) == "numeric")){
        val[val == 5] <- c("Unknown")
        val[val == 6] <- c("Displacement, nm")
        val[val == 7] <- c("Velocity, nm/sec")
        val[val == 8] <- c("Acceleration, nm/sec/sec")
        val[val ==50] <- c("Velocity, volts")
    }
    return(val)
}

#' @rdname sacfiles
#' @export
sync <- function(x) UseMethod("sync")
#' @rdname sacfiles
#' @aliases sync.saclist
#' @method sync saclist
#' @S3method sync saclist
sync.saclist <- function(x){
    st <- sapply(x, fstart)
    st <- st - min(st)
    ref <- which(st == min(st))[1]
    reft <- list(nzhour = x[[ref]]$nzhour,
                 nzmin = x[[ref]]$nzmin,
                 nzsec = x[[ref]]$nzsec,
                 nzmsec = x[[ref]]$nzmsec,
                 b = x[[ref]]$b)
    for (i in seq_len(length(x))){
        x[[i]]$nzhour <- reft$nzhour
        x[[i]]$nzmin <- reft$nzmin
        x[[i]]$nzsec <- reft$nzsec
        x[[i]]$nzmsec <- reft$nzmsec
        x[[i]]$b <- x[[i]]$b + st[i]
        x[[i]]$e <- x[[i]]$e + st[i]
    }
    return(x)
}