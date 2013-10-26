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
#' @param is.binary logical; are the sac files in \code{files} binary or ascii?
#' @param endianness character; specify the endianness of \code{file}.
#' \code{'auto'} uses the platform value, or \code{'little'} and \code{'big'}
#' can be used to force a specific structure.
#' @param fi character; the sac-filename
#' @param endi character; the actual endianness of the sac-file
#' @param na.value the \code{NA} representation
#' @param amp.as.ts logical; should the amplitudes be converted to a \code{'ts'} object?
#' @param x an object with class \code{'sac'} to operate on.
#' @param ncol numeric; the number of columns in the plot \code{\link{layout}}
#' @param ... additional parameters;
#' For \code{\link{read.sac}}: additional objects to \code{\link{.sacreader}}.
#' For \code{c.sac}: roughly equivalent to \code{\link{c}}.
#' @param i indices specifying elements to extract or replace.
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
read.sac <- function(files, is.binary, endianness=c("auto","little","big"), ...){
    sacfiles <- as.character(files)
    nsacfi <- length(sacfiles)
    sacdat <- vector(mode="list", length=nsacfi)
    #
    # try and find out
    #scan("iriswsQ.PB.B084.--.LDD.2013-10-01T00:00:00.100s.sac", numeric(), n=1)
    # NA for sac, 1 for txt
    if (is.binary){
        endian <- match.arg(endianness)
        if (endian=="auto"){
            endian <- .Platform[["endian"]]
        }
        SACR <- function(Fi, ...){.sacreader.bin(Fi, endian, ...)}
    } else {
        SACR <- function(Fi, ...){.sacreader.asc(Fi, ...)}
    }
    #
    for (fi in seq_len(nsacfi)){
        sfi <- sacfiles[fi]
        sacdat[[fi]] <- SACR(sfi, ...)
    }
    class(sacdat) <- "saclist"
    return(sacdat)
}

#' @rdname sacfiles
#' @export
.sacreader.asc <- function(fi, na.value=c("-12345","-12345.00"), amp.as.ts=TRUE){
    #
    HFUN <- function(H, i0, j0){
        H[i0,j0]
    }
    #H 1 -- 5-col
    h1 <- read.table(fi, skip=0, nrows=14, na.strings=na.value)
    dt <- HFUN(h1, 1, 1)
    depmin <- HFUN(h1, 1, 2)
    depmax <- HFUN(h1, 1, 3)
    scale <- HFUN(h1, 1, 4)
    odelta <- HFUN(h1, 1, 5)
    b <- HFUN(h1, 2, 1)
    e <- HFUN(h1, 2, 2)
    o <- HFUN(h1, 2, 3)
    a <- HFUN(h1, 2, 4)
    f <- HFUN(h1, 5, 1)
    stla <- HFUN(h1, 7, 2)
    stlo <- HFUN(h1, 7, 3)
    stel <- HFUN(h1, 7, 4)
    stdp <- HFUN(h1, 7, 5)
    evla <- HFUN(h1, 8, 1)
    evlo <- HFUN(h1, 8, 2)
    evel <- HFUN(h1, 8, 3)
    evdp <- HFUN(h1, 8, 4)
    mag <- HFUN(h1, 8, 5)
    dist <- HFUN(h1, 11, 1)
    az <- HFUN(h1, 11, 2)
    baz <- HFUN(h1, 11, 3)
    gcarc <- HFUN(h1, 11, 4)
    cmpaz <- HFUN(h1, 12, 3)
    cmpinc <- HFUN(h1, 12, 4)
    # H 2
    #H 2 -- 5-col
    h2 <- read.table(fi, skip=14, nrows=8, na.strings=na.value)
    nzyear <- HFUN(h2, 1, 1)
    nzjday <- HFUN(h2, 1, 2)
    nzhour <- HFUN(h2, 1, 3)
    nzmin <- HFUN(h2, 1, 4)
    nzsec <- HFUN(h2, 1, 5)
    nzmsec <- HFUN(h2, 2, 1)
    # 2,2 ?
    norid <- HFUN(h2, 2, 3)
    nevid <- HFUN(h2, 2, 4)
    N <- HFUN(h2, 2, 5)
    # 3: is empty (?)
    # 4,1 (?)
    idep <- HFUN(h2, 4, 2)
    iztype <- HFUN(h2, 4, 3)
    #
    leven <- HFUN(h2, 8, 1) #?
    lpspol <- HFUN(h2, 8, 2) #?
    #
    #H 3 -- 2-col
    h3 <- read.table(fi, skip=22, nrows=1, na.strings=na.value)
    #
    kstnm <- HFUN(h3, 1, 1)
    kevnm <- HFUN(h3, 1, 2)
    #H 3 -- 3-col
    h4 <- read.table(fi, skip=23, nrows=7, na.strings=na.value)
    #
    khole <- HFUN(h4, 1, 1) #HFUN(h4, 25, 32, na.value)
    ko <- HFUN(h4, 6, 1) # ? #HFUN(h4, 33, 40, na.value)
    ka <- HFUN(h4, 6, 2) # ? #HFUN(h4, 41, 48, na.value)
    kcmpnm <- HFUN(h4, 6, 3) #HFUN(h4, 161, 168, na.value)
    knetwork <- HFUN(h4, 7, 1) #HFUN(h4, 169, 176, na.value)
    kinst <- HFUN(h4, 7, 3) #HFUN(h4, 185, 192, na.value)
    #
    #H 4 -- 5-col - data
    amp <- as.vector(as.matrix(read.table(fi, skip=30, na.strings=na.value)))
    if (amp.as.ts){
        amp <- ts(data=amp, deltat=as.numeric(dt))
    }
    #
    contents <- list(amp = amp, dt = dt, 
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
    attr(contents, "endianness") <- "NA--ASCII"
    class(contents) <- "sac"
    return(contents)
}

#' @rdname sacfiles
#' @export
.sacreader.bin <- function(fi, endi, na.value=-12345, amp.as.ts=TRUE){
    # Open the file for reading
    zz <- file(fi, "rb")
    #
    binsize <- 4L
    ##
    ## Read in the header information.
    h1 <- try(readBin(con = zz, what = numeric(), n = 70, size = binsize, endian = endi))
    dim(h1) <- c(5, 14)
    h1 <- aperm(h1) # array transpose
    # NA values:
    h1[h1 == na.value] <- NA
    ##
    ##
    h2 <- try(readBin(con = zz, what = integer(), n = 35, size = binsize, endian = endi))
    dim(h2) <- c(5, 7)
    h2 <- aperm(h2)
    # NA values:
    h2[h2 == na.value] <- NA
    ##
    ##
    h3 <- try(readBin(con = zz, what = logical(), n = 5, size = binsize, endian = endi))
    ##
    ##
    h4 <- try(readBin(con = zz, what = character(), n = 1, size = binsize, endian = endi))
    #
    # Define header proocessing function:
    HFUN <- function(H, i0, i1){
        H[i0, i1]
    }
    # and get variables...
    # H1
    dt <- HFUN(h1, 1, 1)
    depmin <- HFUN(h1, 1, 2)
    depmax <- HFUN(h1, 1, 3)
    scale <- HFUN(h1, 1, 4)
    odelta <- HFUN(h1, 1, 5)
    b <- HFUN(h1, 2, 1)
    e <- HFUN(h1, 2, 2)
    o <- HFUN(h1, 2, 3)
    a <- HFUN(h1, 2, 4)
    f <- HFUN(h1, 5, 1)
    stla <- HFUN(h1, 7, 2)
    stlo <- HFUN(h1, 7, 3)
    stel <- HFUN(h1, 7, 4)
    stdp <- HFUN(h1, 7, 5)
    evla <- HFUN(h1, 8, 1)
    evlo <- HFUN(h1, 8, 2)
    evel <- HFUN(h1, 8, 3)
    evdp <- HFUN(h1, 8, 4)
    mag <- HFUN(h1, 8, 5)
    dist <- HFUN(h1, 11, 1)
    az <- HFUN(h1, 11, 2)
    baz <- HFUN(h1, 11, 3)
    gcarc <- HFUN(h1, 11, 4)
    cmpaz <- HFUN(h1, 12, 3)
    cmpinc <- HFUN(h1, 12, 4)
    # H 2
    nzyear <- HFUN(h2, 1, 1)
    nzjday <- HFUN(h2, 1, 2)
    nzhour <- HFUN(h2, 1, 3)
    nzmin <- HFUN(h2, 1, 4)
    nzsec <- HFUN(h2, 1, 5)
    nzmsec <- HFUN(h2, 2, 1)
    norid <- HFUN(h2, 2, 3)
    nevid <- HFUN(h2, 2, 4)
    N <- HFUN(h2, 2, 5)
    idep <- HFUN(h2, 4, 2)
    iztype <- HFUN(h2, 4, 3)
    # H 3
    HFUN <- function(H, i0){
        H[i0]
    }
    leven <- HFUN(h3, 1)
    lpspol <- HFUN(h3, 2)
    # H 4
    HFUN <- function(HH, i0, i1, na.val){
        nav <- as.character(na.val)
        replacement <- paste(rep(" ", nchar(nav)),collapse="")
        #|-12345|
        #|      |
        x <- substr(HH, i0, i1)
        sub(nav, replacement, x)
    }
    kstnm <- HFUN(h4, 1, 8, na.value)
    kevnm <- HFUN(h4, 9, 24, na.value)
    khole <- HFUN(h4, 25, 32, na.value)
    ko <- HFUN(h4, 33, 40, na.value)
    ka <- HFUN(h4, 41, 48, na.value)
    kcmpnm <- HFUN(h4, 161, 168, na.value)
    knetwork <- HFUN(h4, 169, 176, na.value)
    kinst <- HFUN(h4, 185, 192, na.value)
    #
    # Get the amplitudes...
    try(seek(con=zz, where=632))
    try(x <- readBin(con=zz, what=numeric(), n=N, size=binsize, endian=endi))
    #
    close(zz)
    #
    # load up the final product
    if (amp.as.ts){
        x <- ts(data=x, deltat=dt)
    }
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
# @rdname sacfiles
# @aliases [.saclist
# @method [ saclist
# @S3method [ saclist
#"[.saclist" <- function(x, i){
#    x <- unclass(x)
#    x <- x[i]
#    class(x) <- "sac"
#    return(x)
#}

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
#' @aliases plot.saclist
#' @method plot saclist
#' @S3method plot saclist
#' @export
plot.saclist <- function(x, ncol=1, ...){
    uts <- units(x)
    uts[is.na(uts)] <- ".raw."
    #
    tst <- sapply(x, fstart)
    tst <- tst - min(tst)
    #
    nsacs <- length(x)
    sacseq <- seq_len(nsacs)
    op <- par(no.readonly = TRUE)
    par(mar=c(2.1, 4.1, 1.3, 1.1), #5.1 4.1 4.1 2.1
        oma=c(2.3, 0.5, 1.3, 0.5)) #0 0 0 0
    on.exit(par(op))
    lo <- layout(matrix(sacseq, ncol=ncol)) #, sacseq)
    layout.show(lo)
    X <- unclass(x)
    for (n in sacseq){
        fi <- attr(X[[n]],"sacfile")
        amp <- X[[n]]$amp
        delt <- X[[n]]$dt
        amp <- ts(amp, deltat=delt, start=tst[n])
        x <- time(amp)
        plot.default(x, as.vector(amp), 
                     type="l", 
                     xlab="", 
                     ylab=uts[n],
                     ...)
        mtext(fi, cex=0.7)
        if (n %% (nsacs/ncol) == 0){
            mtext("time", side=1, line=2.3)
        }
    }
}

#' @rdname sacfiles
#' @export
fstart <- function(x) UseMethod("fstart")
#' @rdname sacfiles
#' @aliases fstart.sac
#' @method fstart sac
#' @S3method fstart sac
fstart.sac <- function(x){
    return(x$nzhour*3600 + x$nzmin*60 + x$nzsec + x$nzmsec*0.001)
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