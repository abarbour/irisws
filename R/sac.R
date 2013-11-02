#' Read a SAC binary file
#' @description 
#' Loads SAC (Seismic Analysis Code) data files [1], stored as either 
#' ASCII or binary format.
#' 
#' From [2]:
#' \emph{
#' [SAC] files contain a fixed length header section followed by one or 
#' two data sections. The header contains floating point, integer, logical, 
#' and character fields. Evenly spaced data files have only one data section
#'  which contains the dependent variable. Unevenly spaced data and spectral 
#'  data files contain two data sections. For unevenly spaced data, the first 
#'  data section contains the dependent variable and the second contains the 
#'  independent variable. For spectral files the first component is either 
#'  the amplitude or the real component and the second component is either 
#'  the phase or imaginary component.
#' }
#' 
#' @details 
#' The ASCII reader (\code{\link{.sacreader.asc}}) is simply a series 
#' of \code{\link{read.table}} calls,
#' and the binary reader (\code{\link{.sacreader.bin}}) uses
#' \code{\link{readBin}} with the specified endianness.
#' 
#' \subsection{Utility functions}{
#' \code{\link{sync}}:
#' From documentation in the last available version of \code{Rsac}:
#' \emph{
#' Synchronizes the reference times of all files in a vector of SAC files. 
#' [...]
#' This is useful if you are sending each file to a different plot but 
#' want the relative time to be consistent between the different plots.
#' }
#' 
#' \code{\link{sacunits}}:
#' From documentation in the last available version of \code{Rsac}:
#' \emph{
#' Looks up the units of the [amplitudes in the] SAC record. The units in many seismic
#'  headers are notoriously unreliable, so care should be taken to 
#'  find an independent source to confirm the units.
#' }
#' 
#' \code{\link{fstart}}:
#' From documentation in the last available version of \code{Rsac}:
#' \emph{
#' Calculates the starting time [of the SAC data].
#' }
#' }
#' @name sacfiles
#' @aliases sacfile sac read_sac
#' @author A.J. Barbour modified code from the (now defunct)
#' package \code{Rsac}, written originally by E.M. Thompson.
#' 
#' @param files character; the file(s) to read in
#' @param is.binary logical; are the sac files in \code{files} binary or ASCII?
#' @param endianness character; specify the endianness of \code{file}.
#' \code{'auto'} uses the platform value, or \code{'little'} and \code{'big'}
#' can be used to force a specific structure.
#' @param ... additional parameters;
#' For \code{\link{read.sac}}: additional objects to the sac reader; 
#' for \code{\link{c.saclist}}: the objects to concatenate
#' @param fi character; a single filename
#' @param na.value the \code{NA} representation
#' @param amp.as.ts logical; should the amplitudes be converted to a \code{'ts'} object?
#' @param x an object to operate on.
#' @param object an object to operate on.
#' @param recursive  logical; From \code{\link{c}}:\emph{
#' If \code{recursive = TRUE}, the function recursively descends 
#' through lists (and pairlists) combining all their elements into a vector.
#' }
#' @param ncol numeric; the number of columns in the plot \code{\link{layout}}
#' @param relative logical; should the start times be relative to
#' the minimum of the group?
#'
#' @return A list of lists, with class \code{'saclist'}, where each 
#' item corresponds to the contents of each entry in
#'  \code{files}, each with class \code{'sac'}.
#' 
#' @references [1] \url{http://www.iris.edu/software/sac/}
#' @references [2] \url{http://www.iris.edu/files/sac-manual/}
#' 
#' @seealso \code{\link{irisws-package}}
#' 
#' @family SAC
#' 
#' @examples
#' \dontrun{
#' ##
#' ## SAC Binary reader
#' ##
#' sacfi <- system.file("sac/elmayorB084.sac", package="irisws")
#' #   this is a little-endian sac file, so
#' #   must specify (your system may be 'big'!)
#' x1 <- read.sac(sacfi, is.binary=TRUE, endianness="little")
#' #   returns an object of class 'saclist'
#' plot(x1)
#' ##
#' ## SAC ASCII reader
#' ##
#' sacascfi <- system.file("sac/elmayorB084.txt", package="irisws")
#' x2 <- read.sac(sacascfi, is.binary=FALSE)
#' plot(x2)  
#' all.equal(x1[1]$amp, x2[1]$amp) # they are equal, as expected
#' #
#' # Can also load a series of files:
#' #
#' sacfis <- rep(sacfi, 3)
#' x3 <- read.sac(sacfis, is.binary=TRUE, endianness="little")
#' plot(x3) # now there are three frames in the plot
#' #
#' # Utilities
#' #
#' c(x1)
#' sacunits(x1)
#' 
#' }
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
.sacreader.bin <- function(fi, endianness=c("little","big"), na.value=-12345, amp.as.ts=TRUE){
    endi <- match.arg(endianness)
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
    HFUN3 <- function(H, i0){
        H[i0]
    }
    leven <- HFUN3(h3, 1)
    lpspol <- HFUN3(h3, 2)
    # H 4
    HFUN4 <- function(HH, i0, i1, na.val){
        nav <- as.character(na.val)
        replacement <- paste(rep(" ", nchar(nav)),collapse="")
        #|-12345|
        #|      |
        x <- substr(HH, i0, i1)
        sub(nav, replacement, x)
    }
    kstnm <- HFUN4(h4, 1, 8, na.value)
    kevnm <- HFUN4(h4, 9, 24, na.value)
    khole <- HFUN4(h4, 25, 32, na.value)
    ko <- HFUN4(h4, 33, 40, na.value)
    ka <- HFUN4(h4, 41, 48, na.value)
    kcmpnm <- HFUN4(h4, 161, 168, na.value)
    knetwork <- HFUN4(h4, 169, 176, na.value)
    kinst <- HFUN4(h4, 185, 192, na.value)
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
# @param i indices specifying elements to extract or replace.
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
c.saclist <- function(..., recursive = FALSE){
    x <- base::c(unlist(unclass(list(...)), recursive = recursive))
    class(x) <- "sac"
    return(x)
}

#' @rdname sacfiles
#' @aliases print.sac
#' @method print sac
#' @S3method print sac
#' @export
print.sac <- function(x, ...){
    print.default(x, ...)
}

#' @rdname sacfiles
#' @aliases print.saclist
#' @method print saclist
#' @S3method print saclist
#' @export
print.saclist <- function(x, ...){
    xs <- summary(unclass(x))
    fis <- sapply(x, function(n) attr(n, "sacfile"))
    message("++++\n++++\tsaclist composition:\n++++")
    print(xs <- cbind(Sources=fis, xs))
    return(invisible(xs))
}

#' @rdname sacfiles
#' @aliases summary.saclist
#' @method summary saclist
#' @S3method summary saclist
#' @export
summary.saclist <- function(x, ...){
    xs <- summaryStats(x, ...)
    class(xs) <- 'summary.saclist'
    return(xs)
}

#' @rdname sacfiles
#' @aliases print summary.saclist
#' @method print summary.saclist
#' @S3method print summary.saclist
#' @export
print.summary.saclist <- function(x, ...){
    message("++++\n++++\tsaclist content summary:\n++++")
    print(xs <- unclass(x))
    return(invisible(xs))
}

#' @rdname sacfiles
#' @aliases str.saclist
#' @method str saclist
#' @S3method str saclist
#' @export
str.saclist <- function(object, ...){
    invisible(sapply(object, str))
}

#' @rdname sacfiles
#' @aliases plot.saclist
#' @method plot saclist
#' @S3method plot saclist
#' @export
plot.saclist <- function(x, ncol=1, stat.annotate=TRUE, trim = 0,
                         rel.time = NULL, apply.calib=TRUE, ...){
    uts <- sacunits(x)
    uts[uts == "Unknown"] <- ".raw counts."
    #
    tst <- sapply(x, fstart)
    tst <- tst - min(tst)
    #
    nsacs <- length(x)
    sacseq <- seq_len(nsacs)
    op <- par(no.readonly = TRUE)
    par(mar=c(2.3, 4.1, 1.3, 1.1), #5.1 4.1 4.1 2.1
        oma=c(2.5, 0.5, 1.3, 0.5)) #0 0 0 0
    on.exit(par(op))
    lo <- layout(matrix(sacseq, ncol=ncol)) #, sacseq)
    layout.show(lo)
    X <- unclass(x)
    for (n in sacseq){
        fi <- attr(X[[n]],"sacfile")
        ss <- summaryStats(X[[n]], trim=trim, rel.time=rel.time)
        amp <- X[[n]]$amp
        delt <- X[[n]]$dt
        #print(c("SSRS",ss$relsec))
        amp <- ts(amp, deltat=delt, start=ss$start.relsec) #tst[n])
        #
        ylab <- uts[n]
        #annot1 <- c(ss$amp.MAD, ss$amp.SD)
        annot2 <- c(ss$amp.hinge.lower, ss$amp.median, ss$amp.hinge.upper)
        annot3 <- c(ss$amp.maximum, ss$amp.minimum)
        if (apply.calib){
            calib <- sum(c(ss$amp.calib.factor * 1., 0), na.rm=TRUE)
            if (calib > 0){
                #annot1 <- annot1 / calib
                annot2 <- annot2 / calib
                annot3 <- annot3 / calib
                amp <- amp / calib
                ylab <- paste(ylab, '[scaled]')
            } else {
                warning("could not find calibration factor")
            }
        }
        #
        x <- time(amp)
        plot.default(x, as.vector(amp), 
                     type="l", 
                     xlab="", 
                     ylab=ylab,
                     #col=NA,
                     ...)
        # statistical annotations
        if (stat.annotate){
            gcol <- "red"
            #print(c(annot1,annot2,annot3))
            #abline(h=annot1, lty=3, col=gcol)
            gcol <- "grey50"
            abline(h=annot2, lty=2, lwd=1.2, col=gcol)
            gcol <- "red"
            abline(h=annot3, lty=1, col=gcol)
            points(c(ss$relsec.max, ss$relsec.min), annot3, col="blue")
        }
        #lines.default(x, as.vector(amp))
        mtext(fi, cex=0.7)
        sta <- X[[n]]$sta
        mtext(sta, 
              side=3, cex=0.8, font=2, adj=0.01, line=-1.5)
        mtext(paste("start:", ss$year, ss$jday, paste(ss$hr, ss$min, ss$sec, sep=":")), 
              side=1, cex=0.7, font=1, adj=0.01, line=2.1)
        #
        if (n %% (nsacs/ncol) == 0){
            mtext("time", side=1, line=2.5)
        }
    }
}

#' @rdname sacfiles
#' @export
fstart <- function(x, relative=FALSE) UseMethod("fstart")
#' @rdname sacfiles
#' @aliases fstart.sac
#' @method fstart sac
#' @S3method fstart sac
fstart.sac <- function(x, relative=NULL){
    return(x$nzhour*3600 + x$nzmin*60 + x$nzsec + x$nzmsec*0.001)
}
#' @rdname sacfiles
#' @aliases fstart.saclist
#' @method fstart saclist
#' @S3method fstart saclist
fstart.saclist <- function(x, relative=FALSE){
    xst <- sapply(seq_along(x), function(n) fstart(x[[n]]))
    if (relative){
        xmin <- min(xst, na.rm=TRUE)
        xmin.i <- which(xst==xmin)[1]
        xst <- xst - xmin
        attr(xst,"ref") <- xmin.i
        attr(xst,"val") <- xmin
    }
    return(xst)
}

#' @rdname sacfiles
#' @export
sacunits <- function(x) UseMethod("sacunits")
#' @rdname sacfiles
#' @aliases sacunits.sac
#' @method sacunits sac
#' @S3method sacunits sac
sacunits.sac <- function(x){
    val <- x$units
    if (!is.na(val) & val != 5){
        switch(paste0("u"), 
               u6 = "Displacement, nm",
               u7 = "Velocity, nm/sec",
               u8 = "Acceleration, nm/sec/sec",
               u50 = "Velocity, volts"
        )
    } else {
        "Unknown"
    }
}
#' @rdname sacfiles
#' @aliases sacunits.saclist
#' @method sacunits saclist
#' @S3method sacunits saclist
sacunits.saclist <- function(x){
    sapply(seq_along(x), function(n) sacunits(x[[n]]))
}

#' @rdname sacfiles
#' @export
sync <- function(x) UseMethod("sync")
#' @rdname sacfiles
#' @aliases sync.saclist
#' @method sync saclist
#' @S3method sync saclist
sync.saclist <- function(x){
    #
    st <- fstart(x, relative=TRUE)
    ref <- attr(st, "ref")
    refval <- attr(st, "val")
    #
    reft <- list(nzhour = x[[ref]]$nzhour,
                 nzmin = x[[ref]]$nzmin,
                 nzsec = x[[ref]]$nzsec,
                 nzmsec = x[[ref]]$nzmsec,
                 b = x[[ref]]$b)
    #
    for (i in seq_len(length(x))){
        x[[i]]$reftime <- c(ref, refval)
        x[[i]]$nzhour <- reft$nzhour
        x[[i]]$nzmin <- reft$nzmin
        x[[i]]$nzsec <- reft$nzsec
        x[[i]]$nzmsec <- reft$nzmsec
        x[[i]]$b <- x[[i]]$b + st[i]
        x[[i]]$e <- x[[i]]$e + st[i]
    }
    return(x)
}

#' @rdname sacfiles
#' @param trim numeric; the fraction of data to trim from the start and end
#' of the amplitude record for the statistical summary. Can be a two-length
#' vector, but must be within [0,0.5].
#' @param rel.time POSIXct; report the number of seconds relative to this
#' @export
summaryStats <- function(x, trim=0, rel.time=NULL) UseMethod("summaryStats")
#' @rdname sacfiles
#' @aliases summaryStats.sac
#' @method summaryStats sac
#' @S3method summaryStats sac
summaryStats.sac <- function(x, trim=0, rel.time=NULL){
    #print(str(x))
    N <- x$N
    if (N == 0){
        stop("sac file contains no data (N == 0)")
    }
    dt <- x$dt
    #..$ nzyear  : int 2010
    yr <- x$nzyear
    #..$ nzjday  : int 94
    jd <- x$nzjday
    #..$ nzhour  : int 21
    hrs <- x$nzhour
    #..$ nzmin   : int 42
    mins <- x$nzmin
    #..$ nzsec   : int 11
    #..$ nzmsec  : int 4
    secs <- x$nzsec
    msecs <- x$nzmsec
    #
    #print(c(N,dt,yr,jd,hrs,secs,msecs))
    #
    # total relative second index (TRS) ==
    #       nzsec + nzmsec*0.001
    trsecs <- secs + msecs*0.001
    # total absolute second index ==
    #       nzhour*3600 + nzmin*60 + TRS
    #arsecs <- hrs*3600 + mins*60 + TRS
    #
    record.start <- ISOdatetime.j(yr, jd, hrs, mins, trsecs)
    #
    if (is.null(rel.time)){
        rel.start <- difftime(record.start, record.start, units="sec")
    } else {
        stopifnot(length(rel.time)==1)
        if (!is(rel.time, "POSIXt")){
            rel.time <- ISOdatetime.j(tstr=paste(rel.time))
        }
        rel.start <- difftime(record.start, rel.time, units="sec")
    }
    rel.start <- unclass(rel.start)[1]
    #
    stopifnot(trim>=0 & trim<0.5)
    #
    if (length(trim)==1){
        trim <- rep(trim,2)
    }
    lo <- floor(N * trim[1]) + 1
    hi <- N - floor(N * trim[2])
    #
    dato <- x$amp
    dat <- dato[lo:hi]  # trimmed data
    calib <- x$scale
    calib.units <- sacunits(x)
    #
    fn <- fivenum(dat)
    xsd <- sd(dat, na.rm=TRUE)
    xmad <- mad(dat, na.rm=TRUE)
    xmin <- fn[1]
    lhinge <- fn[2]
    xmed <- fn[3]
    uhinge <- fn[4]
    xmax <- fn[5]
    xmax.loc <- which(dato==xmax)
    xmin.loc <- which(dato==xmin)
    xsum <- data.frame(start.year=yr, 
                       start.jday=jd, 
                       start.hr=hrs, 
                       start.min=mins, 
                       start.sec=trsecs,
                       start.relsec=rel.start,
                       N=N, 
                       N.sec=N*dt,
                       n.lo=lo, n.hi=hi, 
                       amp.calib.factor=calib,
                       amp.maximum=xmax, 
                       amp.minimum=xmin,
                       amp.peak2peak.rms=abs(xmax - xmin)/2,
                       amp.hinge.upper=uhinge,
                       amp.median=xmed,
                       amp.hinge.lower=lhinge,
                       amp.MAD=xmad,
                       amp.SD=xsd,
                       relsec.max=(xmax.loc - 1)*dt + rel.start,
                       relsec.min=(xmin.loc - 1)*dt + rel.start
                       )
    attr(xsum,"calib.units") <- calib.units
    return(xsum)
}

#' @rdname sacfiles
#' @aliases summaryStats.saclist
#' @method summaryStats saclist
#' @S3method summaryStats saclist
summaryStats.saclist <- function(x, trim=0, rel.time=NULL){
    sapply(seq_along(x), function(n){summaryStats(x[[n]], trim, rel.time)})
}
