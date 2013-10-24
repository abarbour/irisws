#' Constructs a query function
#' 
#' @details 
#' These functions form correctly formatted queries for IRIS WS.
#' 
#' \code{\link{constructor}} is a basic constructor which simply
#' collapses the arguments into a single query.
#' 
#' \code{\link{constructor2}} also does this, but enforces 
#' parameter inputs so the
#' query at least has a chance for success.
#' 
#' \code{\link{params2queryparams}} is a function
#' to collapse arguments into a single query string
#' 
#' @param ... query parameters. 
#' For \code{\link{constructor}} these are comma-separated character strings (\code{"a=1", "b=2"} etc)
#' For \code{\link{constructor2}} these are comma-separated arguments (\code{a=1, b=2} etc)
#' @param service character; the type of web service to construct for.
#' @param defaults list; the parameters to merge \code{...} with
#' @param list.fields.only logical; return names of default arguments
#' @param exclude.nulls logical; should the result \emph{not} contain null values;
#' this is important, for instance, if the query cannot contain null values even
#' though they are set that way though the constructor defaults.
#' @export
#' @author AJ Barbour
#' 
#' @references \url{http://pages.stern.nyu.edu/~dbackus/BCH/data/FRED/fredplot.R}
#' was the motivation for \code{\link{params2queryparams}}.
#' 
#' @seealso \code{\link{iris.query}} to query IRIS WS
#' 
#' @examples
#' \dontrun{
#' # Basic construction:
#' constructor()
#' constructor("net=1","sta=2")
#' #
#' # parameter flattening
#' params2queryparams(net=1,sta=2)
#' #
#' # Constrained construction:
#' constructor2(net=1,sta=2)
#' # see what needs to be given:
#' constructor2(list.fields.only=TRUE)
#' #
#' # Distaz
#' constructor2(stalat=45, stalon=-120, evtlat=30.0, evtlon=-100.0, service="distaz")
#' }
constructor <- function(..., service=c("timeseries","distaz","traveltime")){
    #
    # service here DOES need to match iris specification
    service <- match.arg(service)
    #
    service.iris.edu <- "http://service.iris.edu/irisws"
    irisquery <- paste0(service.iris.edu,"/",service,"/1","/query?")
    query <- paste0(irisquery, paste(..., sep="&"))
    #
    return(query)
}
#' @rdname constructor
#' @export
constructor2 <- function(..., service=c("timeseries","distaz","tt.deg","tt.km"), list.fields.only=FALSE){
    #
    # service here does NOT need to match iris specification
    service <- match.arg(service)
    #
    # minimum defaults
    #   NULL fields are considered optional
    #   NA fields are considered mandatory
    if (service=="timeseries"){
        #http://service.iris.edu/irisws/timeseries/1/
        opts <- list(...)
        d.et <- NULL
        d.dur <- NA
        if ("endtime" %in% names(opts)){
            if (!is.null(opts[["endtime"]])){
                d.et <- NA
                d.dur <- NULL
            }   
        }
        mlst <- list(#rqd:
                     net=NA, sta=NA, loc=NA, cha=NA, 
                     starttime=NA, 
                     # these are either/or
                     endtime=d.et, 
                     duration=d.dur, 
                     # filter options: (order matters!)
                     taper=NULL, envelope=NULL,
                     lpfilter=NULL, hpfilter=NULL, bpfilter=NULL,
                     demean=NULL, differentiate=NULL, integrate=NULL,
                     scale=NULL, divscale=NULL, correct=NULL,
                     freqlimits=NULL, autolimits=NULL, units=NULL,
                     decimate=NULL, antialiasplot=NULL, 
                     audiocompress=NULL, audiosamplerate=NULL,
                     # and rqd:
                     output=NA)
    } else if (service=="distaz"){
        #http://service.iris.edu/irisws/distaz/1/
        mlst <- list(stalat=NA, stalon=NA, evtlat=NA, evtlon=NA)
    } else if (service=="tt.deg"){
        #http://service.iris.edu/irisws/traveltime/1/
        # where distance is epicentral degrees
        #/query? (distdeg=<degrees>) [evdepth=<km>] [model=<iasp91|prem|ak135>] [phases=<phaselist>] [output-params]
        service <- "traveltime"
        mlst <- list(distdeg=NA, evdepth=NULL, model=NULL, phases=NULL,
                     noheader=NULL, traveltimeonly=NULL, 
                     rayparamonly=NULL, mintimeonly=NULL)
    } else if (service=="tt.km"){
        #http://service.iris.edu/irisws/traveltime/1/
        # where distance is kilometers
        #/query? (distdeg=<km>) [evdepth=<km>] [model=<iasp91|prem|ak135>] [phases=<phaselist>] [output-params]
        service <- "traveltime"
        mlst <- list(distkm=NA, evdepth=NULL, model=NULL, phases=NULL,
                     noheader=NULL, traveltimeonly=NULL, 
                     rayparamonly=NULL, mintimeonly=NULL)
    }
    
    if (list.fields.only){
        ## Return only the field names
        defs <- names(mlst)
        optionals <- sapply(mlst, is.null)
        f.req <- defs[!optionals]
        f.opt <- defs[optionals]
        query <- list(required.fields=f.req, optional.fields=f.opt)
    } else {
        ## or the actual query
        qparams <- params2queryparams(..., defaults=mlst, exclude.nulls=TRUE)
        query <- constructor(qparams, service=service)
    }
    return(query)
}

#' @rdname constructor
#' @export
params2queryparams <- function(..., defaults, exclude.nulls=TRUE){
    # creates a list of parameters: e.g., a, b
    if (missing(defaults)) defaults <- list()
    params <- merge(list(...), defaults)
    # flattens to strings with, e.g., "a=1", "b=1", etc
    eparams <- sapply(names(params), function(pname) { 
        val <- params[[pname]]
        if (is.null(val)){NULL}else{paste0(pname, "=", val)}
        })
    # collapse them into a single line
    if (exclude.nulls){
        eparams <- eparams[!sapply(eparams, is.null)]
    }
    qparams <- paste(eparams, collapse="&")
    return(qparams)
}

#' Perform a query to IRIS-WS
#' @details
#' This function uses \code{\link{curlPerform}} in the \strong{RCurl} package
#' to query the IRIS WS. 
#' Firstly, it
#' checks if \code{query} will fail using \code{\link{url.exists}},
#' and then queries, writing to \code{filename} (which is instantiated
#' with \code{\link{CFILE}}).
#' 
#' 
#' \code{\link{iris.query}} is simply a pointer to \code{\link{query.iris}}
#' 
#' @export
#' @author AJ Barbour
#' @param iquery character; the web service query
#' @param filename character; the file to save query results to.  If this is \code{NULL} a
#' temporary file from \code{\link{tempfile}} is used.
#' @param is.binary logical; will the output be binary? (e.g., \code{TRUE} for SAC binary, and \code{FALSE} for a plot)
#' @param check logical; should \code{\link{check.query}} be used to check the quality of \code{iquery}
#' @param verbose logical; should messages be given?
#' @param ... additional arguments to \code{\link{curlPerform}}
#' @examples
#' \dontrun{
#' # This will create ANMO.png
#' # (duration, or use end=2005.002T00:00:00)
#' Q <- constructor2(net="IU", sta="ANMO", loc="00", cha="BHZ", starttime="2005.001T00:00:00", duration="1000", output="plot")
#' query.iris(Q, "ANMO.png")
#' #
#' # and this will put it in a temporary file
#' query.iris(Q, NULL) 
#' #
#' # this will fail, obviously, unless there's a seismic network
#' # named 'XXXTHISWILLFAILXXX'
#' Q <- constructor("net=XXXTHISWILLFAILXXX")
#' query.iris(Q)
#' }
query.iris <- function(iquery, filename="iris.query.results", is.binary=FALSE, check=TRUE, verbose=TRUE, ...){ 
    if (check) check.query(iquery)
    ure <- RCurl::url.exists(iquery)
    if (ure){
        if (is.null(filename)){
            filename <- tempfile('irisws.query.results')
        }
        #
        md <- "w"
        if (is.binary){ md <- paste0(md,"b")}
        lf = RCurl::CFILE(filename, mode=md)
        RCurl::curlPerform(url = iquery, writedata = lf@ref, ...)
        RCurl::close(lf)
        #
        if (verbose) message(sprintf("IRIS WS query complete:  %s", filename))
        return(invisible(list(file=filename, query=iquery)))
    } else {
       stop("IRIS WS query failed.")
    }
}
#' @export
#' @rdname query.iris
iris.query <- query.iris

#' @export
#' @rdname query.iris
check.query <- function(Q){
    Qs <- unlist(strsplit(as.character(Q),"&"))
    gr <- grepl("NA", Qs)
    #[1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    if (any(gr)){
        stop(paste("Invalid query options:", paste(Qs[gr], collapse=" ")))
    }
    return(invisible(list(Q=Q, Qs=Qs)))
}

#' Produce a properly formatted string for IRIS WS
#' @description 
#' Provides a mechanism to produce properly formatted
#' strings for the time-fields of, say, \code{\link{query.iris}},
#' acceptable to IRIS WS.
#' @details
#' An IRIS WS time-string can be formatted in two ways: 
#' (1a) using year-month-day, (e.g., \code{'1997-01-31T12:04:32.123'}) or
#' (2) using year-day (e.g.,\code{'1997.031T12:04:32.123'}),
#' where the string after 'T' corresponds to hour:minute:fractional-seconds.
#' The string can also be of the form (1b)
#' \code{'1997-01-31'} (in this case \code{'00:00:00'} is assumed), but
#' we have found this format can lead to query failures.
#' \emph{\strong{
#' In this program the string is always returned in format (2):
#' \code{<year>.<day>T<hour>:<min>:<sec>}
#' }}
#' 
#' \code{sec} may be fractional, but is formatted with \code{\link{sprintf}} 
#' (\code{02.06f}) so
#' values less than 1 microsecond will be truncated (\emph{not} rounded).
#' 
#' Note that IRIS WS accepts values for hour, minute, and second which are less
#' than 100.  Return data will have times adjusted to account for values in excess of
#' the normal limits (i.e., 24, 60, 60).
#'
#' @param year numeric; the \emph{full} year A.D. (e.g., 2012 \emph{not} 12)
#' @param day numeric; the day, either of-the-year, or of-the-month (see \code{month})
#' @param hour numeric; the hour of the day (less than 100)
#' @param min numeric; the minute (less than 100)
#' @param sec numeric; fractional seconds (less than 100; will be truncated to 6 decimal places)
#' @param month numeric; the month of the year. 
#' If this is \code{NULL} then \code{day} is assumed
#' to be the Julian day of year.
#' @export
#' @author AJ Barbour
#' @references [1] \url{http://service.iris.edu/irisws/timeseries/1/}
#' @examples
#' \dontrun{
#' #
#' # Specify the month
#' timestring(2012, 15, 32, 12, 12.222, month=12)
#' # [1] "2012.350T32:12:12.222000"
#' #
#' # or not
#' timestring(2012, 15, 32, 12, 12.222)
#' # [1] "2012.015T32:12:12.222000"
#' #
#' # some errors:
#' try(timestring(2012, 15, 32, 100, 12.222)) # min too large
#' try(timestring(2012, 75755, 32, 12, 12.222)) # day too large
#' try(timestring(2012, 15, 32, 100, 12.222, 13)) # month too large
#' # etc...
#' }
timestring <- function(year, day, hour, min, sec, month=NULL){
    #
    stopifnot(length(c(year, day, hour, min, sec, month)) <= 6 )
    hour <- as.numeric(hour)
    min <- as.numeric(min)
    sec <- as.numeric(sec)
    irislim <- 100
    if (any(c(sec, min, hour) >= irislim)){
        stop(paste("IRISWS requires that 'sec', 'min', and 'hour all be less than", irislim))
    }
    #
    jday <- ifelse(is.null(month), TRUE, FALSE)
    if (jday){
        nd <- as.numeric(strftime(sprintf("%04i-12-31",year),"%j"))
        stopifnot(day <= nd)
        yjd <- sprintf("%04i.%03i", year, day)
    } else {
        stopifnot(month <= 12 & day <= 31)
        yjd <- strftime(sprintf("%04i-%02i-%02i", year, month, day), "%Y.%j")
    }
    isec <- round(sec)
    rsec <- trunc(1e6*(sec - isec))
    tstr <- sprintf("%sT%02i:%02i:%02i.%06i", yjd, hour, min, isec, rsec)
    return(tstr)
}
