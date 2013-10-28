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
#' @param exclude.empty.options logical; should the result \emph{not} contain 
#' optional arguments which were not specified? 
#' @param exclude.null.fields logical; should \code{NULL} fields in \code{...}
#' or \code{defaults} be removed prior to operations?
#' @param ws.version character; corresponds to the documentation version (See [1]). 
#' @return character string(s)
#' 
#' @export
#' @author A.J. Barbour; and, see [2]
#' 
#' @references 
#' [1] \url{http://service.iris.edu/irisws/}
#' @references
#' [2] \url{http://pages.stern.nyu.edu/~dbackus/BCH/data/FRED/fredplot.R}
#' was the motivation for \code{\link{params2queryparams}}.
#' 
#' @seealso 
#' \code{\link{iris.query}} to query IRIS WS
#' 
#' @seealso \code{\link{irisws-package}}
#' 
#' @family Utilities
#' 
#' @examples
#' \dontrun{
#' # Basic construction:
#' constructor()
#' constructor("net=1","sta=2")
#' #
#' # parameter flattening
#' params2queryparams(net=1,sta=2)
#' params2queryparams(net=1,sta=NULL)
#' # this only adds parameters
#' params2queryparams(a=1, b=2, defaults=c(a=1, b=1,c=2))
#' # this adds mandatory/optional (TRUE/FALSE) parameters:
#' params2queryparams(a=1, b=2, defaults=c(a=TRUE, b=FALSE))
#' # missing optionals are excluded by default
#' params2queryparams(a=1, defaults=c(a=TRUE, b=FALSE, c=TRUE))
#' # include them:
#' params2queryparams(a=1, defaults=c(a=TRUE, b=FALSE, c=TRUE), exclude.empty.options=FALSE)
#' #
#' # Constrained construction:
#' Q <- constructor2(net=1, sta=2) 
#' print(Q)  # note the 'MISSING.MANDATORY' field values
#' # check that it's valid:
#' try(check.query(Q))  # it's not.
#' #
#' # Another... Distaz
#' # What needs to be given though??
#' constructor2(service="distaz", list.fields.only=TRUE)
#' # fill them in...
#' Q <- constructor2(stalat=45, stalon=-120, evtlat=30.0, evtlon=-100.0, service="distaz")
#' print(Q)
#' # check that it's valid:
#' try(check.query(Q))  # it is.
#' #
#' # 'endtime' is an optional default, by default, and also 
#' not recognized if it is NULL
#' all.equal(constructor2(), constructor2(endtime=NULL))
#' }
constructor <- function(..., 
                        service=c("timeseries","distaz","traveltime","flinnengdahl"),
                        ws.version=c("1","2")){
    #
    # service here DOES need to match iris specification
    service <- match.arg(service)
    #
    ver <- match.arg(ws.version)
    #
    service.iris.edu <- "http://service.iris.edu/irisws"
    query <- "query?"
    irisquery <- paste(service.iris.edu, service, ver, query, sep="/")
    query <- paste0(irisquery, paste(..., sep="&"))
    #
    return(query)
}
#' @rdname constructor
#' @export
constructor2 <- function(..., 
                         service=c("timeseries","distaz","tt.deg","tt.km","flinnengdahl"), 
                         list.fields.only=FALSE, ws.version="1"){
    #
    # service here does NOT need to match iris specification
    service <- match.arg(service)
    #
    # minimum defaults
    #   FALSE fields are considered optional (was NULL)
    #   TRUE fields are considered mandatory
    mandatory <- TRUE
    optional <- FALSE
    if (service=="timeseries"){
        #http://service.iris.edu/irisws/timeseries/1/
        #
        opts <- list(...)
        #
        # default: endtime is optional
        d.et <- optional
        # and duration is mandatory
        d.dur <- mandatory
        #
        if ("endtime" %in% names(opts)){
            # endtime was given
            if (!is.null(opts[["endtime"]])){
                # and it was not null, so
                # endtime is mandatory
                d.et <- mandatory
                # duration is optional
                d.dur <- optional
            }   
        }
        mlst <- list(#rqd:
                     net=mandatory, sta=mandatory, loc=mandatory, cha=mandatory, 
                     starttime=mandatory, 
                     # these are either/or
                     endtime=d.et, 
                     duration=d.dur, 
                     # filter options: (order matters!)
                     taper=optional, envelope=optional,
                     lpfilter=optional, hpfilter=optional, bpfilter=optional,
                     demean=optional, differentiate=optional, integrate=optional,
                     scale=optional, divscale=optional, correct=optional,
                     freqlimits=optional, autolimits=optional, units=optional,
                     decimate=optional, antialiasplot=optional, 
                     audiocompress=optional, audiosamplerate=optional,
                     # and rqd:
                     output=mandatory)
    } else if (service=="flinnengdahl"){
        #http://service.iris.edu/irisws/flinnengdahl/2/
        mlst <- list(lat=mandatory, lon=mandatory, output=mandatory)
    } else if (service=="distaz"){
        #http://service.iris.edu/irisws/distaz/1/
        mlst <- list(stalat=mandatory, stalon=mandatory, evtlat=mandatory, evtlon=mandatory)
    } else if (service=="tt.deg"){
        #http://service.iris.edu/irisws/traveltime/1/
        # where distance is epicentral degrees
        #/query? (distdeg=<degrees>) [evdepth=<km>] [model=<iasp91|prem|ak135>] [phases=<phaselist>] [output-params]
        service <- "traveltime"
        mlst <- list(distdeg=mandatory, evdepth=optional, model=optional, phases=optional,
                     noheader=optional, traveltimeonly=optional, 
                     rayparamonly=optional, mintimeonly=optional)
    } else if (service=="tt.km"){
        #http://service.iris.edu/irisws/traveltime/1/
        # where distance is kilometers
        #/query? (distdeg=<km>) [evdepth=<km>] [model=<iasp91|prem|ak135>] [phases=<phaselist>] [output-params]
        service <- "traveltime"
        mlst <- list(distkm=mandatory, evdepth=optional, model=optional, phases=optional,
                     noheader=optional, traveltimeonly=optional, 
                     rayparamonly=optional, mintimeonly=optional)
    }
    
    if (list.fields.only){
        ## Return only the field names
        defs <- unlist(mlst)
        optional.fields <- !defs
        defsn <- names(defs)
        f.req <- defsn[!optional.fields]
        f.opt <- defsn[optional.fields]
        query <- list(required.fields=f.req, optional.fields=f.opt)
    } else {
        ## or the actual query
        qparams <- params2queryparams(..., defaults=mlst, exclude.empty.options=TRUE, exclude.null.fields=TRUE)
        query <- constructor(qparams, service=service, ws.version=ws.version)
    }
    #print(query)
    return(query)
}

#' @rdname constructor
#' @export
params2queryparams <- function(..., defaults, exclude.empty.options=TRUE, exclude.null.fields=TRUE){
    # creates a list of parameters: e.g., a, b
    plist <- list(...)
    #print(plist)
    if (missing(defaults)){
        defaults <- list()
    } else {
        if (!inherits(defaults, "list")){
            defaults <- as.list(defaults)
            warning("'defaults' was not of class 'list' and was therfore coerced to one: values may have been changed")
        }
    }
    if (exclude.null.fields){
        plist <- plist[!sapply(plist, is.null)]
        defaults <- defaults[!sapply(defaults, is.null)]
    }
    params <- RCurl::merge.list(plist, defaults)
    param.names <- names(params)
    # flattens to strings with, e.g., "a=1", "b=1", etc
    miss.opt <- "MISSING.OPTIONAL"
    miss.mand <- "MISSING.MANDATORY"
    not.applic <- "NOT.APPLICABLE"
    Eparams <- sapply(param.names, function(Pname, Dat=params) { 
        val <- Dat[[Pname]]
        ##print(val)
        if (!is.na(val)){
            if (val=="TRUE"){
                # mandatory, but missing
                val <- miss.mand
            } else if (val=="FALSE"){
                # optional, and missing
                val <- miss.opt
            }
        } else {
            val <- not.applic
        }
        #print(c(Pname,val))
        paste0(Pname, "=", val)
        })
    # collapse them into a single line
    #print(Eparams)
    if (exclude.empty.options){
        optionals <- grepl(pattern=miss.opt, Eparams) 
        Eparams <- Eparams[!optionals]
    }
    #print(Eparams)
    Qparams <- paste(Eparams, collapse="&")
    return(Qparams)
}

#' Perform a query to IRIS-WS
#' @details
#' This function uses \code{\link{curlPerform}} in the \strong{RCurl} package
#' to query the IRIS WS. 
#' Firstly, it
#' checks \code{query} 
#' for internal consistency, with \code{\link{check.query}};
#' then, \code{query} is checked externally with \code{\link{url.exists}}.
#'  If \code{query} passes the checks, then it is sent to the IRIS WS
#' and successfull results are written to \code{filename} (which is instantiated
#' with \code{\link{CFILE}}).
#' 
#' \code{\link{iris.query}} is simply a pointer to \code{\link{query.iris}}
#' 
#' @author AJ Barbour
#' @export
#' 
#' @param iquery character; the web service query
#' @param filename character; the file to save query results to.  
#' If this is \code{NULL} a
#' temporary file from \code{\link{tempfile}} is used.
#' @param is.binary logical; will the output be binary? (e.g., \code{TRUE} for SAC binary, and \code{FALSE} for a plot)
#' @param check logical; should \code{\link{check.query}} be used to check the quality of \code{iquery}
#' @param verbose logical; should messages be given by this function, and \code{\link{curlPerform}}?
#' @param ... additional arguments to \code{\link{curlPerform}}
#' 
#' @return A list (invisibly) with the filename, and the query string
#' 
#' @seealso 
#' \code{\link{irisws-package}}
#' 
#' @family Utilities
#' 
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
#' #
#' # Arbitrary query validation
#' try(check.query(params2queryparams(a=1, defaults=list(a=TRUE))))  # succeeds
#' try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=NA)))) # fails
#' try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=FALSE)))) # succeeds
#' try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=FALSE),exclude.empty.options=FALSE))) # fails
#' try(check.query(params2queryparams(a=1, defaults=list(a=TRUE,b=2,c=TRUE)))) # fails
#' }
query.iris <- function(iquery, filename="iris.query.results", is.binary=FALSE, check=TRUE, verbose=TRUE, ...){ 
    if (check) check.query(iquery)
    ure <- RCurl::url.exists(iquery)
    if (ure){
        if (is.null(filename)){
            filename <- tempfile('iriswsQ.query.results')
        }
        #
        md <- "w"
        if (is.binary){ md <- paste0(md,"b")}
        lf = RCurl::CFILE(filename, mode=md)
        RC <- RCurl::curlPerform(url = iquery, writedata = lf@ref, verbose=verbose, ...)
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
check.query <- function(iquery){
    QQ <- unlist(strsplit(as.character(iquery), "query?"))
    nq <- length(QQ)
    #[1] "http://service.iris.edu/irisws/timeseries/1/" "net=1&sta=2&loc=MISSING.MANDATORY&..."
    Q <- QQ[nq]
    Qs <- unlist(strsplit(Q,"&"))
    grtests <- c(
        gr1 <- grepl(pattern="MISSING.MANDATORY", Qs),
        gr2 <- grepl(pattern="MISSING.OPTIONAL", Qs),
        gr3 <- grepl(pattern='=$', Qs), # empty field
        gr4 <- grepl(pattern=' $', Qs), # white space at end
        gr5 <- grepl(pattern="NOT.APPLICABLE", Qs)
    )
    #[1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    if (any(grtests)){
        offenders <- unique(c(Qs[gr1],Qs[gr2],Qs[gr3],Qs[gr4],Qs[gr5]))
        stop(paste("Invalid query:", paste(offenders, collapse=" ")))
    }
    return(invisible(list(Q=iquery, Qs=Qs)))
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
#' 
#' @export
#' @author AJ Barbour
#' @references [1] \url{http://service.iris.edu/irisws/timeseries/1/}
#' 
#' @seealso 
#' \code{\link{irisws-package}}
#' 
#' @family Utilities
#' 
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
