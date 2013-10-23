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
#' @export
#' @author AJ Barbour
#' 
#' @references \url{http://pages.stern.nyu.edu/~dbackus/BCH/data/FRED/fredplot.R}
#' was the motivation for \code{\link{params2queryparams}}.
#' 
#' @seealso \code{\link{iris.query}} to query IRIS WS
#' 
#' @examples
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
#' 
constructor <- function(..., service=c("timeseries")){
    #
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
constructor2 <- function(..., service=c("timeseries"), list.fields.only=FALSE){
    #
    service <- match.arg(service)

    # minimum defaults
    if (service=="timeseries"){
        #http://service.iris.edu/irisws/timeseries/1/
        # NULL fields will be excluded
        mlst <- list(net=NA, sta=NA, loc=NA, cha=NA, 
                     starttime=NA, endtime=NULL, duration=NA, 
                     taper=NULL, 
                     envelope=NULL,
                     lpfilter=NULL,
                     hpfilter=NULL,
                     bpfilter=NULL,
                     demean=NULL,
                     diff=NULL,
                     int=NULL,
                     scale=NULL,
                     divscale=NULL,
                     correct=NULL,
                     freqlimits=NULL,
                     autolimits=NULL,
                     units=NULL,
                     decimate=NULL,
                     antialiasplot=NULL,
                     audiocompress=NULL,
                     audiosamplerate=NULL,
                     output=NA)
        optional <- sapply(mlst, is.null) # field numbers
    } 
    if (list.fields.only){
        defs <- names(mlst)
        f.req <- defs[!optional]
        f.opt <- defs[optional]
        query <- list(required.fields=f.req, optional.fields=f.opt)
    } else {
        qparams <- params2queryparams(..., defaults=mlst, exclude.nulls=TRUE)
        query <- constructor(qparams, service=service)
    }
    return(query)
}
#' @rdname constructor
#' @export
params2queryparams <- function(..., defaults, exclude.nulls=FALSE){
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
query.iris <- function(iquery, filename="iris.query.results", is.binary=FALSE, verbose=TRUE, ...){ 
    ure <- RCurl::url.exists(iquery)
    if (ure){
        if (is.null(filename)){
            filename <- tempfile('iris.query.results')
        }
        md <- "w"
        if (is.binary){ md <- paste0(md,"b")}
        lf <- RCurl::CFILE(filename, mode=md)
        RCurl::curlPerform(url = iquery, writedata = lf@ref, ...)
        if (verbose) message(sprintf("IRIS WS query complete:  %s", filename))
        return(invisible(list(file=filename, query=iquery)))
    } else {
       stop("IRIS WS query failed.")
    }
}
#' @export
#' @rdname query.iris
iris.query <- query.iris
