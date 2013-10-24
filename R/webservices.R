#' Access to the 'timeseries' Web Service for obtaining continuous data
#' 
#' @description
#' From [1]: \emph{The ws-timeseries service provides access to individual
#'  channels of time series data for a specified time range. Requested 
#'  segments may be up to 30 days in length and optional signal processing 
#'  may be applied to return data. The time series may be returned in a 
#'  variety of formats.}
#' 
#' @details
#' Signal processing options:
#' \itemize{
#' \item{high, low and band-pass filter (\code{})}
#' \item{remove mean value (\code{})}
#' \item{scaling by constant value (\code{})}
#' \item{deconvolution of instrument response (with frequency limits and unit conversion)  (\code{})}
#' \item{differentiation and integration  (\code{})}
#' \item{decimation to lower sample rates  (\code{})}
#' }
#' 
#' Output format options :
#' \itemize{
#' \item{miniSEED}
#' \item{SAC Alpha (ASCII)}
#' \item{SAC binary (big & little endian)}
#' \item{Audio (WAV format)}
#' \item{PNG Plot}
#' \item{ASCII (data column)}
#' \item{ASCII (data column, time column)}
#' }
#' The output options can be specifiec with
#' (\code{output=})
#' \code{miniseed}, \code{saca}, \code{sacbb}, \code{sacbl}, \code{plot}, 
#' \code{ascii}, \code{ascii1}, \code{ascii2}, \code{audio}
#' 
#' @author AJ Barbour
#' @export
#' 
#' @param net character; the network code
#' @param sta character; the station code
#' @param loc character; the location code
#' @param cha character; the channel code
#' @param startdate character; the beginning of the record
#' @param duration numeric; the length of the record, in seconds
#' @param ... additional query parameters
#' 
#' @return list
#' 
#' @references [1] \url{http://service.iris.edu/irisws/timeseries/1/}
#' 
#' @family WebServices
#' 
ws.timeseries <- function(net, sta, loc, cha, 
                          startdate, duration, ...){
    # /query? (channel-options) (date-range-options) (filter-options) [plot-options] [audio-options] (output-options)
    # where:
    #   channel-options      ::  (net=<network> & sta=<station> & loc=<location> & cha=<channel>)
    #   date-range-options   ::  (starttime=<time>) & ([endtime=<time>] | [duration=<seconds>])
    #   filter-options       ::  [taper=WIDTH,TYPE] [envelope] [lpfilter=FREQ] [hpfilter=FREQ]
    #                            [bpfilter=HIFREQ-LOFREQ] [demean=<true|false>] 
    #                            [diff=<true|false>] [int=<true|false>] [scale=number|AUTO] 
    #                            [divscale=number] [correct=<true|false>] [freqlimits=F1-F2-F3-F4] 
    #                            [autolimits=lowerdBdown-upperdBdown] 
    #                            [units=<DEF|DIS|VEL|ACC>] [decimate=SAMPLERATE]
    #   plot-options         ::  [antialiasplot=<true|false>]
    #   audio-options        ::  [audiocompress=<true|false>] [audiosamplerate=<playback-rate-hz>]
    #   output-options       ::  (output=<miniseed|saca|sacbb|sacbl|plot|ascii|ascii1|ascii2|audio>)
    #   
    #   (..) required
    #   [..] optional   
}

#' @rdname ws.timeseries
#' @export
timeseries.ws <- ws.timeseries

#' Access the 'distaz' Web Service for distance/azimuth computations
#' 
#' @description
#' From [1]: \emph{The distance-azimuth service will calculate the great-circle 
#' angular distance, azimuth, and backazimuth between two geographic 
#' coordinate pairs. All results are reported in degrees, with azimuth 
#' and backazimuth measured clockwise from North.}
#' 
#' @details
#' \code{\link{distaz.ws}} is simply a pointer to \code{\link{ws.distaz}}
#' 
#' @author AJ Barbour
#' @export
#' 
#' @param station.latlon numeric; the decimal latitude and longitude of the station
#' @param event.latlon numeric; the decimal latitude and longitude of the event (source)
#' @param verbose logical; should messages be given?
#' @return list
#' 
#' @references [1] \url{http://service.iris.edu/irisws/distaz/1/}
#' 
#' @family WebServices
#' 
#' @examples
#' \dontrun{
#' ws.distaz()
#' ws.distaz(c(0.,0.),c(30.,-100.))
#' }
ws.distaz <- function(station.latlon=c(0.,0.), event.latlon=c(0.,0.), verbose=FALSE){
    #
    sta <- as.numeric(station.latlon)
    stopifnot(length(sta) == 2)
    evt <- as.numeric(event.latlon)
    stopifnot(length(evt) == 2)
    #
    Q <- constructor2(stalat=sta[1], stalon=sta[2], evtlat=evt[1], evtlon=evt[2], service="distaz")
    res <- query.iris(Q, filename=NULL, verbose=verbose)
    #
    xmlfi <- res[["file"]]
    if (!verbose){
        sink(file=tempfile())
    } # sink because cannot turn off cat
    dat <- XML2R::XML2R(xmlfi)
    if (!verbose){
        sink()
    }
    #     $`DistanceAzimuth//azimuth`
    #     XML_value url_key
    #     [1,] "319.053" "url1" 
    #     
    #     $`DistanceAzimuth//backAzimuth`
    #     XML_value   url_key
    #     [1,] "126.74508" "url1" 
    #     
    #     $`DistanceAzimuth//distance`
    #     XML_value  url_key
    #     [1,] "21.73182" "url1" 
    selec <- c("azimuth","backAzimuth","distance")
    dat <- dat[seq_along(selec)]
    names(dat) <- selec
    dat <- lapply(dat, function(x) as.numeric(x[1]))
    dat$query <- Q
    return(dat)
}

#' @rdname ws.distaz
#' @export
distaz.ws <- ws.distaz

#' Access the 'traveltime' Web Service for traveltime computations
#' 
#' @description
#' From [1]: \emph{The traveltime webservice calculates travel-times for 
#' seismic phases using a 1-D spherical earth model.}
#' There are three ways to do this computation: 
#' (1) \code{\link{ws.ttDeg}} for epicentral distances specified in decimal degrees;
#' (2) \code{\link{ws.ttKm}}, for epicentral distances in kilometers; and,
#' (3) \code{\link{ws.ttStaSrc}} for epicentral distances specified
#' by the latitudes and longitudes of the station/source pair.
#' 
#' @details
#' For distance calculations \code{\link{ttDeg.ws}} is the primary
#' function; both \code{\link{ws.ttDeg}} and \code{\link{ws.ttKm}} are
#' wrapper functions for calculations using degrees and kilometers, respectively.  
#' Note that \code{\link{ttDeg.ws}} is simply a pointer to \code{\link{ws.ttDeg}} and
#' similarly for \code{\link{ttKm.ws}} and \code{\link{ttStaSrc.ws}} 
#' 
#' It is advisable \emph{not} to turn on \code{traveltime.only=TRUE}
#' or \code{rayparam.only=TRUE} \emph{unless} a vector of \code{phases}
#' is given; this is because the IRIS WS does not return the phase list
#' if these options are enabled, so the numbers returned
#' will be essentially meaningless (that is, if \code{phases} is not set).
#' 
#' \emph{Note that parameter descriptions were adapted from [1], and the
#' defaults used are following [1].}
#' 
#' @name traveltime
#' @author AJ Barbour
#' 
#' @param distances numeric; great-circle distance from source to station.
#' Specify multiple distances as a vector (e.g., \code{c(1,20,30)})
#' @param distance.units character; the units of \code{distances}, either
#' in decimal degrees or kilometers.
#' @param depth numeric; the depth of the event, in kilometers.
#' @param model character; Name of 1–D earth velocity model to be used. 
#' Available models include: 
#' \code{iasp91} by the Int’l Assoc of Seismology and Physics of the Earth’s Interior,
#' \code{prem } the Preliminary Reference Earth Model,
#' and \code{ak135} by Kennett B.L.N., Engdahl E.R. and Buland R. (1995). (See [2].)
#' @param phases character; Comma separated list of phases, 
#' defaulting to \code{p,s,P,S,Pn,Sn,PcP,ScS,Pdiff,Sdiff,PKP,SKS,PKiKP,SKiKS,PKIKP,SKIKS}
#' @param no.header logical; suppresses header from the resulting table
#' @param traveltime.only logical; returns a space–separated list of travel times, in seconds. 
#' \emph{Travel times are produced in ascending time order regardless 
#' of the order in which the phases are specified.}
#' @param rayparam.only logical; will return a space-separated list of ray parameters, in sec/deg.
#' @param mintime.only logical; will only retrieve the first arrival of each phase for each distance
#' @param verbose logical; should messages be given?
#' @param ... additional parameters to \code{\link{ws.ttDistances}}
#' @return A list (invisibly)
#' 
#' @references [1] \url{http://service.iris.edu/irisws/traveltime/1/}
#' @references [2] \url{http://www.iris.edu/dms/products/emc-referencemodels/}
#' 
#' @family WebServices
#' 
#' @examples
#' \dontrun{
#' #
#' # In epicentral degrees
#' wsdeg1 <- ws.ttDistances(c(0,10,20,30,40), verbose=TRUE)
#' wsdeg2 <- ws.ttDeg(c(0,10,20,30,40), verbose=TRUE)
#' all.equal(wsdeg1, wsdeg2)
#' #
#' # In kilometers
#' ws.ttKm(c(0,10,20,30,40), verbose=TRUE)
#' }
NULL

#' @rdname traveltime
#' @export
ws.ttDistances <- function(distances, distance.units=c("degrees","kilometers"),
                     depth=0, model=c('iasp91','prem','ak135'), phases=NULL, 
                     no.header=FALSE, traveltime.only=FALSE, rayparam.only=FALSE, mintime.only=FALSE, 
                     verbose=FALSE){
    # /query? (distdeg=<degrees>) [evdepth=<km>] [model=<iasp91|prem|ak135>] [phases=<phaselist>] [output-params]
    # where:
    #   output-params     ::    [noheader=<true|false>] 
    #                           [traveltimeonly=<true|false>]
    #                           [rayparamonly=<true|false>]
    #                           [mintimeonly=<true|false>]
    #   
    #   (..) required
    #   [..] optional
    model <- match.arg(model)
    #
    distance.units <- match.arg(distance.units)
    distlist <- paste(as.character(distances),collapse=",")
    #
    if (!is.null(phases)){
        phaselist <- paste(as.character(phases),collapse=",")
    } else {
        phaselist <- phases
    }
    #
    IE <- function(x) ifelse(x, "true", "false")
    NH <- IE(no.header)
    TTO <- IE(traveltime.only)
    RPO <- IE(rayparam.only)
    MTO <- IE(mintime.only)
    #
    if (distance.units=="degrees"){
        deg.flag <- TRUE
        Q <- constructor2(distdeg=distlist, evdepth=depth, model=model, phases=phaselist, 
                          noheader=NH, traveltimeonly=TTO, rayparamonly=RPO, mintimeonly=MTO,
                          service="tt.deg")
    } else if (distance.units=="kilometers"){
        deg.flag <- FALSE
        Q <- constructor2(distkm=distlist, evdepth=depth, model=model, phases=phaselist, 
                          noheader=NH, traveltimeonly=TTO, rayparamonly=RPO, mintimeonly=MTO,
                          service="tt.km")
    }
    stopifnot(exists("Q"))
    res <- query.iris(Q, filename=NULL, verbose=verbose)
    fi <- res[["file"]]
    if (verbose) system(paste("cat",fi))
    #
    # regular and mintime.only
    #Model: iasp91
    #Distance   Depth   Phase   Travel    Ray Param  Takeoff  Incident  Purist    Purist
    #  (deg)     (km)   Name    Time (s)  p (s/deg)   (deg)    (deg)   Distance   Name 
    #-----------------------------------------------------------------------------------
    #0.00     0.0   P          0.00    19.172     90.00    90.00     0.00   = P
    #0.00     0.0   S          0.00    33.094     90.00    90.00     0.00   = S
    #
    # no.header == TRUE (mintime.only too)
    #0.00     0.0   P          0.00    19.172     90.00    90.00     0.00   = P
    #0.00     0.0   S          0.00    33.094     90.00    90.00     0.00   = S
    #
    # traveltime.only == TRUE & rayparam.only == TRUE (regardless of no.header)
    #0.00000 0.00000
    # (note, tt has precedence)
    if (traveltime.only | rayparam.only){
        dat <- scan(fi, nlines=1, quiet=!verbose)
    } else {
        nskip <- ifelse(no.header,0,4)
        cn1 <- ifelse(deg.flag, "dist.deg", "dist.km")
        dat <- read.table(fi, header=FALSE,
            col.names=c(cn1,"depth.km","phase","traveltime.s",
                        "slowness.p","takeoffAng.deg","incidentAng.deg",
                        "distance.purist","xxx","phase.purist"),
            skip=nskip)
        xxx <- NULL
        dat <- subset(dat,select=-c(xxx))
    }
    dat <- list(phases=phaselist, traveltime.data=dat, query=Q)
    return(invisible(dat))
}

#' @rdname traveltime
#' @export
ws.ttDeg <- function(distances, ...) ws.ttDistances(distances, distance.units="degrees", ...)

#' @rdname traveltime
#' @export
ttDeg.ws <- ws.ttDeg

#' @rdname traveltime
#' @export
ws.ttKm <- function(distances, ...) ws.ttDistances(distances, distance.units="kilometers", ...)

#' @rdname traveltime
#' @export
ttKm.ws <- ws.ttKm

#' @rdname traveltime
#' @export
ws.ttStaSrc <- function() .NotYetImplemented()

#' @rdname traveltime
#' @export
ttStaSrc.ws <- ws.ttStaSrc
