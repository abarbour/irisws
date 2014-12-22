#
#  httr can make this system more robust, since it returns much more information
#  about the url  (see https://github.com/hadley/httr)
#

    freshest <- FALSE
    if (freshest){
        library(devtools)
        devtools::install_github("hadley/httr")
    }
    library(httr)

#  For example:

    u <- paste0("http://service.iris.edu/irisws/timeseries/1/query?",
         "net=IU&",
         "sta=ANMO&",
         "loc=00&",
         "cha=BHZ&",
         "starttime=2005-01-01T00:00:00&",
         "endtime=2005-01-01T00:01:00&")

    u.ok <- paste0(u, "output=ascii")
    # a bad request (400), whereas errors from bad timespans, network-codes, etc, will return as 404
    u.not_ok <- paste0(u, "output=aski") 
    
    ##GET(u.ok)
    
#Response [http://service.iris.edu/irisws/timeseries/1/query?net=IU&sta=ANMO&loc=00&cha=BHZ&starttime=2005-01-01T00:00:00&endtime=2005-01-01T00:01:00&output=ascii]
#  Date: 2014-12-21 00:51
#  Status: 200
#  Content-type: text/plain
#  Size: 39.4 kB
#TIMESERIES IU_ANMO_00_BHZ_M, 1200 samples, 20 sps, 2005-01-01T00:00:00.010800...
#2005-01-01T00:00:00.010800  24
#2005-01-01T00:00:00.060800  20
#2005-01-01T00:00:00.110800  19
#2005-01-01T00:00:00.160800  19
#2005-01-01T00:00:00.210800  19
#2005-01-01T00:00:00.260800  15
#2005-01-01T00:00:00.310800  10
#2005-01-01T00:00:00.360800  4
#2005-01-01T00:00:00.410800  -4
#...

    ##GET(u.not_ok)
    
#Response [http://service.iris.edu/irisws/timeseries/1/query?net=IU&sta=ANMO&loc=00&cha=BHZ&starttime=2005-01-01T00:00:00&endtime=2005-01-01T00:01:00&output=aski]
#  Date: 2014-12-21 00:52
#  Status: 400
#  Content-type: text/plain;charset=ISO-8859-1
#  Size: 508 B
#Error 400: Not Found
#
#The Timeseries webservice was unable to understand your request. Unrecognized...
#
#Usage details are available from http://service.iris.edu/irisws/timeseries/1
#
#Request:
#http://service.iris.edu/irisws/timeseries/1/query?net=IU&sta=ANMO&loc=00&cha=...
#
#Request Submitted:
#...

    if (!exists("xs")) xs <- GET(u.ok, write_disk("my_output", overwrite=TRUE))
    if (!exists("xf")) xf <- GET(u.not_ok, write_disk("my_output_failed", overwrite=TRUE))

    status_code(xf) < 300 # FALSE: 400 < 300
    status_code(xs) < 300 # TRUE: 200 < 300

#  Could then use tryCatch somehow (modified from ?http_condition):

    f <- function(url) {
     tryCatch(stop_for_status(GET(url)),
         http_400 = function(c.) "Query failed",
         http_403 = function(c.) "You need to authenticate!",
         http_404 = function(c.) "That url doesn't exist",
         http_500 = function(c.) "The server screwed up"
       )
    }

    if (FALSE){
      f(u.ok)
      f(u.not_ok)
      
      f("http://httpbin.org/status/200")
      f("http://httpbin.org/status/400")
      f("http://httpbin.org/status/403")
      f("http://httpbin.org/status/404")
      f("http://httpbin.org/status/505") 
    }

## xs is of class 'response' and has _lots_ of information:

    ##str(xs)
    
#List of 9
# $ url        : chr "http://service.iris.edu/irisws/timeseries/1/query?net=IU&sta=ANMO&loc=00&cha=BHZ&starttime=2005-01-01T00:00:00&endtime=2005-01-"| __truncated__
# $ status_code: int 200
# $ headers    :List of 10
#  ..$ server                     : chr "Apache-Coyote/1.1"
#  ..$ access-control-allow-origin: chr "*"
#  ..$ content-disposition        : chr "attachement; filename=IU.ANMO.00.BHZ.2005.001.00.00.00.000-2005.001.00.01.00.000.txt"
#  ..$ last-modified              : chr "Sun, 21 Dec 2014 07:59:30 GMT"
#  ..$ expires                    : chr "Sun, 21 Dec 2014 08:29:30 GMT"
#  ..$ content-encoding           : chr "gzip"
#  ..$ content-type               : chr "text/plain"
#  ..$ content-length             : chr "5711"
#  ..$ date                       : chr "Sun, 21 Dec 2014 08:48:38 GMT"
#  ..$ connection                 : chr "close"
#  ..- attr(*, "class")= chr [1:2] "insensitive" "list"
# $ all_headers:List of 1
#  ..$ :List of 3
#  .. ..$ status : int 200
#  .. ..$ version: chr "HTTP/1.1"
#  .. ..$ headers:List of 10
#  .. .. ..$ server                     : chr "Apache-Coyote/1.1"
#  .. .. ..$ access-control-allow-origin: chr "*"
#  .. .. ..$ content-disposition        : chr "attachement; filename=IU.ANMO.00.BHZ.2005.001.00.00.00.000-2005.001.00.01.00.000.txt"
#  .. .. ..$ last-modified              : chr "Sun, 21 Dec 2014 07:59:30 GMT"
#  .. .. ..$ expires                    : chr "Sun, 21 Dec 2014 08:29:30 GMT"
#  .. .. ..$ content-encoding           : chr "gzip"
#  .. .. ..$ content-type               : chr "text/plain"
#  .. .. ..$ content-length             : chr "5711"
#  .. .. ..$ date                       : chr "Sun, 21 Dec 2014 08:48:38 GMT"
#  .. .. ..$ connection                 : chr "close"
#  .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
# $ cookies    : list()
# $ content    :Class 'path'  chr "my_output"
# $ date       : POSIXct[1:1], format: "2014-12-21 00:48:39"
# $ times      : Named num [1:6] 0 0.000014 0.00017 0.000217 0.146516 ...
#  ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
# $ request    :List of 5
#  ..$ handle:List of 2
#  .. ..$ handle:Formal class 'CURLHandle' [package "RCurl"] with 1 slot
#  .. .. .. ..@ ref:<externalptr>
#  .. ..$ url   :List of 9
#  .. .. ..$ scheme  : chr "http"
#  .. .. ..$ hostname: chr "service.iris.edu"
#  .. .. ..$ port    : NULL
#  .. .. ..$ path    : chr ""
#  .. .. ..$ query   : NULL
#  .. .. ..$ params  : NULL
#  .. .. ..$ fragment: NULL
#  .. .. ..$ username: NULL
#  .. .. ..$ password: NULL
#  .. .. ..- attr(*, "class")= chr "url"
#  .. ..- attr(*, "class")= chr "handle"
#  ..$ writer:List of 2
#  .. ..$ path: chr "my_output"
#  .. ..$ file: NULL
#  .. ..- attr(*, "class")= chr [1:2] "write_disk" "write_function"
#  ..$ method: chr "GET"
#  ..$ opts  :List of 8
#  .. ..$ followlocation: logi TRUE
#  .. ..$ maxredirs     : int 10
#  .. ..$ encoding      : chr "gzip"
#  .. ..$ cainfo        : chr "/Users/abarbour/src/R/PACKAGES/httr/cacert.pem"
#  .. ..$ useragent     : chr "curl/7.37.1 Rcurl/1.95.4.4 httr/0.6.0.9000"
#  .. ..$ httpheader    : Named chr "application/json, text/xml, */*"
#  .. .. ..- attr(*, "names")= chr "accept"
#  .. ..$ customrequest : chr "GET"
#  .. ..$ url           : chr "http://service.iris.edu/irisws/timeseries/1/query?net=IU&sta=ANMO&loc=00&cha=BHZ&starttime=2005-01-01T00:00:00&endtime=2005-01-"| __truncated__
#  .. ..- attr(*, "class")= chr "config"
#  ..$ body  : NULL
# - attr(*, "class")= chr "response"

query.iris2 <- function(iquery, filename="iris.query.results", is.binary=FALSE, check=TRUE, verbose=TRUE, ...){ 
  if (check) check.query(iquery)      
  if (is.null(filename)){
    filename <- tempfile('iris.query.results')
  }
  RC <- httr::GET(iquery, httr::write_disk(filename, overwrite=TRUE))
  #ure <- RCurl::url.exists(iquery)
  ure <- identical(httr::status_code(RC), 200L)
  qm <- "IRIS WS query"
  if (ure){
    if (verbose) message(sprintf("%s complete:  %s", qm, filename))
  } else{
    bad.params <- iquery  #strsplit(iquery, "query?", fixed = TRUE)[[1]][2]
    if (verbose) message(sprintf("%s   FAILED:  %s", qm, bad.params))
    icall <- match.call()
    icall[['iquery']] <- iquery
    #if (immediate) options(warn = 1) 
    warning(httr::http_condition(RC, type='warning', call=icall))
  }
  
  toret <- list(rc=RC, file=filename, query=iquery, success=ure)
  #assign("last_irisquery", toret, envir=.iriswsEnv)
  #return(invisible(toret))
  return(toret)
}

rc.ok <- query.iris2(u.ok)
rc.not_ok <- query.iris2(u.not_ok)
rc.ok <- query.iris2(u.ok)
    