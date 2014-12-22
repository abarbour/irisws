#' @title Access to IRIS Web Services within R
#' 
#' @description
#' 
#' This package can be used to obtain products from the
#' Data Management Center  of the
#' Incorporated Research Institutions in Seismology [1],
#' through their web-services API [2].
#' 
#' Currenty there is support for access to
#' \code{\link{Distance-Azimuth}} calculations,
#' xxx, ...
#' 
#' @details
#' The code constructs queries according to
#' the requirements of the API, executes them, and
#' optionally saves and/or loads the results into
#' the R environment.
#' A good place to start is with the \code{\link{webservices}}
#' function.
#' 
#' @docType package
#' @name irisws-package
#' @aliases irisws
#' @author A.J. Barbour
#' 
#' @references [1] \url{http://www.iris.edu/}
#' @references [2] \url{http://service.iris.edu/irisws}
#' 
#' @seealso 
#' \code{\link{irisws-webservices}} to see a list
#' of the webservice accessors currently available
#'  
#' \code{\link{constructor}} \emph{et al.} to construct queries
#' 
#' \code{\link{query.iris}} to query IRIS WS
#' 
#' @family Utilities
#' @examples
#' # see the available functions
#' webservices()
#' 
#' # see which webservices are actually supported
#' services()
NULL

.iriswsEnvName = ".iriswsEnv"
.iriswsEnv = new.env()
.iriswsBaseUrl = "http://service.iris.edu/irisws"
.field.mandatory = 'MISSING.MANDATORY'
.field.optional = "MISSING.OPTIONAL"
.field.na = "NOT.APPLICABLE"
