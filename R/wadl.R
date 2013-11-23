#' Find the WADL file for an IRIS WS
#' @param service character
#' @name irisws-wadl
#' @author A.J. Barbour
#' @references 
#' [1] \url{http://www.w3.org/Submission/wadl/}
#' 
#' @seealso 
#' \code{\link{iris.query}} to query IRIS WS
#' 
#' @seealso \code{\link{irisws-package}}
#' 
#' @family Utilities
#' 
NULL
#' @rdname irisws-wadl
#' @export
waddler <- function(service, ...){
    wadl.xml <- constructor("application.wadl", service=service, query.field="", ...)
    wadl <- XML2R(wadl.xml, df=TRUE)
    class(wadl) <- "irisws.wadl"
    wadl
}
#' @rdname irisws-wadl
#' @export
wadl <- waddler