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
    wml <- XML2R(wadl.xml, df=TRUE)
    class(wml) <- "iriswadl"
    wml
}
#' @rdname irisws-wadl
#' @export
wadl <- waddler
#' @rdname irisws-wadl
#' @param x object to test, describe, or query-construct with
#' @param ... additional objects
#' @export
is.iriswadl <- function(x, ...) inherits(x, what="iriswadl", ...)
#' @rdname irisws-wadl
#' @S3method constructor2 iriswadl
constructor2.iriswadl <- function(x, ...){
  wdf <- x[[4]]
  
}
#' @rdname irisws-wadl
#' @export
describe <- function(x, ...) UseMethod("describe")
#' @rdname irisws-wadl
#' @S3method describe iriswadl
describe.iriswadl <- function(x, ...){
  nms <- names(x)
  parami <- grep(pattern="//request//param$",names(eop))
  if (length(parami)>1) stop("Multiple '...//request//param' children? Check the validity of the wadl file.")
  params <- subset(x[[parami]], select=-c(url))
  desc <- x[['application//doc']]
  nd <- colnames(desc)
  desc2 <- paste(desc[,!c(nd %in% c("lang","url"))], collapse="\n")
  resour <- x[['application//resources']]
  msg <- paste(paste(strwrap(desc2, width=60), collapse="\n"),"",
               "+++++++  Web-service & WADL urls:","",
                resour[[1]],resour[[2]],"",
               "+++++++  Parameters:\n","",sep="\n")
  cat(msg, ...)
  print(params)
}