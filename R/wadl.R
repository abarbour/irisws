#' Accessing IRIS WS services with .wadl protocol
#' @description
#' WADL is the Web Applications Description Language, which
#' is essentially a way to specify WS parameters 
#' through dressed up XML
#' 
#' @param service character
#' @param u the full url
#' @param x object to test, describe, or query-construct with
#' @param ... additional objects
#' 
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
#' @examples
#' wd <- waddler("timeseries")
#' class(wd)
#' is.iriswadl(wd)
#' # print some information and return query parameters
#' describe(wd)
NULL

#' @rdname irisws-wadl
#' @export
waddler <- function(service, ...){
    wadl.xml <- constructor("application.wadl", service=service, query.field="", ...)
    wml <- wadl(wadl.xml)
    attr(wml, "service") <- service
    class(wml) <- "iriswadl"
    return(wml)
}
#' @rdname irisws-wadl
#' @export
wadl <- function(u, ...){
    XML2R(u, df=TRUE)
}

#' @rdname irisws-wadl
#' @export
is.iriswadl <- function(x, ...) inherits(x, what="iriswadl", ...)
#' @rdname irisws-wadl
#' @export
constructor2.iriswadl <- function(x, ...){
  args <- suppressMessages(describe(x))
  anms <- names(args)
  rqd <- ifelse("required" %in% anms, TRUE, FALSE)
  def <- ifelse("default" %in% anms, TRUE, FALSE)
  serv <- attr(x, "service")
  c(rqd, def)
}

#' @rdname irisws-wadl
#' @export
describe <- function(x, ...) UseMethod("describe")
#' @rdname irisws-wadl
#' @export
describe.iriswadl <- function(x, ...){
  nms <- names(x)
  parami <- grep(pattern="//request//param$",nms)
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
  message(msg, ...)
  return(as.data.frame(params))
}