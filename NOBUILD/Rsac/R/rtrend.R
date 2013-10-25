"rtrend" <- function(x){
  for(i in 1:length(x)){
    xx <- x[[i]]$amp
    j <- 1:length(xx)
    x[[i]]$amp <- residuals(lm(xx ~ j))
  }
  return(x)
}

"rmean" <- function(x){
  for(i in 1:length(x))
    x[[i]]$amp <- x[[i]]$amp - mean(x[[i]]$amp)
  return(x)
}
