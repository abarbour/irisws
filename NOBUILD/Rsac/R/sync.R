"sync" <- function(x)
{
  st <- sapply(x, fstart)
  st <- st - min(st)
  ref <- which(st == min(st))[1]
  reft <- list(nzhour = x[[ref]]$nzhour,
               nzmin = x[[ref]]$nzmin,
               nzsec = x[[ref]]$nzsec,
               nzmsec = x[[ref]]$nzmsec,
               b = x[[ref]]$b)
  for(i in 1:length(x))
  {
    x[[i]]$nzhour <- reft$nzhour
    x[[i]]$nzmin <- reft$nzmin
    x[[i]]$nzsec <- reft$nzsec
    x[[i]]$nzmsec <- reft$nzmsec
    x[[i]]$b <- x[[i]]$b + st[i]
    x[[i]]$e <- x[[i]]$e + st[i]
  }
  return(x)
}
