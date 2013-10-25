"int" <- function(s)
{
  for(i in 1:length(s))
  {
    dt <- s[[i]]$dt
    x <- s[[i]]$amp
    A <- dt * x
    int <- cumsum(A)
    s[[i]]$amp <- int
  }
  return(s)
}

"dif" <- function(s)
{
  for(i in 1:length(s))
  {
    x <- s[[i]]$amp
    dt <- s[[i]]$dt
    s[[i]]$amp <- diff(x)/dt
    s[[i]]$N <- s[[i]]$N - 1
    s[[i]]$b <- s[[i]]$b + 0.5 * dt
  }
  return(s)
}
