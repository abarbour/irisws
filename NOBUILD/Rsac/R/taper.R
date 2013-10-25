"taper" <- function(x, type = c("hanning", "hamming", "cosine"),
                    width = 0.05)
{
  type <- match.arg(type)
  for(i in 1:length(x))
  {
    xx <- x[[i]]$amp
    N <- length(xx)
    n <- round(N * width, 0)
    taper <- rep(1, N)
    taper[1:n] <- switch(type,
                         hanning = 0.5 - 0.5 * cos(pi/n * ((1:n)-1)),
                         hamming = 0.54 - 0.46 * cos(pi/n * ((1:n)-1)),
                         cosine = 1 - cos(pi/2/n * ((1:n)-1))
                        )
    taper <- taper * rev(taper)
    x[[i]]$amp <- xx * taper
  }
  return(x)
}
