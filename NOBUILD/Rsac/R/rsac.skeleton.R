"rsac.skeleton" <- function(n)
{
  data(srosa)
  ne <- length(names(srosa[[1]]))
  ele <- list()
  for(i in 1:ne)
    ele[i] <- NA
  names(ele) <- names(srosa[[1]])
  # Some need to be zero rather than NA
  i.zero <- c(7, 27:32)
  for(i in i.zero)
    ele[i] <- 0
  x <- vector(mode = "list", length = n)
  for(i in 1:n)
    x[[i]] <- ele
  class(x) <- "rsac"
  x
}
