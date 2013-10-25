"getunits" <- function(x)
{
  getint <- function(X) X$units
  val <- sapply(x, getint)
  if(all(mode(val) == "numeric"))
  {
    val[val == 5] <- "Unknown"
    val[val == 6] <- "Displacement in nm"
    val[val == 7] <- "Velocity in nm/sec"
    val[val == 8] <- "Acceleration in nm/sec/sec"
    val[val ==50] <- "Velocity in volts"
  }
  return(val)
}
