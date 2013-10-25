"iir" <- function(x, ord = 3, fl = NA, fh = NA,
                  type = c("LP", "HP", "BP", "BR"),
                  proto = c("BU", "BE", "C1", "C2"),
                  zerophase = TRUE,
                  ca = NA, ctr = NA)
{
  proto <- match.arg(proto)
  type <- match.arg(type)
  if(ord >= 10)
    stop("\'ord\' must be less than 10.\n")
  if(type == "HP" & is.na(fl))
    stop("\'fl\' must be provided for type \'HP\'.\n")
  if(type == "LP" & is.na(fh))
    stop("\'fh\' must be provided for type \'LP\'.\n")
  if((type == "BP" | type == "BR")  &
     (is.na(fl) | is.na(fh) ) )
    stop("Must provide both \'fh\' or \'fl\'.\n")
  if(class(x) != "rsac")
    stop("\'x\' must be class \'rsac\'.\n")
  # SAC defaults:
  if(is.na(ca))
    ca <- 30
  if(is.na(ctr))
    ctr <- 0.3
  # can't send NA to .C() even if it isn't used
  if(is.na(fl))
    fl <- 1
  if(is.na(fh))
    fh <- 1
  for(i in 1:length(x))
  {
    # float *input,      
    # int *na,
    # int *iord,         number of poles for filter (< 10; preferably < 5)
    # int *zp            logical for zero phase filtering
    # char **type,       Character*2 variable containing filter type
    # char **aproto,     Character*2 variable designating analog prototype
    # double *aa,        Chebyshev stop band attenuation (ignored for others)
    # double *atrbndw,   Chebyshev transition bandwidth (ignored for others)
    # double *afl,       low pass corner freq. (ignored if type HP)
    # double *afh,       high pass corner freq. (ignored if type LP)
    # double *ats,       time sample interval in secs (e.g.,0.01 = 100 samp/sec)
    # float  *output)    
    Cout <- .C("jfilt", PACKAGE = "Rsac",
               input = as.single(x[[i]]$amp),
               na = as.integer(length(x[[i]]$amp)),
               iord = as.integer(ord),
               zp = as.integer(zerophase),
               type = as.character(type),
               aproto = as.character(proto),
               aa = as.double(ca),
               atrbndw = as.double(ctr),
               afl = as.double(fl),
               afh = as.double(fh),
               ats = as.double(x[[i]]$dt),
               output  = single(length(x[[i]]$amp)))
    x[[i]]$amp <- Cout$output
  }
  class(x) <- "rsac"
  return(x)
}

"lp" <- function(x, c, n = 3, proto = "BU", zerophase = TRUE)
  iir(x, ord = n, fh = c, type = "LP", proto = proto, zerophase = zerophase)

"hp" <- function(x, c, n = 3, proto = "BU", zerophase = TRUE)
  iir(x, ord = n, fl = c, type = "HP", proto = proto, zerophase = zerophase)

"bp" <- function(x, c, n = 3, proto = "BU", zerophase = TRUE)
{
  if(length(c) != 2)
    stop("Must provide two corner frequenies.\n")
  c <- sort(c)
  iir(x, ord = n, fl = c[1], fh = c[2], type = "BP",
      proto = proto, zerophase = zerophase)
}






