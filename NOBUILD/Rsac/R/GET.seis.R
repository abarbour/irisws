# Calls C-subroutines:
#     o CALL_SETSEIS
#     o CALL_GETSEIS

parseFN2STA <- function(fn)
  {
    # o  get the station name and the component name from the file name
    # o  function assumes that the station name and the component name
    #    are the last  items on the finle name seperated by a period
    # o  first get the file name from the full path name
    # o  could make this more general by adding options 

    f1 <- unlist(strsplit(fn, "/"))
    fn1 <- f1[length(f1)]
    f2 <- unlist(strsplit(fn1, "\\."))
    sta <- f2[length(f2) - 1]
    comp <- f2[length(f2)]
    return(list(sta = sta, comp = comp) )
  }


getmoday <- function(jul, iyear)
{
  if(length(iyear) < length(jul))
    iyear <- rep(iyear, length(jul))
  
  inine <- tojul(iyear, 1, 1)
  ijul <- inine + jul - 1
  MD <- fromjul( ijul, iyear)
  
  return(list(mo = MD$mo, dom = MD$dom))
}

fromjul <- function(jul, yy)
{
  j <- jul - 1721119
  yy <- trunc((4 * j - 1)/146097)
  j <- 4 * j - 1 - 146097 * yy
  dd <- trunc(j/4)
  j <- trunc((4 * dd + 3)/1461)
  dd <- 4 * dd + 3 - 1461 * j
  dd <- trunc((dd + 4)/4)
  mm <- trunc((5 * dd - 3)/153)
  dd <- 5 * dd - 3 - 153 * mm
  dd <- trunc((dd + 5)/5)
  yy <- 100 * yy + j
  
  yy[mm < 10] <- yy[mm < 10]
  yy[mm >= 10] <- yy[mm >= 10] + 1
     
  flg <- mm<10
  mm[flg] <- mm[flg]+3
  mm[!flg] <- mm[!flg] - 9
       
  return(list(mo = mm, dom = dd))	
}
 

tojul <- function(year, month, day)
{
  #  given a year a month and day, return the julian day
  
  yy <- year
  mm <- month
  dd <- day
  jul <- 0

  flg <- mm > 2
  yy[flg] <- yy[flg]
  yy[!flg] <- yy[!flg] - 1
  mm[flg] <- mm[flg] - 3
  mm[!flg] <- mm[!flg] + 9
           
  c <- trunc(yy/100)
  ya <- yy - 100 * c
  jul <- trunc((146097 * c)/4) + trunc((1461 * ya)/4) +
           trunc((153 * mm + 2)/5) + dd + 1721119
  return(jul)
}


"GET.seis" <- function(fnames, kind = 2 ){
  # o gets a bunch of seismic data files from a
  #   directory and store in structure
  # o kind: 1 = segy, 2 = sac, 3 = AH
  
  GIVE <- as.list(1:length(fnames))
  
  ii <- 1
  
  DATIM <- rep(0,length=4)
  n <- 1
  dt <- 0.025000
  sec <- 0
  thesta <- "XXXXX"
  thecomp <- "XXXXX"
  
  for(i in 1:length(fnames))
    {
      fn <- fnames[i]
      infile <- fn
      #   print(fn);
      #   if this file does not exist, exit!
      if(file.exists(infile) == FALSE)
        {
          print(paste(sep = " ", "file does not exist", fn) )
          next
        }
      else
        {
          #  print(paste(sep=' ', "file exists", fn) );
        }
      
      barfa <- .C("CALL_SETSEIS", infile,
                  as.integer(kind),
                  as.integer(n),
                  as.double(dt), 
                  as.integer(DATIM),   
                  as.double(sec),
                  thesta , thecomp,
                  PACKAGE = "Rsac")
      
      N <- barfa[[3]]
      dt <- barfa[[4]]
      DATIM <- barfa[[5]]
      sec <- barfa[[6]]
      
      thesta <- barfa[[7]]
      thecomp <- barfa[[8]]
      
      if(kind == 2)
        {
          if(thesta == "-12345")
            {
              stn <- parseFN2STA(infile)
              thesta <- stn$sta
              thecomp <- stn$comp
            }
        }
      aunits <- "volts"
      #  print(paste(sep=' ', infile, thesta, thecomp, aunits, N, dt, sec))
      
      x <- rep(0, length = N)
      infile <- fn
      
      barf <- .C("CALL_GETSEIS", infile,
                 as.integer(kind),
                 as.double(x),
                 as.integer(n),
                 as.double(dt), 
                 as.integer(DATIM),   
                 as.double(sec),
                 PACKAGE = "Rsac")
      
      x <- barf[[3]]
      N <- barf[[4]]
      dt <- barf[[5]]
      DATIM <- barf[[6]]
      sec <- barf[[7]]
      
      md <- getmoday(DATIM[2], DATIM[1])
      
      t1 <- 0
      t2 <- dt * (N - 1)
      
      tstart <- list(yr = DATIM[1], jd = DATIM[2],
                     mo = md$mo, dom = md$dom,
                     hr = DATIM[3], mi = DATIM[4],
                     sec = sec, msec = 0, dt = dt,
                     t1 = t1, t2 = t2, off = 0)
      
      if(is.null(thesta))
        thesta <- "XXX"
      if(is.null(thecomp))
        thecomp <- "X"
      
      if(is.null(aunits))
        aunits <- "volts"
     
      GIVE[[i]] <- list(amp = x, dt = dt,
                        nzyear = DATIM[1], nzjday = DATIM[2],
                        nzhour = DATIM[3], nzmin  = DATIM[4],
                        nzsec  = sec,
                        nzmsec = 0, # can this be read in?
                        b = t1,     # beginning time (offset)
                        e = t2,     # ending time
                        o = NA, # Event rigin time(sec relative to ref time.)
                        fn = fn, sta = thesta, comp = thecomp,
                        DATTIM = tstart, N = N,
                        units = aunits)
      
    }
  class(GIVE) <- "rsac"
  invisible(GIVE)
  # EMT Notes:
  #    o I changed $amp to $x
  #    o the headers I left the same, but added redundant values
  #      that are consistent with Rsac
}

testgetseis <- function(GG)
{
  for(i in 1:length(GG))
    {
      n <- length(GG[[i]]$x)
      print(paste(sep = " ", i, GG[[i]]$sta, GG[[i]]$comp,
                  GG[[i]]$N, GG[[i]]$DATTIM$dt, n))
      print(paste(sep = " ", i, GG[[i]]$DATTIM$jd, GG[[i]]$DATTIM$hr,
                  GG[[i]]$DATTIM$mi, GG[[i]]$DATTIM$sec))
    }
}
