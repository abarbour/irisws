"spect" <- function(x, plot = TRUE, main = NULL, log = "no", ...)
{
  # See spectrum, plot.spec, spec.pgram, in STATS
  sac2ts <- function(X)
    ts(data = X$amp, start = X$b, frequency = 1/X$dt)
  GetTitle <- function(X)
  {
    if(!is.character(X$sta) )
      sta <- NULL
    else
      sta <- gsub(" ", "", X$sta)
    if(!is.character(X$comp) )
      cmp <- NULL
    else
      cmp <- gsub(" ", "", X$comp)
    if(!is.character(X$knetwork) )
      net <- NULL
    else
      net <- gsub(" ", "", X$knetwork)
    paste(sta, cmp, net)
  }
  spect <- vector(mode = "list", length = length(x))
  if(is.null(main))
    main <- sapply(x, GetTitle)
  else{
    if(length(main) == 1 )
      main <- rep(main, length(x))
    else{
      if((length(main) != length(x)) )
        stop("length(x) must equal length(main).\n")
    }
  }
  sac.ts <- lapply(x, sac2ts)
  spect <- lapply(sac.ts, spectrum, plot = FALSE, ...)
  if(plot)
  {
    par(mfrow = c(length(x), 1))
    for(i in 1:length(x))
    {
      spect[[i]] <- spectrum(sac.ts[[i]], plot = TRUE,
                             main = main[i], log = log, ...)
    }
  }else
  {
    for(i in 1:length(x))
    {
      spect[[i]] <- spectrum(sac.ts[[i]], plot = FALSE,
                             main = main[i], log = log, ...)
    }
  }
  class(spect) <- "rsac-spec"
  invisible(spect)
}
