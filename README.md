#_*irisws*_

Access to the
Incorporated Research Institutions in Seismology
(IRIS) Web Services (WS) from within R

------

This project is intended to serve as a tool
to access
[Incorporated Research Institutions in Seismology (IRIS) Web Services (WS)](http://service.iris.edu/) 
using the R programming language.
The repository is a self-consistent R-package, meaning
one can do the following:

~~~~~{.R}
install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("abarbour/irisws", dependencies=TRUE)
~~~~~

and from then on

~~~~~{.R}
library(irisws)
# inspect documentation
?irisws
# list current webservice functions:
webservices()  
# and so on...
~~~~~

The latter command prints a list of 
the webservice access-functions
currently included. 

This project has limited functionality at the moment, and is evolving -- you should update often.
But, once this code is of suitable
completeness (and reasonably well tested), 
I plan to upload it to [CRAN](http://cran.r-project.org/).
Feel free to contact me 
(<a href="https://github.com/abarbour" class="user-mention">@abarbour</a>)
should you have questions, or wish to contribute; or, use github as it was
intended and submit some pull requests! :)

_Note that you will also need to do the following
for all features in the package to function properly:_

~~~~~{.R}
pkgs <- c("httr","lubridate","png","RCurl","reshape2","XML","XML2R")
install.packages(pkgs, dependencies=TRUE)
~~~~~

_but these should've been installed at
the `install_github` stage._

------

Examples
------

### Raw-data (timeseries)

Among other types of data, seismic data is easily accessed with the 
[timeseries](http://service.iris.edu/irisws/timeseries/1/) 
webservice. For example, the following command will 
download an image (generated internally) of two hours of 1-Hz pore pressure data
at PBO station B084, containing signals from the 
[2010 M7.2 El Mayor Cucapah earthquake](http://en.wikipedia.org/wiki/2010_Baja_California_earthquake):

~~~~~{.R}
require(irisws)

# download the data plotted in a png file
# (the figure itself is generated within the IRIS-WS internal framework)

w <- ws.timeseries(network="PB",    # network code
	station="B084",                 # station code
	location="--",                  # location code
	channel="LDD",                  # channel code
	starttime="2010.094T22:00:00",  # the beginning of the data
	duration=7200,                  # how many second from 'starttime' to download
	output="plot",                  # output format
	filename="myplot.png")          # the filename of the output
~~~~~

which, upon success, is loaded into `w`.
Loading is an optional feature, but `TRUE` by default. 
The figure returned by the original query should resemble something like this:

![alt text](inst/sac/elmayorB084_LDD.png "Pore pressure at B084: 2010 El Mayor Cucapah M7.2")

The object `w` in this example also includes additional
information besides the data returned from IRIS-WS:

~~~~~{.R}
str(w, nchar.max = 40)
#List of 5
# $ file     : chr "myplot.png"
# $ query    : chr "http://service.iris.edu/irisws/timeseri"| __truncated__
# $ success  : logi TRUE
# $ opts     : list()
# $ querydata: 'nativeRaster' int [1:700, 1:1000] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
#  ..- attr(*, "channels")= int 4
~~~~~

The data loaded into `w` -- in this case an object with class `"nativeRaster"` -- can be accessed
with the `querydata` method:

~~~~~{.R}
qd <- querydata(w)
str(qd)
# 'nativeRaster' int [1:700, 1:1000] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
# - attr(*, "channels")= int 4
~~~~~

One could then, for example, plot the image from within `R` (> 2.11):

~~~~~{.R}
if (exists("rasterImage")) {
   plot(1:2, type='n')
   rasterImage(querydata(w), 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
}
~~~~~

We just demonstrated how to set the output format to a internally-generated plot
( with `output="plot"` ), but there are in fact a number of (more useful)
formats which can be obtained -- see the documentation ( `?ws.timeseries` ) for ways
to specify other formats.

### Basic support for Seismic Analysis Code (SAC) files

In regards to obtaining data in a different output format, the package 
includes a limited support framework for working with
[Seismic Analysis Code (SAC)](http://www.iris.edu/files/sac-manual/manual/file_format.html) files 
directly -- these are commonly
used in seismological applications, and are usually
named with the suffix `.sac`.

To illustrate some of the functionality, we have
included a `.sac` (binary) file to play with,
which can be read-in and plotted, for example:

~~~~~{.R}
require(irisws)

sacfi <- system.file("sac/elmayorB084_LDD.sac", package="irisws")

# this is a little-endian binary file, so be sure to specify it so the result
# makes sense (your system might be "big")
x <- read.sac(sacfi, is.binary=TRUE, endianness="little")

# the function 'read.sac' returns an object of class 'saclist', for which
# there is a plot method:
plot(x)
~~~~~

The `plot.saclist` method yields a figure similar to the one shown above.

### Query parameters from .wadl files

I have difficulty keeping track of the the various parameters
required for a given webservice (not to mention the myriad optional
arguments).  
Because of this, we include a mechanism 
to quickly inspect for parameters via the associated 
[`.wadl`](http://en.wikipedia.org/wiki/Web_Application_Description_Language) 
description;
this eliminates the need to check the service's webpage,
unless details of the different options are needed.

~~~~~{.R}
require(irisws)

#  Access the .wadl file for the timeseries application
wd <- waddler("timeseries")

#  find the parameters acceptable in a query...
print(p <- parameters(wd))

#  and ones which are required
print(subset(p, required))
~~~~~

_Another way to solve this would be to hard-code the query-parameter names 
as function arguments, but this will be left for development in the distant future._
