__*irisws*__: Access to the
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

This project has limited functionality, and is evolving from
a rather slowly, so you should re-install often.
Note that you will also need to do the following
for all features in the package to function properly:

~~~~~{.R}
pkgs <- c("lubridate","png","RCurl","reshape2","XML","XML2R")
install.packages(pkgs, dependencies=TRUE)
~~~~~

but these should've been installed at
the `install_github` stage.

Once this code is of suitable
completeness (and reasonably well tested), 
I plan to upload it to [CRAN](http://cran.r-project.org/).
Feel free to contact me 
(<a href="https://github.com/abarbour" class="user-mention">@abarbour</a>)
should you have questions, or wish to contribute; or, use github as it was
intended and commit some changes of your own! :)

------

Examples
------

### Timeseries

Continuous seismic data is easily accessed with the timeseries webservice.
For example, to download an image of two hours of 1-Hz pore pressure data
at PBO station B084, containing signals
from the 
[2010 M7.2 El Mayor Cucapah earthquake](http://en.wikipedia.org/wiki/2010_Baja_California_earthquake):

~~~~~{.R}
require(irisws)

w <- ws.timeseries(network="PB",
	station="B084", 
	location="--", 
	channel="LDD",
	starttime="2010.094T22:00:00", 
	duration=7200, 
	output="plot", 
	filename="myplot.png")
~~~~~

<!---
# upon success, the data is loaded (an optional feature, but TRUE by default)
# (can plot "nativeRaster" objects only in R > 2.11)
if (exists("rasterImage")) {
   plot(1:2, type='n')
   rasterImage(querydata(w), 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
}

The result of the original query:
-->

![alt text](inst/sac/elmayorB084_LDD.png "Pore pressure at B084: 2010 El Mayor Cucapah M7.2")

A number of options exist for the output format -- see the documentation ( `?ws.timeseries` ).

### Basic support for .sac files

A limited amount of support is provided for dealing with
.sac files.

~~~~~{.R}
require(irisws)

# the package includes a sac file to play with
sacfi <- system.file("sac/elmayorB084_LDD.sac", package="irisws")

# this is a little-endian sac file, so
# must specify (your system may be 'big'!)
x <- read.sac(sacfi, is.binary=TRUE, endianness="little")

# read.sac returns an object of class 'saclist', for which
# there is a plot method:
plot(x)
~~~~~

### Query parameters from .wadl files

I have difficulty remembering which parameters are
required for any given service (not to mention optional
arguments), without going to the webpage for the application. 
The .wadl files provide
a mechanism to quickly find out those parameters.

~~~~~{.R}
require(irisws)

#  Access the .wadl file for the timeseries application
wd <- waddler("timeseries")

#  find the parameters acceptable in a query...
print(p <- parameters(wd))

#  and ones which are required
print(subset(p, required))
~~~~~
