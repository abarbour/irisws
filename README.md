irisws: Access to IRIS Web Services within R
======

This project is intended to serve as a tool
to access
[IRIS WS](http://service.iris.edu/) using the R environment.
The repository is a self-consistent R-package, meaning
one can do the following
~~~~~{.R}
install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("irisws", username="abarbour")
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
will load the package library
and print a list of features currently included.  

This project has limited functionality, and is evolving from
a rather slowly, so you should re-install often.
Note that you will also need to do the following
for all features in the package to function properly:
~~~~~{.R}
pkgs <- c("lubridate", "png","RCurl","reshape2","XML2R")
install.packages(pkgs, dependencies=TRUE)
~~~~~

Once this code is of suitable
completeness (and reasonably well tested), 
I plan to upload it to [CRAN](http://cran.r-project.org/).
Feel free to contact me 
(<a href="https://github.com/abarbour" class="user-mention">@abarbour</a>)
should you have questions, or wish to contribute; or, use github as it was
intended and commit some changes of your own! :)

Examples
--------

### Timeseries

Continuous seismic data is easily accessed with the timeseries webservice.

~~~~~{.R}
require(irisws)

# Pore pressure data for the El Mayor Cucapah earthquake
w <- ws.timeseries(network="PB", station="B084", location="--", channel="LDD",
	starttime="2010.094T22:00:00", 
	duration=7200, 
	output="plot", 
	filename="myplot.png")
~~~~~

Reference-style: 
![alt text][inst/sac/elmayorB084_LDD.png]

### Basic support for .sac files

~~~~~{.R}
sacfi <- system.file("sac/elmayorB084_LDD.sac", package="irisws")
#   this is a little-endian sac file, so
#   must specify (your system may be 'big'!)
x1 <- read.sac(sacfi, is.binary=TRUE, endianness="little")
#   returns an object of class 'saclist', and there is
#   a plot method:
plot(x1)
~~~~~

### Parameters from .wadl

I have found it difficult to remember which parameters are
required, and which are optional, without going to
the webpage for the application.  The .wadl files provide
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
