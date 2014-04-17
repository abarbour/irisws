irisws: Access to IRIS Web Services within R
======

This project is intended to be a storage repository for code and other bits of
data to access
[IRIS WS](http://service.iris.edu/).  
It's also a self-consistent R-package, meaning
one can do the following
~~~~~{.R}
install.packages("devtools",dependencies=TRUE)
library(devtools)
install_github("irisws", username = "abarbour")
~~~~~
and from then on
~~~~~{.R}
library(irisws)
?irisws
# this :
webservices()  
~~~~~
will load the package library
and print a list of features currently included.  
Note that you will also need to do the following
for the package to function properly:
~~~~~{.R}
pkgs <- c("lubridate", "png","RCurl","reshape2","XML2R")
install.packages(pkgs, dependencies=TRUE)
~~~~~

This project is functional, but evolving from
a rather primitive state rather slowly, so
you should re-install often.
Once it's of suitable
completeness, I plan to upload it to [CRAN](http://cran.r-project.org/).

Feel free to contact me 
(<a href="https://github.com/abarbour" class="user-mention">@abarbour</a>)
should you have questions, or wish to contribute; or, use github as it was
intended and commit some changes of your own! :)
