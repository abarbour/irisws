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
