\name{querydata}
\alias{envList}
\alias{querydata}
\title{Access query data from webservice result}
\usage{
querydata(x, ...)

envList()
}
\arguments{
  \item{x}{ws result}

  \item{...}{additional parameters}
}
\description{
Access query data from webservice result
}
\examples{
querydata(flinnengdahl.ws(10,10))
# saved to env as 'querydata' too
envList()
}

