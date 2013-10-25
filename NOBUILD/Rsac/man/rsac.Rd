\name{rsac}
\alias{rsac}
\title{READ A BINARY SAC FILE}
\description{
  This function reads the header information and data of a SAC binary
  file into a list.
}

\usage{
  rsac(files, endian = .Platform$endian)
}
\arguments{
  \item{files}{name of file to be read.}
  \item{endian}{the endian-ness of the file. See
    \code{\link[base]{readBin}}.}
}

\value{an object of class \code{"rsac"}, which is a list containing the
  following elements: 
  \item{amp}{data.}
  \item{dt}{Increment between evenly spaced samples (nominal
    value).}
  \item{depmin}{Minimum value of dependent variable.}
  \item{depmax}{Maximum value of dependent variable.}
  \item{scale}{Multiplying scale factor for dependent variable.}
  \item{odelta}{Observed increment if different from nominal
    value.} 
  \item{b}{Beginning value of the independent variable.}
  \item{e}{Ending value of the independent variable.}
  \item{o}{Event origin time (seconds relative to reference
    time.} 
  \item{a}{First arrival time, seconds relative to reference
    time.} 
  \item{f}{End of event time.}
  \item{stla}{Station latitude (degrees, north positive).}
  \item{stlo}{Station longitude (degrees, east positive).}
  \item{stel}{Station elevation (meters).}
  \item{stdp}{Station depth below surface (meters). }
  \item{evla}{Event latitude (degrees, north positive).}
  \item{evlo}{Event longitude (degrees, east positive).}
  \item{evel}{Event elevation (meters).}
  \item{evdp}{Event depth below surface (meters).}
  \item{mag}{Event magnitude.}
  \item{dist}{Station to event distance (km).}
  \item{az}{Event to station azimuth (degrees).}
  \item{baz}{Station to event azimuth (degrees).}
  \item{gcarc}{Station to event great circle arc length
    (degrees).} 
  \item{cmpaz}{Component azimuth (degrees clockwise from
    north).} 
  \item{cmpinc}{Component incident angle (degrees from
    vertical).} 
  \item{nzyear}{GMT year corresponding to reference (zero) time
    in file.} 
  \item{nzjday}{GMT julian day.}
  \item{nzhour}{GMT hour.}
  \item{nzmin}{GMT minute.}
  \item{nzsec}{GMT second.}
  \item{nzmsec}{GMT millisecond.}
  \item{norid}{Origin ID.}
  \item{nevid}{Event ID.}
  \item{N}{Number of points in time series.}
  \item{units}{Type of dependent variable (IUNKN=Unknown;
    IDISP=Displacement in nm; IVEL=Velocity in nm/sec; IVOLTS=Velocity
    in volts; IACC=Acceleration in nm/sec/sec).}
  \item{iztype}{Reference time equivalence (IUNKN=Unknown;
    IB=Begin time; IDAY=Midnight of refernece GMT day; IO=Event origin
    time; IA=First arrival time; ITn=User defined time pick n, n=0,9).}
  \item{leven}{\code{TRUE} if data is evenly spaced.}
  \item{lpspol}{\code{TRUE} if station components have a positive
    polarity (left-hand rule).} 
  \item{sta}{Station name.}
  \item{kevnm}{Event name.}
  \item{khole}{Hole identification if nuclear event.}
  \item{ko}{Event origin time identification.}
  \item{ka}{First arrival time identification.}
  \item{comp}{Component name.}
  \item{knetwork}{Name of seismic network.}
  \item{kinst}{Generic name of recording instrument.}  
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\references{
  \url{http://www.llnl.gov/sac/}
}
\examples{

x <- rsac(system.file("bigendian/2003.356.19.15.31.0000.NC.NTAB..EHZ.D.SAC", package = "Rsac"), endian = "big")

plot(x)

}

