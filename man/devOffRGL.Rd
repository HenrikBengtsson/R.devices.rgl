%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  devOffRGL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{devOffRGL}
\alias{devOffRGL}

\alias{devDoneRGL}

\title{Closes zero or more devices}

\description{
 Closes zero or more devices.
}

\usage{
  devOffRGL(which=rgl::rgl.cur(), silent=TRUE, maxTries=10L, ...)
  devDoneRGL(...)
}

\arguments{
  \item{which}{An index (\code{\link[base]{numeric}}) \code{\link[base]{vector}} or a label (\code{\link[base]{character}}) \code{\link[base]{vector}}.}
  \item{silent}{If \code{\link[base:logical]{TRUE}}, updates of windows titles are suppressed.}
  \item{maxTries}{Number of times the device is tried to be closed,
     before giving up and throwing an error.}
  \item{...}{Arguments (eventually) passed to \code{\link[rgl]{writeWebGL}}.}
}

\value{
  Returns the index of the currently open RGL device.
}

\details{
  Function \code{devDoneRGL()} is for \code{devOffRGL()} what
  \code{\link[R.devices]{devDone}} is for \code{\link[R.devices]{devOff}}.
}

\author{Henrik Bengtsson}



\keyword{device}
\keyword{utilities}
