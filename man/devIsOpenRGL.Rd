%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  devIsOpenRGL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{devIsOpenRGL}
\alias{devIsOpenRGL}


\title{Checks if zero or more devices are open or not}

\description{
 Checks if zero or more devices are open or not.
}

\usage{
devIsOpenRGL(which=rgl::rgl.cur(), ...)
}

\arguments{
  \item{which}{An index (\code{\link[base]{numeric}}) \code{\link[base]{vector}} or a label (\code{\link[base]{character}}) \code{\link[base]{vector}}.}
  \item{...}{Not used.}
}

\value{
  Returns a named \code{\link[base]{logical}} \code{\link[base]{vector}} with \code{\link[base:logical]{TRUE}} if a device is open,
  otherwise \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson}



\keyword{device}
\keyword{utilities}
