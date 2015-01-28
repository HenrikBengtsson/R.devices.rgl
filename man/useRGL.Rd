%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  useRGL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{useRGL}
\alias{useRGL}


\title{Loads/attaches the 'rgl' package}

\description{
 Loads/attaches the 'rgl' package while setting up the RGL NULL devices properly.
 The default is to attach \pkg{rgl}.
}

\usage{
useRGL(useNULL=TRUE, ...)
}

\arguments{
  \item{useNULL}{If \code{\link[base:logical]{TRUE}}, only RGL "NULL" devices and no on-screen
    RGL devices are be used.}
  \item{...}{Additional arguments passed to \code{\link[R.utils]{use}}.}
}

\value{
  Returns (invisibly) the new options settings for \code{"rgl.useNULL"}.
}

\author{Henrik Bengtsson}

\seealso{
  See \code{\link[rgl]{rgl.useNULL}} and \code{\link[rgl]{rgl.init}}.
}



\keyword{device}
\keyword{utilities}
