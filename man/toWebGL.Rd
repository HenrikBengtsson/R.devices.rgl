%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  toWebGL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{toWebGL}
\alias{toWebGL}


\title{Method for creating RGL image files of a specific format}

\description{
 Method for creating RGL image files of a specific format.
}

\usage{
  toWebGL(name, ..., envir=parent.frame(), field="data")
}

\arguments{
  \item{name}{A \code{\link[base]{character}} string specifying the name of the image file.}
  \item{...}{Additional arguments passed to \code{\link{devEvalRGL}}(), e.g.
     \code{tags} and \code{aspectRatio}.}
  \item{envir}{The \code{\link[base]{environment}} where the plot expression should be
     evaluated.}
  \item{field}{A \code{\link[base]{character}} string specifying the field of the named
     result \code{\link[base]{list}} to be returned.}
}

\value{
  Returns what \code{\link{devEvalRGL}}() returns.
}

\author{Henrik Bengtsson}

\seealso{
  These functions are wrappers for \code{\link{devEvalRGL}}().
}



\keyword{documentation}
\keyword{device}
\keyword{utilities}
