%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  devNewRGL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{devNewRGL}
\alias{devNewRGL}


\title{Opens a new RGL graphics device}

\description{
 Opens a new RGL graphics device.
}

\usage{
devNewRGL(type="webgl", filename="Rplot.WebGL.html", width=NULL, height=NULL, scale=1,
  aspectRatio=1, ...)
}

\arguments{
  \item{type}{Specifies the type of RGL graphics device \code{\link[base]{function}} to
   be used.}
  \item{filename}{The filename of the Javascript-embedded HTML file saved.}
  \item{width, height}{The width and height in pixels of the RGL figure.}
  \item{scale}{A \code{\link[base]{numeric}} scalar factor specifying how much the
    width and the height should be rescaled.}
  \item{aspectRatio}{A \code{\link[base]{numeric}} ratio specifying the aspect ratio
    of the image.  See below.}
  \item{...}{Additional arguments passed to the device \code{\link[base]{function}}.}
}

\value{
  Returns a \code{\link{DevEvalFileProduct}}.
  If argument \code{field} is given, then the field of the
  \code{\link{DevEvalProduct}} is returned instead.
}

\section{Generated file}{
  If created, the generated file is saved in the directory
  specfied by argument \code{path} with a filename consisting of
  the \code{name} followed by optional comma-separated \code{tags}
  and a filename extension given by argument \code{ext}.

  By default, the file is only created if the \code{expr}
  is evaluated completely.  If it is, for instance, interrupted
  by the user or due to an error, then any incomplete/blank
  file that was created will be removed.  This behavior can be
  turned of using argument \code{onIncomplete}.
}



\keyword{device}
\keyword{utilities}
