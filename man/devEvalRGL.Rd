%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  devEvalRGL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{devEvalRGL}
\alias{devEvalRGL}


\title{Opens a new RGL graphics device, evaluate (graphing) code, and closes device}

\description{
 Opens a new RGL graphics device, evaluate (graphing) code, and closes device.
}

\usage{
 devEvalRGL(type="webgl", expr, ..., initially=NULL, finally=NULL, envir=parent.frame(),
  name="Rplot", tags=NULL, sep=getDevOption("*", "sep"), ext="WebGL.html", filename=NULL,
  path=getDevOption("*", "path"), field=NULL, onIncomplete=c("remove", "rename", "keep"),
  force=getDevOption("*", "force"))
}

\arguments{
  \item{type}{Specifies the type of graphics device to be used.
   The device is created and opened using \code{\link{devNew}}.
   Multiple types may be specified.}
  \item{expr}{The \code{\link[base]{expression}} of graphing commands to be evaluated.}
  \item{initially, finally}{Optional \code{\link[base]{expression}}:s to be evaluated
   before and after \code{expr}. If \code{type} specifies multiple
   devices, these optional \code{\link[base]{expression}}:s are only evaluated ones.}
  \item{...}{Additional arguments passed to \code{\link{devNewRGL}}().}
  \item{envir}{The \code{\link[base]{environment}} where \code{expr} should be evaluated.}
  \item{name, tags, sep}{The fullname name of the image is specified
    as the name with optional \code{sep}-separated tags appended.}
  \item{ext}{The filename extension of the image file generated, if any.
   By default, it is inferred from argument \code{type}.}
  \item{filename}{The filename of the image saved, if any.
   By default, it is composed of arguments \code{name}, \code{tags},
   \code{sep}, and \code{ext}.  See also below.}
  \item{path}{The directory where then image should be saved, if any.}
  \item{field}{An optional \code{\link[base]{character}} string specifying a specific
    field of the named result \code{\link[base]{list}} to be returned.}
  \item{onIncomplete}{A \code{\link[base]{character}} string specifying what to do with
    an image file that was incompletely generated due to an interrupt
    or an error.}
  \item{force}{If \code{\link[base:logical]{TRUE}}, and the image file already exists, then it is
    overwritten, otherwise not.}
}

\value{
  Returns (invisibly) the index of the opened RGL device.
}



\keyword{device}
\keyword{utilities}
