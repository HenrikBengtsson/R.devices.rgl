%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  webgl.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{webgl}
\alias{webgl}

\alias{webgl}

\title{Opens a new WebGL HTML device for RGL-based graphics}

\description{
 Opens a new WebGL HTML device for RGL-based graphics.
 The opened device, which is indeed an RGL device, must be closed
 using \code{\link{devDoneRGL}}(), otherwise the WebGL HTML file is not generated.

 When the device is closed, the output is written to a WebGL HTML document,
 which can then be incorporated into a main HTML document, for instance,
 using RSP-embed HTML templates, cf. \pkg{R.rsp}.
}

\usage{
webgl(filename="Rplot.WebGL.html", width=480L, height=480L, font=c("sans-serif", "Arial",
  "Helvetica"), useNULL=TRUE, snapshot=FALSE, header=TRUE, ...)
}

\arguments{
  \item{filename}{The pathname of the generated WebGL HTML file.}
  \item{width, height}{\code{\link[base]{numeric}} scalars specifying the width and
   the height (in pixels) of the WebGL HTML canvas.}
  \item{font}{A \code{\link[base]{character}} \code{\link[base]{vector}} of the HTML font names used in
   the font-family attribute.}
  \item{useNULL}{A \code{\link[base]{logical}} specifying whether to use the RGL null
    graphics device or not, cf. \code{\link[rgl]{open3d}}.}
  \item{snapshot}{A \code{\link[base]{logical}} specifying whether to include a static
    PNG snaphot to be displayed before the WebGL graphics is
    rendered.  Snapshots are not supported if \code{useNULL=TRUE}.}
  \item{header}{A \code{\link[base]{logical}} specifying whether the generated WebGL
    HTML document should include/define the global 'CanvasMatrix.js'
    Javascript or not.  If \code{\link[base:logical]{FALSE}}, it must be manually added to the
    main HTML page.  By using \code{\link[base:logical]{TRUE}}, each figure will include/define
    the same code, which slightly increases the output size, but
    is safe and valid to do.}
  \item{...}{Additional arguments passed to \code{\link[rgl]{writeWebGL}} upon
    closing the opened device.}
}

\value{
  Returns (invisibly) the RGL device number, cf. \code{\link[rgl]{rgl.dev.list}}.
  When done plotting, the device must be closed using \code{\link{devDoneRGL}}().
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

\section{Defaults}{
  Whenever a default value is missing, the fallback is to use the
  corresponding value for the \code{"png"} device type according
  to \pkg{R.devices}, i.e. \code{R.devices::devOptions("png")}.
  Currently, this only applies to the width and the height arguments.
}

\seealso{
  To close the device, use  \code{\link{devDoneRGL}}().
  A more convenient and robust function for generating WebGL HTML file
  is \code{\link{toWebGL}}() (recommended).
  See also \code{\link{devEvalRGL}}() and \code{\link{devNewRGL}}().
}



\keyword{device}
\keyword{utilities}
