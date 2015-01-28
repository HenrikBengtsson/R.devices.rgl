###########################################################################/**
# @RdocDocumentation toWebGL
#
# @title "Method for creating RGL image files of a specific format"
#
# \description{
#  @get "title".
# }
#
# \usage{
#   toWebGL(name, ..., envir=parent.frame(), field="data")
# }
#
# \arguments{
#   \item{name}{A @character string specifying the name of the image file.}
#   \item{...}{Additional arguments passed to @see "devEvalRGL", e.g.
#      \code{tags} and \code{aspectRatio}.}
#   \item{envir}{The @environment where the plot expression should be
#      evaluated.}
#   \item{field}{A @character string specifying the field of the named
#      result @list to be returned.}
# }
#
# \value{
#   Returns what @see "devEvalRGL" returns.
# }
#
# @author
#
# \seealso{
#   These functions are wrappers for @see "devEvalRGL".
# }
#
# @keyword device
# @keyword utilities
#*/###########################################################################
toWebGL <- function(name, ..., envir=parent.frame(), field="data") {
  devEvalRGL(type="webgl", name=name, ..., envir=envir, field=field)
} # toWebGL()


############################################################################
# HISTORY:
# 2015-01-28
# o DOCUMENTATION: Added Rdoc help.
# o Added useRGL().
# o ROBUSTNESS: Now webgl(..., useNULL=FALSE) will set option
#   rgl.useNULL=FALSE before loading the 'rgl' package, which makes
#   the package work also on Linux when X11 is not available.
# 2014-10-17
# o Now using R.devices (>= 2.12.0) device-option style.
# 2014-09-24
# o Added class 'DevEvalProductField' and as.character() for it.
# 2014-09-10
# o Added devOffRGL(), which now tries hard to make sure devices are closed.
# o Extract webgl() out of devNewRGL().  Harmozing with R.devices.
# 2014-09-09
# o Added devNewRGL().
# o WORKAROUND: devEvalRGL() detects if the rgl version has a bug in
#   exporting from RGL "null" devices or not.  If so, it avoids using
#   such devices.
# o CLEANUP: Simplified .writeWebGL(), moved functionality to devEvalRGL()
#   and now toWebGL() is a simple wrapper around devEvalRGL().  This makes
#   the code work more like what's in R.devices.
#   Next is to move more code from devEvalRGL() to devNewGRL().
# 2014-09-05
# o Now argument 'font' supports a vector of font names.
# 2014-09-03
# o For now, WebGL 'width' and 'height' defaults to devOptions('png').
# o Added argument 'aspectRatio'.
# 2014-09-02
# o Added toWebGL() immitating toNnn() functions of R.devices.
# o Created.
############################################################################
