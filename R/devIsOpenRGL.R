###########################################################################/**
# @RdocFunction devIsOpenRGL
#
# @title "Checks if zero or more devices are open or not"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{which}{An index (@numeric) @vector or a label (@character) @vector.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a named @logical @vector with @TRUE if a device is open,
#   otherwise @FALSE.
# }
#
# @author
#
# @keyword device
# @keyword utilities
#*/###########################################################################
devIsOpenRGL <- function(which=rgl::rgl.cur(), ...) {
  # Nothing to do?
  if (length(which) == 0L) {
    res <- logical(0L)
    names(res) <- character(0L)
    return(res)
  }

  devList <- rgl::rgl.dev.list()
  if (is.numeric(which)) {
    isOpen <- is.element(which, devList)
  } else {
    throw("Non-supported value of 'which': ", which)
  }

  isOpen
} # devIsOpenRGL()


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
