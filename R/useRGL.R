###########################################################################/**
# @RdocFunction useRGL
#
# @title "Loads/attaches the 'rgl' package"
#
# \description{
#  @get "title" while setting up the RGL NULL devices properly.
#  The default is to attach \pkg{rgl}.
# }
#
# @synopsis
#
# \arguments{
#   \item{useNULL}{If @TRUE, only RGL "NULL" devices and no on-screen
#     RGL devices are be used.}
#   \item{...}{Additional arguments passed to @see "R.utils::use".}
# }
#
# \value{
#   Returns (invisibly) the new options settings for \code{"rgl.useNULL"}.
# }
#
# @author
#
# \seealso{
#   See @see "rgl::rgl.useNULL" and @see "rgl::rgl.init".
# }
#
# @keyword device
# @keyword utilities
#*/###########################################################################
useRGL <- function(useNULL=TRUE, ...) {
  ## The 'rgl' package initiates the RGL device differently depending on
  ## rgl.useNULL() already when the package is loaded.  Moreover, at least
  ## on Linux, X11 is required whenever useNULL=FALSE, whereas it is not
  ## when useNULL=TRUE.  By telling 'rgl' already here to useNULL=TRUE,
  ## we can use it without X11, e.g. in head-less batch jobs.

  ## Is the 'rgl' package already loaded?
  if (is.element("rgl", loadedNamespaces())) {
    oldUseNULL <- rgl::rgl.useNULL()
    if (useNULL && !oldUseNULL) {
      throw("Failed to setup 'rgl' with 'useNULL=TRUE', because the package was already loaded/initiated with 'useNULL=FALSE', which cannot be undone without restarting R.")
    }
    oopts <- options(rgl.useNULL=useNULL)
  } else {
    oopts <- options(rgl.useNULL=useNULL)
    use("rgl", ...)
  }

  ## Compatibility check
  assertCompatibility <- getOption("R.devices.rgl::assertCompatibility", TRUE)
  ver <- as.character(packageVersion("rgl"))
  if (assertCompatibility && compareVersion(ver, "0.95.1201")) {
    throw("For compatibility reasons, the current version of R.devices.rgl requires rgl <= 0.95.1201: ", ver)
  }

  invisible(oopts)
} # useRGL()


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
