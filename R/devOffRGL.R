###########################################################################/**
# @RdocFunction devOffRGL
# @alias devDoneRGL
#
# @title "Closes zero or more devices"
#
# \description{
#  @get "title".
# }
#
# \usage{
#   @usage devOffRGL
#   @usage devDoneRGL
# }
#
# \arguments{
#   \item{which}{An index (@numeric) @vector or a label (@character) @vector.}
#   \item{silent}{If @TRUE, updates of windows titles are suppressed.}
#   \item{maxTries}{Number of times the device is tried to be closed,
#      before giving up and throwing an error.}
#   \item{...}{Arguments (eventually) passed to @see "rgl::writeWebGL".}
# }
#
# \value{
#   Returns the index of the currently open RGL device.
# }
#
# \details{
#   Function \code{devDoneRGL()} is for \code{devOffRGL()} what
#   @see "R.devices::devDone" is for @see "R.devices::devOff".
# }
#
# @author
#
# @keyword device
# @keyword utilities
#*/###########################################################################
devOffRGL <- function(which=rgl::rgl.cur(), silent=TRUE, maxTries=10L, ...) {
  rgl.cur <- rgl::rgl.cur
  rgl.set <- rgl::rgl.set
  rgl.close <- rgl::rgl.close
  rgl.dev.list <- rgl::rgl.dev.list

  # Nothing to do?
  if (length(which) == 0L) return(rgl.cur())

  # Only close each device once
  which <- unique(which)
  if (length(which) > 1L) {
    lapply(which, FUN=devOffRGL)
    return(rgl.cur())
  }

  # Nothing to do?
  if (!devIsOpenRGL(which)) {
    return(invisible(rgl.cur()))
  }

  # Do nothing?
  if (is.numeric(which) && length(which) == 1L && which < 1L) {
    return(invisible(rgl.cur()))
  }

    # Record the output pathname
  key <- sprintf("rgl.device.%d.options", which)
  args <- getOption(key, NULL)
  setOption(key, NULL)

  rgl.set(which, silent=silent)

  # Export to WebGL HTML file?
  if (!is.null(args)) {
##    mstr(args)
    args <- c(args, list(...))
    pathname <- do.call(.writeWebGL, args=args)
  }


  # Robustness Make sure the device is really closed
  while(maxTries > 0L && devIsOpenRGL(which)) {
    rgl.set(which, silent=silent)
    rgl.close()
    Sys.sleep(0.1)
    maxTries <- maxTries - 1L
  }

  # Sanity check
  if (devIsOpenRGL(which)) {
    devList <- rgl.dev.list()
    type <- names(devList)[which]
    throw("Failed to close %s RGL device: ", sQuote(type), which)
  }

  rgl.cur()
} # devOffRGL()


devDoneRGL <- function(...) {
  devOffRGL(...)
} # devDoneRGL()




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
