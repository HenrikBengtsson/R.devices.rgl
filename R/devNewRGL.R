###########################################################################/**
# @RdocFunction devNewRGL
#
# @title "Opens a new RGL graphics device"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{type}{Specifies the type of RGL graphics device @function to
#    be used.}
#   \item{filename}{The filename of the Javascript-embedded HTML file saved.}
#   \item{width, height}{The width and height in pixels of the RGL figure.}
#   \item{scale}{A @numeric scalar factor specifying how much the
#     width and the height should be rescaled.}
#   \item{aspectRatio}{A @numeric ratio specifying the aspect ratio
#     of the image.  See below.}
#   \item{...}{Additional arguments passed to the device @function.}
# }
#
# \value{
#   Returns a @see "DevEvalFileProduct".
#   If argument \code{field} is given, then the field of the
#   @see "DevEvalProduct" is returned instead.
# }
#
# \section{Generated file}{
#   If created, the generated file is saved in the directory
#   specfied by argument \code{path} with a filename consisting of
#   the \code{name} followed by optional comma-separated \code{tags}
#   and a filename extension given by argument \code{ext}.
#
#   By default, the file is only created if the \code{expr}
#   is evaluated completely.  If it is, for instance, interrupted
#   by the user or due to an error, then any incomplete/blank
#   file that was created will be removed.  This behavior can be
#   turned of using argument \code{onIncomplete}.
# }
#
# @keyword device
# @keyword utilities
#*/###########################################################################
devNewRGL <- function(type="webgl", filename="Rplot.WebGL.html", width=NULL, height=NULL, scale=1, aspectRatio=1, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cleanLength <- function(x, ...) {
    name <- substitute(x);
    if (is.null(x) || !is.numeric(x) || !is.finite(x)) {
      warning("Ignoring non-finite '", name, "' value: ", x);
      x <- NULL;
    }
    x;
  } # cleanLength()


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'type':
  type <- match.arg(type)

  # Argument 'filename':
  pathname <- Arguments$getWritablePathname(filename, mustNotExist=FALSE)

  # Argument 'width':
  if (!is.null(width)) {
    width <- Arguments$getNumeric(width, range=c(0,Inf))
  }

  # Argument 'height':
  if (!is.null(height)) {
    height <- Arguments$getNumeric(height, range=c(0,Inf))
  }

  # Argument 'scale':
  if (!is.null(scale)) {
    scale <- Arguments$getDouble(scale, range=c(0,Inf))
  }

  # Argument 'aspectRatio':
  if (!is.null(aspectRatio)) {
    aspectRatio <- Arguments$getDouble(aspectRatio, range=c(0,Inf))
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Update the 'height' by argument 'aspectRatio'?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  typeT <- "png" # Default to same settings as the PNG devices
  if (!is.null(aspectRatio)) {
    # Were both 'width' and 'height' explicitly specified?
    if (!is.null(width) && !is.null(height)) {
      if (aspectRatio != 1) {
        warning("Argument 'aspectRatio' was ignored because both 'width' and 'height' were given: ", aspectRatio)
      }
    } else {
      # None of 'width' and 'height' was specified?
      if (is.null(width) && is.null(height)) {
        # (a) Infer 'width' from devOptions()...
        width <- devOptions(typeT)$width
        if (!is.null(width)) {
          height <- aspectRatio * width
        } else {
          warning("Argument 'aspectRatio' was ignored because none of 'width' and 'height' were given and 'width' could not be inferred from devOptions(\"", typeT, "\"): ", aspectRatio)
        }
      } else if (!is.null(width)) {
        # Argument 'width' was specified but not 'height'
        height <- aspectRatio * width
      } else if (!is.null(height)) {
        # Argument 'height' was specified but not 'width'
        width <- height / aspectRatio
      }
    }
  } # if (!is.null(aspectRatio))


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Rescale 'width' & 'height' by argument 'scale'?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  typeT <- "png" # Default to same settings as the PNG devices
  if (!is.null(scale) && scale != 1.0) {
    # Infer 'width' from the settings
    if (is.null(width)) {
      width <- devOptions(typeT)$width
    }

    # Possible to rescale?
    if (is.null(width)) {
      warning("Argument 'scale' was ignored because it was not possible to infer 'width': ", scale)
    } else {
      # Infer 'height'...
      if (!is.null(aspectRatio)) {
        # ...from aspect ratio
        height <- aspectRatio * width
      } else {
        # ...from settings
        if (is.null(height)) {
          height <- devOptions(typeT)$height
          height <- cleanLength(height)
        }
        if (is.null(height)) {
          warning("Argument 'scale' was ignored because it was not possible to infer 'height': ", scale)
        }
      }
    }

    # So finally, possible to rescale?
    if (!is.null(width) && !is.null(height)) {
      width <- scale * width
      height <- scale * height
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Find device function
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  devFcn <- get(type, mode="function")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Open RGL device
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  devIdx <- devFcn(filename=pathname, width=width, height=height, ...)

  invisible(devIdx)
} # devNewRGL()

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
