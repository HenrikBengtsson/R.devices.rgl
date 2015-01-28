###########################################################################/**
# @RdocFunction devEvalRGL
#
# @title "Opens a new RGL graphics device, evaluate (graphing) code, and closes device"
#
# \description{
#  @get "title".
# }
#
# \usage{
#  @usage devEvalRGL
# }
#
# \arguments{
#   \item{type}{Specifies the type of graphics device to be used.
#    The device is created and opened using @see "devNew".
#    Multiple types may be specified.}
#   \item{expr}{The @expression of graphing commands to be evaluated.}
#   \item{initially, finally}{Optional @expression:s to be evaluated
#    before and after \code{expr}. If \code{type} specifies multiple
#    devices, these optional @expression:s are only evaluated ones.}
#   \item{...}{Additional arguments passed to @see "devNewRGL".}
#   \item{envir}{The @environment where \code{expr} should be evaluated.}
#   \item{name, tags, sep}{The fullname name of the image is specified
#     as the name with optional \code{sep}-separated tags appended.}
#   \item{ext}{The filename extension of the image file generated, if any.
#    By default, it is inferred from argument \code{type}.}
#   \item{filename}{The filename of the image saved, if any.
#    By default, it is composed of arguments \code{name}, \code{tags},
#    \code{sep}, and \code{ext}.  See also below.}
#   \item{path}{The directory where then image should be saved, if any.}
#   \item{field}{An optional @character string specifying a specific
#     field of the named result @list to be returned.}
#   \item{onIncomplete}{A @character string specifying what to do with
#     an image file that was incompletely generated due to an interrupt
#     or an error.}
#   \item{force}{If @TRUE, and the image file already exists, then it is
#     overwritten, otherwise not.}
# }
#
# \value{
#   Returns (invisibly) the index of the opened RGL device.
# }
#
# @keyword device
# @keyword utilities
#*/###########################################################################
devEvalRGL <- function(type="webgl", expr, ..., initially=NULL, finally=NULL, envir=parent.frame(), name="Rplot", tags=NULL, sep=getDevOption("*", "sep"), ext="WebGL.html", filename=NULL, path=getDevOption("*", "path"), field=NULL, onIncomplete=c("remove", "rename", "keep"), force=getDevOption("*", "force")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'type':
  type <- match.arg(type)

  # Argument 'name', 'tags' and 'sep':
  fullname <- paste(c(name, tags), collapse=sep)
  fullname <- unlist(strsplit(fullname, split=sep, fixed=TRUE))
  fullname <- sub("^[\t\n\f\r ]*", "", fullname) # trim tags
  fullname <- sub("[\t\n\f\r ]*$", "", fullname) #
  fullname <- fullname[nchar(fullname) > 0L]     # drop empty tags
  fullname <- paste(fullname, collapse=sep)

  # Argument 'ext':
  ext <- Arguments$getCharacter(ext)

  # Argument 'filename' & 'path':
  if (is.null(filename)) {
    filename <- sprintf("%s.%s", fullname, ext)
  }
  pathname <- Arguments$getWritablePathname(filename, path=path, mustNotExist=FALSE)

  # Argument 'onIncomplete':
  onIncomplete <- match.arg(onIncomplete)

  # Argument 'force':
  force <- Arguments$getLogical(force)

  # Result object
  res <- DevEvalFileProduct(pathname, type=type)

  done <- FALSE
  if (force || !isFile(pathname)) {
    devIdx <- devNewRGL(type=type, filename=pathname, ...)

    # Make sure to always close the opened RGL device
    on.exit({
      devDoneRGL(devIdx)
      devIdx <- NULL
    }, add=TRUE)

    # Call RGL plot expression
    eval(expr, envir=envir)

    done <- TRUE
  } # if (force || !isFile(pathname))

  # Close it here to make sure the image file is created.
  # This may be needed depending on 'field'.
  if (done) {
    devDoneRGL(devIdx)
    devIdx <- NULL
  }

  # Subset?
  if (!is.null(field)) {
    res <- res[[field]]
    class(res) <- c("DevEvalProductField", class(res))
  }

  res
} # devEvalRGL()

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
