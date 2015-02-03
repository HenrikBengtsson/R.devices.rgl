###########################################################################/**
# @RdocFunction webgl
# @alias webgl
#
# @title "Opens a new WebGL HTML device for RGL-based graphics"
#
# \description{
#  @get "title".
#  The opened device, which is indeed an RGL device, must be closed
#  using @see "devDoneRGL", otherwise the WebGL HTML file is not generated.
#
#  When the device is closed, the output is written to a WebGL HTML document,
#  which can then be incorporated into a main HTML document, for instance,
#  using RSP-embed HTML templates, cf. \pkg{R.rsp}.
# }
#
# @synopsis
#
# \arguments{
#   \item{filename}{The pathname of the generated WebGL HTML file.}
#   \item{width, height}{@numeric scalars specifying the width and
#    the height (in pixels) of the WebGL HTML canvas.}
#   \item{font}{A @character @vector of the HTML font names used in
#    the font-family attribute.}
#   \item{useNULL}{A @logical specifying whether to use the RGL null
#     graphics device or not, cf. @see "rgl::open3d".}
#   \item{snapshot}{A @logical specifying whether to include a static
#     PNG snaphot to be displayed before the WebGL graphics is
#     rendered.  Snapshots are not supported if \code{useNULL=TRUE}.}
#   \item{header}{A @logical specifying whether the generated WebGL
#     HTML document should include/define the global 'CanvasMatrix.js'
#     Javascript or not.  If @FALSE, it must be manually added to the
#     main HTML page.  By using @TRUE, each figure will include/define
#     the same code, which slightly increases the output size, but
#     is safe and valid to do.}
#   \item{class}{A @character @vector specifying the CSS classes on
#     the HTML canvas object that displays the WebGL graphics.}
#   \item{...}{Additional arguments passed to @see "rgl::writeWebGL" upon
#     closing the opened device.}
# }
#
# \value{
#   Returns (invisibly) the RGL device number, cf. @see "rgl::rgl.dev.list".
#   When done plotting, the device must be closed using @see "devDoneRGL".
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
# \section{Defaults}{
#   Whenever a default value is missing, the fallback is to use the
#   corresponding value for the \code{"png"} device type according
#   to \pkg{R.devices}, i.e. \code{R.devices::devOptions("png")}.
#   Currently, this only applies to the width and the height arguments.
# }
#
# \seealso{
#   To close the device, use  @see "devDoneRGL".
#   A more convenient and robust function for generating WebGL HTML file
#   is @see "toWebGL" (recommended).
#   See also @see "devEvalRGL" and @see "devNewRGL".
# }
#
# @keyword device
# @keyword utilities
#*/###########################################################################
webgl <- function(filename="Rplot.WebGL.html", width=480L, height=480L, font=c("sans-serif", "Arial", "Helvetica"), useNULL=TRUE, snapshot=FALSE, header=TRUE, class=c("rglWebGL"), ...) {
  oopts <- useRGL(useNULL=useNULL)
  on.exit(options(oopts))

  ver <- packageVersion("rgl")
  if (ver > "0.93.1098" && ver < "0.94.1143") {
    if (useNULL) {
      warning(sprintf("Detected rgl v%s. Due to a bug in rgl (>= 0.94.1111 & < 0.94.1143), it is not possible to generate WebGL via RGL \"null\" devices. Will fall back to use a regular and temporarily visible screen device.", ver))
      useNULL <- FALSE
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'filename':
  pathname <- Arguments$getWritablePathname(filename, mustNotExist=FALSE)

  # Argument 'width':
  width <- Arguments$getNumeric(width, range=c(0,Inf))

  # Argument 'height':
  height <- Arguments$getNumeric(height, range=c(0,Inf))

  # Argument 'class':
  if (!is.null(class)) {
    class <- Arguments$getCharacters(class)
  }

  # Argument 'font':
  font <- unlist(strsplit(font, split=",", fixed=TRUE))
  font <- paste(trim(font), collapse=",")

  if (snapshot) {
    if (useNULL) {
      throw("Argument snaphot=TRUE is not supported when using RGL \"null\" devices.")
    }
    throw("Argument snaphot=TRUE is not supported.")
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Open a temporary RGL device (and close it when done)
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (a) Record device specific parameters needed when closing
  #     the device.
  args <- list(pathname=pathname, header=header, snapshot=snapshot,
               width=width, height=height, font=font, class=class, ...)
  attr(args, "timestamp") <- Sys.time()
##  mstr(args)

  # (b) Open RGL device width same dimensions as output file
  rgl::open3d(windowRect=c(0,0,width,height), useNULL=useNULL)
  devIdx <- rgl::rgl.cur()

  # Record the output pathname
  key <- sprintf("rgl.device.%d.options", devIdx)
  setOption(key, args)

  invisible(devIdx)
} # webgl()



############################################################################
# HISTORY:
# 2015-02-03
# o Added argument 'class' for injecting a 'class' attribute to the
#   HTML canvas generated when RGL device is closed with devOffRGL().
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
