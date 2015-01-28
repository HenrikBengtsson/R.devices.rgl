# Writes current RGL device to a WebGL HTML file (atomically)
.writeWebGL <- function(pathname, header=TRUE, ...) {
  # Argument 'pathname':
  pathname <- Arguments$getWritablePathname(pathname)

  # Argument 'header':
  header <- Arguments$getLogical(header)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Prefix
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  fullname <- tools::file_path_sans_ext(basename(pathname))
  fullname <- gsub("[.](WebGL)$", "", fullname, ignore.case=TRUE)

  # HTML/Javascript name prefix to use
  prefix <- sprintf("%s_", gsub("[,]", "_", fullname))

  # Encode prefix character (FIXME)
  prefix <- gsub("[^[:alnum:]]", "_", prefix)
  prefix <- sub("^([^[:alpha:]])", "_\\1", prefix)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Write RGL plot to WebGL HTML code
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (a) Set temporary working directory
  pathT <- tempdir()
  prefixT <- sprintf("rgl_WebGL_%s", fullname)
  pathT <- file.path(pathT, prefixT)
  pathT <- Arguments$getWritablePath(pathT, mustNotExist=FALSE)
  opwd <- setwd(pathT)
  on.exit({
    setwd(opwd)
  }, add=TRUE)

  # (c) Export full WebGL HTML code
  printf(file="WebGL.tmpl",
         "%%%sWebGL%%\n<script>%swebGLStart();</script>\n", prefix, prefix)
  rgl::writeWebGL(dir=".", filename="WebGL.html", template="WebGL.tmpl",
             prefix=prefix, ...)



  # (d) Import WebGL HTML code
  bfr <- readLines("WebGL.html", warn=FALSE)

  # (e) Trim exported full WebGL HTML code
  # Trim white space (to make file smaller and to avoid any Markdown
  # parser to interpret it as code chunks)
  bfr <- trim(bfr)
  # Drop empty lines (to make file smaller)
  bfr <- bfr[nchar(bfr) > 0L]
  # Insert or drop 'CanvasMatrix.js' script?
  idx <- grep("^[[:space:]]*<script src=.CanvasMatrix[.]js..*>.*</script>$", bfr)
  # Sanity check
  stopifnot(length(idx) == 1L)
  if (header) {
    hdr <- readLines("CanvasMatrix.js", warn=FALSE)
    hdr <- c('<script type="text/javascript">', hdr, '</script>')
    hdr <- paste(hdr, collapse="\n")
    bfr[idx] <- hdr
  } else {
    bfr <- bfr[-idx]
  }

  # (f) Cleanup
  # Remove auxillary files
  file.remove(c("CanvasMatrix.js", "WebGL.tmpl", "WebGL.html"))
  # Reset working directory
  setwd(opwd)
  # Remove temporary directory
  unlink(pathT, recursive=TRUE)

  # (g) Write trimmed HTML
  bfr <- paste(bfr, collapse="\n")
  writeChar(bfr, con=pathname)

  pathname
} # .writeWebGL()


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
