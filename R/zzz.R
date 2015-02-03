## covr: skip=all

.onLoad <- function(libname, pkgname) {
  ns <- getNamespace(pkgname)
  pkg <- Package(pkgname)
  assign(pkgname, pkg, envir=ns)
}

.onAttach <- function(libname, pkgname) {
  startupMessage(get(pkgname, envir=getNamespace(pkgname)))
}


############################################################################
# HISTORY:
# 2015-02-03
# o Created.
############################################################################
