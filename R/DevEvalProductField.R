as.character.DevEvalProductField <- function(x, ...) {
  x <- NextMethod("as.character")
  n <- nchar(x)
  if (n > 1e3) {
    x <- paste(c(
      substring(x, first=1L, last=900L),
      substring(x, first=n-100L, last=n)
    ), collapse=" [...] ")
  }
  x
}


## This should only be set used when using rsource()
## rpaste.DevEvalProductField <- function(x, ...) {
##   as.character(x, ...)
## } # rpaste()
