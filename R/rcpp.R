
#' @useDynLib xrf
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("xrf", libpath)
}
