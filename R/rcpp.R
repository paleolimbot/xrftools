
#' @useDynLib xrftools
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("xrftools", libpath)
}
