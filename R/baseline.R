
#' Calculate baselines using SNIP
#'
#' Based on \link[Peaks]{SpectrumBackground} (Peaks package). The most useful parameter to change is the
#' \code{iterations} argument (higher value will result in a more conservative baseline estimation).
#'
#' ... Passed to \link[Peaks]{SpectrumBackground}
#' @inheritParams xrf_add_baseline_pkg
#'
#' @export
#'
xrf_add_baseline_snip <- function(.spectra, .values = .data$.spectra$cps, ..., .clamp = -Inf, .env = parent.frame()) {
  # have to load the Peaks DLLs for some reason
  .First.lib <- NULL; rm(.First.lib)
  withr::with_namespace("Peaks", .First.lib(dirname(find.package("Peaks")), "Peaks"))

  .values <- enquo(.values)
  xrf_add_baseline(
    .spectra,
    Peaks::SpectrumBackground,
    !!.values,
    ...,
    .clamp = .clamp,
    .env = .env
  )
}

#' Caclulate baselines using the baseline package
#'
#' Wraps a call to the \link[baseline]{baseline}(). See documentation in that package for
#' references for baseline correction.
#'
#' @param .values The value column in each spectra object
#' @param ... Passed to \link[baseline]{baseline}. Evaluated rowwise using objects in \code{spectra}.
#' @inheritParams xrf_add_baseline
#'
#' @references
#' See \link[baseline]{baseline}
#'
#' @export
#'
xrf_add_baseline_pkg <- function(.spectra, .values = .data$.spectra$cps, ..., .clamp = -Inf, .env = parent.frame()) {
  .values <- enquo(.values)
  xrf_add_baseline(
    .spectra,
    function(x, ...) {
      obj <- baseline::baseline(matrix(x, nrow = 1), ...)
      baseline::getBaseline(obj)[1, , drop = TRUE]
    },
    !!.values,
    ...,
    .clamp = .clamp,
    .env = .env
  )
}

#' Add a baseline estimate from a function
#'
#' @param .spectra A spectra_df
#' @param .fun A function that receives a vector of values and outputs a vector of values
#'   of the same length
#' @param ... Passed to \code{.fun}, evaluated within .spectra
#' @param .clamp The minimum allowable value for background
#' @param .env The calling environment
#'
#' @return .spectra with a modified .spectra column
#' @export
#'
#' @importFrom rlang enquo quos !! !!!
#'
xrf_add_baseline <- function(.spectra, .fun, ..., .clamp = -Inf, .env = parent.frame()) {
  stopifnot(
    is.numeric(.clamp), length(.clamp) == 1
  )

  dots <- quos(...)
  .spectra$.spectra <- purrr::map(purrr::transpose(.spectra), function(spectrum) {
    x <- spectrum$.spectra

    # args evaluated within each row
    args <- purrr::map(dots, rlang::eval_tidy, data = spectrum, env = .env)

    x$baseline <- do.call(.fun, args)
    x$baseline <- pmax(x$baseline, .clamp)

    x
  })

  .spectra
}
