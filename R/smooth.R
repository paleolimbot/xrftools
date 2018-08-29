
#' Add smooth using iterative gaussian smoothing
#'
#' @param .values Which values to smooth
#' @param window,alpha,tails See \link[smoother]{smth.gaussian}. Increasing the window size
#'  (e.g., 5L or 10L) will increase the amount of smoothing.
#' @inheritParams xrf_add_smooth
#'
#' @export
#'
xrf_add_smooth_gaussian <- function(.spectra, .values = .data$.spectra$cps,
                                    window = 3L, alpha = 2.5, tails = TRUE,
                                    .iter = 10, .epsilon = NA, .env = parent.frame()) {
  .values <- enquo(.values)
  xrf_add_smooth(
    .spectra,
    smoother::smth.gaussian,
    !!.values,
    window = !!enquo(window),
    alpha = !!enquo(alpha),
    tails = !!enquo(tails),
    .iter = .iter,
    .epsilon = .epsilon,
    .env = .env
  )
}

#' Smooth spectra using a function
#'
#' @param .iter Maximum number of iterations to smooth (minus 1)
#' @param .epsilon Further constrain maximum iterations
#' @inheritParams xrf_add_baseline
#'
#' @return A modified .spectra
#' @export
#'
xrf_add_smooth <- function(.spectra, .fun, ..., .iter = 0, .epsilon = NA, .env = parent.frame()) {

  dots <- quos(...)
  .spectra$.spectra <- purrr::map(purrr::transpose(.spectra), function(spectrum) {
    x <- spectrum$.spectra

    # args evaluated within each row
    args <- purrr::map(dots, rlang::eval_tidy, data = spectrum, env = .env)

    x$smooth <- do.call(.fun, args)
    for(i in seq_len(.iter)) {
      args[[1]] <- x$smooth
      new_smooth <- do.call(.fun, args)
      if(!identical(.epsilon, NA) && all(abs(x$smooth - new_smooth) < .epsilon)) {
        x$smooth <- new_smooth
        break
      }

      x$smooth <- new_smooth
    }

    x
  })

  .spectra
}

