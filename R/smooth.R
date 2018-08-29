
#' Add smooth using iterative filtering
#'
#' @param filter The filter which should be applied
#' @param .iter Maximum number of iterations to smooth
#' @param .epsilon Further constrain maximum iterations
#' @param .values Which values to smooth
#' @inheritParams xrf_add_smooth
#'
#' @export
#'
xrf_add_smooth_filter <- function(.spectra, .values = .data$.spectra$cps,
                                  filter = xrf_filter_gaussian(),
                                  .iter = 5, .epsilon = -1, .env = parent.frame()) {
  .values <- enquo(.values)
  xrf_add_smooth(
    .spectra,
    smooth_wrap,
    !!.values,
    !!enquo(filter),
    iterations = !!enquo(.iter),
    epsilon = !!enquo(.epsilon),
    .env = .env
  )
}

#' Generate smoothing filters
#'
#' @param width The width of the smoothing window (an odd number)
#' @param alpha Higher alpha makes the smoothing window more focused on the center.
#'
#' @return A numeric vector of length \code{width}
#' @export
#'
#' @examples
#' plot(xrf_filter_gaussian())
#' points(xrf_filter_pyramid(), col = "red")
#'
#'
xrf_filter_pyramid <- function(width = 7) {
  stopifnot(length(width) == 1, width >= 1, (width %% 2) != 0)
  if(width == 1) return(1)
  mid <- width %/% 2
  f <- c(
    seq(1, mid + 1),
    seq(mid, 1)
  )
  f / sum(f)
}

#' @rdname xrf_filter_pyramid
#' @export
xrf_filter_gaussian <- function(width = 7, alpha = 2.5) {
  stopifnot(
    is.numeric(width), length(width) == 1, width >= 3, (width %% 2) != 0,
    is.numeric(alpha), length(alpha) == 1, alpha > 0
  )
  hw <- width %/% 2 # halfwidth
  e <- exp(1) # eulers number
  a <- alpha
  ret <- exp(
    -0.5 * (a * (seq(0, width - 1) - hw) / hw) ^ 2
  )
  ret / sum(ret)
}

# wraps iterative smoother with better type checking
smooth_wrap <- function(x, f, iterations = 1, epsilon = -1) {
  stopifnot(
    is.numeric(x), length(x) > length(f),
    is.numeric(f), (length(f) %% 2) != 0,
    length(iterations) == 1, iterations >= 1,
    is.numeric(epsilon), length(epsilon) == 1
  )
  discreteFilterIterative(x, f, iterations = iterations, epsilon = epsilon)
}

#' Smooth spectra using a function
#'
#' @inheritParams xrf_add_baseline
#'
#' @return A modified .spectra
#' @export
#'
xrf_add_smooth <- function(.spectra, .fun, ..., .env = parent.frame()) {

  dots <- quos(...)
  .spectra$.spectra <- purrr::map(purrr::transpose(.spectra), function(spectrum) {
    x <- spectrum$.spectra

    # args evaluated within each row
    args <- purrr::map(dots, rlang::eval_tidy, data = spectrum, env = .env)
    x$smooth <- do.call(.fun, args)
    x
  })

  .spectra
}

