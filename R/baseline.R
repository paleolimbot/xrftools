
#' Asymmetric least squares baseline calcluation
#'
#' Uses the \link[baseline]{baseline.als} function in the baseline package.
#'
#' @param p,lambda,maxit See \link[baseline]{baseline.als}
#' @inheritParams xrf_add_baseline_pkg
#'
#' @return spectra with original information plus basline added
#' @export
#'
#' @importFrom rlang enquo quos !! !!! .data
#'
xrf_add_baseline_als <- function(
  spectra, p = 0.003, lambda = 6, maxit = 20, clamp = 0,
  values = .data$cps, env = parent.frame()
) {
  xrf_add_baseline_pkg(
    spectra = spectra, method = "als",
    p = !!enquo(p), lambda = !!enquo(lambda), maxit = !!enquo(maxit), clamp = clamp,
    values = !!enquo(values), env = env
  )
}

#' Caclulate baselines using the baseline package
#'
#' Wraps a call to the \link[baseline]{baseline}()
#'
#' @param spectra A data frame with a nested .spectra column
#' @param values The value column in each spectra object
#' @param ... Passed to \link[baseline]{baseline}. Evaluated rowwise using objects in \code{spectra}.
#' @param clamp The smallest value that the baseline should be. Use -Inf to suppress.
#' @param env The calling environment
#'
#' @export
#'
xrf_add_baseline_pkg <- function(spectra, ..., values = .data$cps, clamp = 0, env = parent.frame()) {
  stopifnot(
    is.numeric(clamp), length(clamp) == 1
  )

  values <- enquo(values)
  dots <- quos(...)
  spectra$.spectra <- purrr::map(purrr::transpose(spectra), function(spectrum) {
    x <- spectrum$.spectra
    # vals and args evaluated within each row
    vals <- rlang::eval_tidy(values, data = x, env = env)
    args <- c(
      list(matrix(vals, nrow = 1)),
      purrr::map(dots, rlang::eval_tidy, data = spectrum, env = env)
    )

    baseline_output <- do.call(baseline::baseline, args)
    x$baseline <- baseline::getBaseline(baseline_output)[1, , drop = TRUE]
    x$baseline <- pmax(x$baseline, clamp)

    x
  })

  spectra
}
