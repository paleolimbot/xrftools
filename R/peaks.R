
xrf_add_peaks <- function(.spectra, .values = .data$.spectra$smooth - .data$.spectra$baseline, ...) {
  # have to load the Peaks DLLs for some reason
  .First.lib <- NULL; rm(.First.lib)
  withr::with_namespace("Peaks", .First.lib(dirname(find.package("Peaks")), "Peaks"))

  .values <- enquo(.values)

}

peak_search_pkg <- function(x, y, ...) {
  search <- Peaks::SpectrumSearch(y, ...)
}

xrf_add_peaks_fun <- function(.spectra, .fun, ..., .env = parent.frame()) {
  dots <- quos(...)
  .spectra$.peaks <- purrr::map(purrr::transpose(.spectra), function(spectrum) {
    # args evaluated within each row
    args <- purrr::map(dots, rlang::eval_tidy, data = spectrum, env = .env)
    do.call(.fun, args)
  })

  .spectra
}
