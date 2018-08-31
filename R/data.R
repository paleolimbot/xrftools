
#' Elemental X-ray energies and intensities
#'
#' Transition energies that may be found in X-Ray spectra and that may be useful in the analysis
#' of XRF data. This dataset (from NIST 2018) includes the KL2 (Kalpha2), KL3 (Kalpha1), KM2 (Kbeta3), KM3 (Kbeta1),
#' KM4 (Kbeta5 II), KM5 (Kbeta5 I), KN2 (Kbeta2 II), KN3 (Kbeta2 I), KN4 (Kbeta4 II), KN5 (Kbeta4 I),
#' L2M4 (Lbeta1), L3M4 (Lalpha2), L3M5 (Lalpha1), and L3N5 (Lbeta2) transition energies
#' for all elements. References in the \code{ref} column can be found at the
#' \href{NIST X-Ray Transitions Database References}{https://physics.nist.gov/PhysRefData/XrayTrans/Html/refs.html}
#' page. Relative intensities are not available for most transitions, but where they are they are provided
#' according to Salem et al. (1974). Similar information is summarised in Kaye and Laby (1995).
#'
#' @references
#' National Institute of Standards and Technology (NIST): X-Ray Transition Energies Database.
#' Retrieved August 2018. \url{https://physics.nist.gov/PhysRefData/XrayTrans/Html/search.html}.
#'
#' Kaye, G. W. C., and T. H. Laby. Tables of Physical and Chemical Constants and Some Mathematical Functions.
#' 16th edition. Essex, England; New York: Longman Sc & Tech, 1995. Table 4.2.1:
#' \url{http://www.kayelaby.npl.co.uk/atomic_and_nuclear_physics/4_2/4_2_1.html}.
#'
#' Salem, S.I., S.L. Panossian, and R.A. Krause. "Experimental K and L Relative X-Ray Emission Rates."
#' Atomic Data and Nuclear Data Tables 14, no. 2 (August 1974): 91–109.
#' \url{https://doi.org/10.1016/S0092-640X(74)80017-3}.
#'
"x_ray_energies"

#' @rdname x_ray_energies
"x_ray_intensities"

#' XRF quantification energies
#'
#' Common energies used to quantify elements in XRF, including edge energies (energy
#' needed to produce that line on the spectrum).
#'
#' @source
#' NIST (2018)
#'
#' @references
#' National Institute of Standards and Technology (NIST): X-Ray Transition Energies Database.
#' Retrieved August 2018. \url{https://physics.nist.gov/PhysRefData/XrayTrans/Html/search.html}.
#'
"xrf_energies"

#' Get XRF energies for selected elements
#'
#' @param elements Elements or element lists (passed to \link{xrf_element_list}).
#' @param beam_energy_kev Beam energy
#' @param ... Used to further \link[dplyr]{filter} the result.
#'
#' @return A subset of \link{xrf_energies}
#' @export
#'
#' @examples
#' xrf_get_energies("major", 25)
#'
xrf_get_energies <- function(elements = "everything", beam_energy_kev = Inf, ...) {
  elements <- xrf_element_list(elements)
  xrf::xrf_energies %>%
    dplyr::filter(
      .data$element %in% !!elements,
      .data$edge_kev <= !!beam_energy_kev,
      ...
    )
}

#' Read XRF example spectra
#'
#' @param .dir The subdirectory from which to read
#' @param ... Used to \link[dplyr]{filter} spectra
#' @param .which Used to subset files before they are read. Use TRUE for all.
#'
#' @return A spectra tibble
#' @export
#'
#' @examples
#' read_xrf_example(.which = 1:10)
#' read_xrf_example(SampleIdent == "oreas 22d")
#'
read_xrf_example <- function(..., .dir = c("Panalytical"), .which = TRUE) {
  .dir <- match.arg(.dir)
  example_dir <- system.file("spectra_files", package = "xrf")
  if(.dir == "Panalytical") {
    subdir <- file.path(example_dir, .dir)
    spectra <- read_xrf_panalytical(
      list.files(subdir, "\\.mp2$", full.names = TRUE)[.which]
    )
  }

  dplyr::filter(spectra, ...)
}
