
#' Elemental X-ray energies and intensities
#'
#' Transition energies that may be found in X-Ray spectra and that may be useful in the analysis
#' of XRF data. This dataset (from NIST 2018) includes most transition energies
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

#' X-Ray transitions
#'
#' A data frame keeping track of the formal and siegbahn names of various transitions
#'
"x_ray_transitions"


#' EADL97 Atomic data
#'
#' Data from the EADL97 database.
#'
#' @source
#' The PyMca5 python package (\url{https://pypi.org/project/PyMca5/#files}),
#' which obtained the data from the EADL page (\url{http://www-nds.iaea.org/epdl97/libsall.htm})
#'
#' @references
#' D.E. Cullen, et al., "Tables and Graphs of Atomic Subshell and
#' Relaxation Data Derived from the LLNL Evaluated Atomic Data Library
#' (EADL), Z = 1 - 100," Lawrence Livermore National Laboratory, UCRL-50400,
#' Vol. 30, October 1991
#'
"x_ray_cross_sections"

#' @rdname x_ray_cross_sections
"x_ray_fluorescence_yields"

#' @rdname x_ray_cross_sections
"x_ray_emission_probabilities"

#' @rdname x_ray_cross_sections
"x_ray_coster_kronig_probabilities"

#' XRF quantification energies
#'
#' A form of \link{x_ray_energies} in a more suitable form for XRF quantification.
#' Best accessed via \link{xrf_energies}.
#'
#' @source
#' NIST (2018), EADL97. See \link{x_ray_energies} and \link{x_ray_emission_probabilities}.
#'
#' @references
#' National Institute of Standards and Technology (NIST): X-Ray Transition Energies Database.
#' Retrieved August 2018. \url{https://physics.nist.gov/PhysRefData/XrayTrans/Html/search.html}.
#'
"x_ray_xrf_energies"

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
  example_dir <- system.file("spectra_files", package = "xrftools")
  if(.dir == "Panalytical") {
    subdir <- file.path(example_dir, .dir)
    spectra <- read_xrf_panalytical(
      list.files(subdir, "\\.mp2$", full.names = TRUE)[.which]
    )
  }

  dplyr::filter(spectra, ...)
}
