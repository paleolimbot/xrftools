
#' Physical parameters of electron shell transitions
#'
#' See Details
#'
#' Absoroption jump ratios: vectorized over both element and shell.
#' The absorption jump (often written \code{r} sub [K, L1, L2, L3])
#' can be used to calculate the probability that an electron from \code{shell} will be
#' ejected (J sub [K, L1, L2, L3]; the absorption jump ratio).
#'
#' Transition probabilities are the probability that a specified transition takes
#' place rather than another in that shell. Generally given the letter "g" sub [KL2, etc.].
#' See also \link{x_ray_emission_probabilities}.
#'
#' Fluorescence yields are the probability of emission of x-radiation rather than
#' an Auger electron. Generally given the letter omega sub [K, L1, L2, L3]. For
#' L shells, there are also Coster-Kronig (1935) transitions.
#'
#' xrf_relative_peak_intensity attempts to combine these functions to estimate
#' the relative intensities of various peaks. This is an important input for
#' deconvolution, and needs to be tweaked based on sample size.
#'
#'
#' @param element An element symbol or atomic number
#' @param shell A valid electron shell (K, L1, L2, L3). Invalid shells are silently ignored.
#' @param trans A transition (e.g., KL2).
#' @param coster_kronig_trans A sub-transition according to Coster-Kronig (1935).
#' @param beam_energy_kev The beam energy used to excite the electrons. Currently unused, but
#'   should be incorporated into the calculation.
#' @param method Only the Viegele (1973) method is currently supported for xrf_absorption_jump,
#'   and EADL97 tables are used for other parameters.
#'
#' @return A vector of numerics
#' @export
#'
#' @references
#' D.E. Cullen, et al., "Tables and Graphs of Atomic Subshell and
#' Relaxation Data Derived from the LLNL Evaluated Atomic Data Library
#' (EADL), Z = 1 - 100," Lawrence Livermore National Laboratory, UCRL-50400,
#' Vol. 30, October 1991. \url{http://www-nds.iaea.org/epdl97/libsall.htm}.
#'
#' Coster, D. and Kronig De L. (1935). New type of auger effect and its influence on the x-ray spectrum.
#' Pysica 2:13-24. \url{https://doi.org/10.1016/S0031-8914(35)90060-X}.
#'
#' Veigele, W.J. (1973) Atomic and Nuclear Data Tables 5:51-111. p 54 and 55.
#' \url{https://doi.org/10.1016/S0092-640X(73)80015-4}.
#'
#' @examples
#' # absorption jump ratio
#' xrf_absorption_jump("Pb", c("K", "L1", "L2", "L3"))
#'
#' # probability that a K shell electron will be ejected
#' rK <- xrf_absorption_jump("Pb", c("K", "L1", "L2", "L3"))
#' JK <- (rK - 1) / rK
#' xrf_absorption_jump_ratio("Pb", c("K", "L1", "L2", "L3"))
#'
xrf_absorption_jump <- function(element, shell, method = "viegele") {
  method <- match.arg(method)

  if(is.character(element)) {
    z <- match(element, all_elements)
  } else {
    z <- as.integer(element)
  }

  stopifnot(
    all(z > 0, z < 100),
    (length(element) == length(shell)) || (length(element) == 1) || (length(shell) == 1)
  )

  # constant for L1, L2, a / z + c for K, L3
  a <- c("K" = 125, "L1" = 1, "L2" = 1, "L3" = 80.0)[shell]
  b <- c("K" = 1, "L1" = Inf, "L2" = Inf, "L3" = 1)[shell]
  c <- c("K" = 3.5, "L1" = 1.2, "L2" = 1.4, "L3" = 1.5)[shell]

  unname((a / (b * z)) + c)
}

#' @rdname xrf_absorption_jump
#' @export
xrf_absorption_jump_ratio <- function(element, shell, method = "viegele") {
  method <- match.arg(method)
  rK <- xrf_absorption_jump(element = element, shell = shell, method = method)
  (rK - 1) / rK
}

#' @rdname xrf_absorption_jump
#' @export
xrf_transition_probability <- function(element, trans, method = "EADL97") {
  method <- match.arg(method)

  if(is.numeric(element)) {
    element <- all_elements[element]
  }
  stopifnot(is.character(trans))

  tibble::tibble(element = element, trans = trans) %>%
    dplyr::left_join(xrftools::x_ray_emission_probabilities, by = c("element", "trans")) %>%
    dplyr::pull("emission_probability")
}

#' @rdname xrf_absorption_jump
#' @export
xrf_fluorescence_yield <- function(element, shell, method = "EADL97") {
  method <- match.arg(method)

  if(is.numeric(element)) {
    element <- all_elements[element]
  }
  stopifnot(is.character(shell))

  tibble::tibble(element = element, shell = shell) %>%
    dplyr::left_join(xrftools::x_ray_fluorescence_yields, by = c("element", "shell")) %>%
    dplyr::pull("fluorescence_yield")
}

#' @rdname xrf_absorption_jump
#' @export
xrf_coster_kronig_probability <- function(element, shell, coster_kronig_trans, method = "EADL97") {
  method <- match.arg(method)
  if(is.numeric(element)) {
    element <- all_elements[element]
  }
  stopifnot(is.character(shell))

  tibble::tibble(element = element, shell = shell, coster_kronig_trans = coster_kronig_trans) %>%
    dplyr::left_join(xrftools::x_ray_coster_kronig_probabilities, by = c("element", "shell", "coster_kronig_trans")) %>%
    dplyr::pull("coster_kronig_prob")
}

#' @rdname xrf_absorption_jump
#' @export
xrf_relative_peak_intensity <- function(element, shell, trans, beam_energy_kev = 50) {
  xrf_absorption_jump_ratio(element, shell) *
    xrf_transition_probability(element, trans) *
    xrf_fluorescence_yield(element, shell)
}

#' Get XRF energies for selected elements
#'
#' @param elements Elements or element lists (passed to \link{xrf_element_list}).
#' @param beam_energy_kev Beam energy, used to filter the list and calculate relative peak intensities.
#' @param min_relative_intensity Smallest peak to include, relatie to the maximum peak for a given
#'   element.
#' @param ... Used to further \link[dplyr]{filter} the result.
#'
#' @return A subset of \link{x_ray_xrf_energies} with some additional information based on the beam energy.
#' @export
#'
#' @examples
#' xrf_energies("major", 25)
#'
xrf_energies <- function(elements = "everything", beam_energy_kev = 50, ..., min_relative_intensity = 0.01) {
  elements <- xrf_element_list(elements)

  xrftools::x_ray_xrf_energies %>%
    dplyr::mutate(
      relative_peak_intensity = xrf_relative_peak_intensity(
        .data$element, .data$edge, .data$trans, !!beam_energy_kev
      )
    ) %>%
    filter(!is.na(.data$relative_peak_intensity)) %>%
    dplyr::group_by(.data$element) %>%
    dplyr::mutate(relative_peak_intensity = .data$relative_peak_intensity / max(.data$relative_peak_intensity)) %>%
    dplyr::filter(
      .data$element %in% !!elements,
      .data$edge_kev <= !!beam_energy_kev,
      .data$relative_peak_intensity >= min_relative_intensity,
      ...
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$z, dplyr::desc(.data$relative_peak_intensity)) %>%
    dplyr::select("element", "trans", "trans_siegbahn", "energy_kev", "relative_peak_intensity", dplyr::everything())
}
