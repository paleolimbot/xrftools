
#' Wavelenth/Energy X-Ray conversion
#'
#' @param wavelength_nm The wavelength, in nanometers
#' @param energy_kev The energy, in keV
#'
#' @export
#'
#' @examples
#' # Fe:
#' energy(0.1936) # from Wiki
#' wavelength(6.40) # from Kenna et al. 2011
#'
energy <- function(wavelength_nm) {
  plancks_constant_kev * speed_of_light_nm / wavelength_nm
}

#' @rdname energy
#' @export
wavelength <- function(energy_kev) {
  plancks_constant_kev * speed_of_light_nm / energy_kev
}

# planck's constant: 6.62607004 × 10-34 m^2 kg / s
# or 4.135667662 × 10−15 eV * s
plancks_constant_si <- 6.62607004e-34
plancks_constant_kev <- 4.135667662e-15 * 1e-3 # keV per eV

# speed of light: 299792458 m / s
speed_of_light_si <- 299792458
speed_of_light_nm <- 299792458 * 1e9 # nanometers per meter

