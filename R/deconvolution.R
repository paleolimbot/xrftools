
#' Deconvolute a spectrum using gaussian least squares
#'
#' @inheritParams xrf_add_deconvolution_fun
#' @param .values,response Values to use for the deconvolution
#' @param energy_kev,.energy_kev Energy values corresponding to response/.values
#' @param energy_min_kev,energy_max_kev Constrain the energies that should be deconvoluted
#' @param peaks A data frame of columns with columns "element", "
#' @param default_sigma The default standard deviation of the peaks (can also be passed in as
#'   a columm in \code{peaks} if this value varies between peaks)
#'
#' @return A modified version of .spectra
#' @export
#'
xrf_add_deconvolution_gls <- function(.spectra, .energy_kev = .data$.spectra$energy_kev,
                                      .values = .data$.spectra$cps - .data$.spectra$baseline,
                                      energy_min_kev = -Inf, energy_max_kev = Inf,
                                      peaks = xrf_energies(), default_sigma = 0.07, .env = parent.frame()) {
  .energy_kev <- enquo(.energy_kev)
  .values <- enquo(.values)
  peaks <- enquo(peaks)
  default_sigma <- enquo(default_sigma)
  energy_min_kev <- enquo(energy_min_kev)
  energy_max_kev <- enquo(energy_max_kev)

  xrf_add_deconvolution_fun(
    .spectra,
    xrf_deconvolute_gaussian_least_squares,
    !!.energy_kev,
    !!.values,
    peaks = !!peaks,
    default_sigma = !!default_sigma,
    energy_min_kev = !!energy_min_kev,
    energy_max_kev = !!energy_max_kev,
    .env = .env
  )
}

#' @rdname xrf_add_deconvolution_gls
#' @export
xrf_deconvolute_gaussian_least_squares <- function(energy_kev, response, peaks = xrf_energies(), default_sigma = 0.07,
                                                   energy_min_kev = -Inf, energy_max_kev = Inf) {

  stopifnot(
    "energy_kev" %in% colnames(peaks),
    "element" %in% colnames(peaks),
    is.numeric(energy_kev), is.numeric(response),
    all(is.finite(energy_kev)), all(is.finite(response)),
    length(energy_kev) == length(response)
  )

  if(!("relative_peak_intensity" %in% colnames(peaks))) {
    peaks$relative_peak_intensity <- 1
  } else {
    stopifnot(is.numeric(peaks$relative_peak_intensity))
  }

  if(!("sigma" %in% colnames(peaks))) {
    peaks$sigma <- default_sigma
  } else {
    stopifnot(is.numeric(peaks$sigma))
    peaks$sigma <- dplyr::coalesce(peaks$sigma, default_sigma)
  }

  # filter energies and responses
  within_range <- (energy_kev >= energy_min_kev) & (energy_kev <= energy_max_kev)
  energy_kev <- energy_kev[within_range]
  response <- response[within_range]

  # check finite-ness of inputs
  stopifnot(
    all(is.finite(peaks$sigma)),
    all(is.finite(peaks$relative_peak_intensity))
  )

  # filter, normalize peak heights (scale to sigma)
  # the peak with a relative height of 1 should be the peak with the greatest
  # intensity
  peaks <- peaks %>%
    dplyr::filter(
      .data$energy_kev >= !!energy_min_kev,
      .data$energy_kev <= !!energy_max_kev,
      .data$relative_peak_intensity > 0
    ) %>%
    dplyr::group_by(.data$element) %>%
    dplyr::mutate(
      relative_peak_intensity = .data$relative_peak_intensity / max(.data$relative_peak_intensity),
      relative_peak_height = .data$relative_peak_intensity / .data$sigma
    ) %>%
    dplyr::mutate(
      relative_peak_height = .data$relative_peak_height /
        .data$relative_peak_height[which.max(.data$relative_peak_intensity)]
    ) %>%
    dplyr::ungroup()

  # calculate gaussian responses for each row in peaks
  # summarise combined response per element
  responses <- peaks %>%
    dplyr::select("element", "energy_kev", "sigma", "relative_peak_height", "relative_peak_intensity") %>%
    dplyr::mutate(
      response_element = purrr::pmap(
        list(.data$energy_kev, .data$sigma, .data$relative_peak_height),
        function(mu, sigma, height) gaussian_fun(!!energy_kev, mu = mu, sigma = sigma, height = height)
      )
    ) %>%
    # sum all the responses per element
    dplyr::group_by(.data$element) %>%
    dplyr::summarise(
      primary_energy_kev = .data$energy_kev[which.max(.data$relative_peak_intensity)],
      primary_sigma = .data$sigma[which.max(.data$relative_peak_intensity)],
      response_element = list(purrr::reduce(.data$response_element, `+`))
    ) %>%
    dplyr::ungroup()

  # least squares estimation of coefficients (peak height of peak with relative height of 1)
  X <- do.call(cbind, responses$response_element)
  colnames(X) <- responses$element
  df <- tibble::as_tibble(cbind(.response = response, X))
  . <- NULL; rm(.); .response <- NULL; rm(.response) # CMD hack for formula
  fit <- stats::lm(.response ~ 0 + ., data = df)

  # add info to df to return
  df$.response_fit <- stats::predict(fit)
  df$.energy_kev <- energy_kev
  df <- dplyr::select(df, dplyr::starts_with("."), dplyr::everything())

  # create coefficient info to return
  fit_sum <- summary(fit)
  responses$response_element <- NULL
  responses$height <- stats::coefficients(fit)[responses$element]
  responses$height_se <- fit_sum$coefficients[, 2, drop = TRUE][responses$element]
  responses$peak_area <- responses$height * responses$primary_sigma * sqrt(2 * pi)
  responses$peak_area_se <- responses$height_se * responses$primary_sigma * sqrt(2 * pi)

  components <- tibble::as_tibble(X) %>%
    dplyr::mutate(energy_kev = energy_kev) %>%
    tidyr::gather("element", "response_fit", -"energy_kev") %>%
    dplyr::left_join(responses %>% dplyr::select("element", "height"), by = "element") %>%
    dplyr::mutate(response_fit = .data$response_fit * .data$height)

  structure(
    list(
      fit = tibble::tibble(
        r2 = fit_sum$r.squared,
        r2_adj = fit_sum$adj.r.squared,
        chi_sq = sum(fit_sum$residuals^2 / df$.response)
      ),
      response = df %>%
        dplyr::select(dplyr::starts_with(".")) %>%
        dplyr::rename_all(stringr::str_remove, "^\\."),
      components = components,
      peaks = responses
    ),
    class = "deconvolution_fit"
  )
}

#' Add a deconvolution based on a function
#'
#' @param .spectra A \link{xrf_spectra} tibble.
#' @param .fun A function that
#' @param ... Passed to .fun
#' @param .env Calling environment
#'
#' @return .spectra with a .deconvolution column, and a deconvolution column added to
#'   the .spectra column
#' @export
#'
xrf_add_deconvolution_fun <- function(.spectra, .fun, ..., .env = parent.frame()) {
  dots <- quos(...)
  fun_wrap <- function(spectrum) {
    # args evaluated within each row
    args <- purrr::map(dots, rlang::eval_tidy, data = spectrum, env = .env)
    do.call(.fun, args)
  }

  # calculate deconvolutions
  deconvs <- purrr::map(purrr::transpose(.spectra), fun_wrap)

  # place each element of the output in a column in the data frame

  # check for named lists
  deconv_not_list <- !purrr::map_lgl(deconvs, is.list)
  if(any(deconv_not_list)) {
    stop(
      "Result of deconvolution function was not a list at positions ",
      paste(which(deconv_not_list), collapse = ", ")
    )
  }

  deconv_names <- purrr::map(deconvs, names)
  deconv_no_names <- purrr::map_lgl(
    deconv_names,
    function(x) is.null(x) || ("" %in% x)
  )

  if(any(deconv_no_names)) {
    stop(
      "Result of deconvolution function had empty or missing names at positions ",
      paste(which(deconv_no_names), collapse = ", ")
    )
  }

  for(name in unique(do.call(c, deconv_names))) {
    .spectra[[paste(".deconvolution", name, sep = "_")]] <- purrr::map(deconvs, name)
  }

  .spectra
}

gaussian_fun <- function(energy_kev, mu = 0, sigma = 1, height = 1) {
  height * exp(-0.5 * ((energy_kev - mu) / sigma) ^ 2)
}
