context("test-deconvolution.R")

test_that("test deconvolution", {

  test_df <- tibble::tibble(
    energy_kev = seq(1, 30, 0.1),
    response = xrftools:::gaussian_fun(energy_kev, mu = 5, sigma = 1, height = 4) +
      xrftools:::gaussian_fun(energy_kev, mu = 10, sigma = 2, height = 2) +
      xrftools:::gaussian_fun(energy_kev, mu = 15, sigma = 0.5, height = 1) +
      rnorm(length(energy_kev), mean = 0, sd = 0.05)
  )

  deconv <- xrf_deconvolute_gaussian_least_squares(
    test_df$energy_kev, test_df$response,
    peaks = tibble::tibble(
      element = c("a", "b", "b"),
      energy_kev = c(5, 10, 15),
      sigma = c(1, 2, 0.5),
      relative_peak_intensity = c(sigma * c(4, 2, 1))
    )
  )

  # make sure peaks line up and are the right height
  expect_equal(deconv$peaks$element, c("a", "b"))
  expect_true(all(abs(deconv$peaks$height - c(4, 2)) < 0.05))

  plot(test_df, type = "l", col = "blue")
  lines(deconv$response$energy_kev, deconv$response$response_fit)
  lines(
    deconv$components$energy_kev[deconv$components$element == "a"],
    deconv$components$response_fit[deconv$components$element == "a"],
    col = "purple"
  )
  lines(
    deconv$components$energy_kev[deconv$components$element == "b"],
    deconv$components$response_fit[deconv$components$element == "b"],
    col = "red"
  )
})

test_that("test base deconvolution function", {

  spec <- read_xrf_example(.which = 7) %>%
    xrf_add_baseline_snip(.values = .spectra$cps, iterations = 20) %>%
    xrf_add_smooth_filter(filter = xrf_filter_gaussian(width = 7, alpha = 2.5), .iter = 5)

  spec_simple <- spec %>%
    dplyr::pull(.spectra) %>%
    dplyr::first() %>%
    dplyr::filter(energy_kev <= 7.5)

  deconv <- xrf_deconvolute_gaussian_least_squares(
    spec_simple$energy_kev,
    spec_simple$smooth - spec_simple$baseline,
    peaks = xrf_energies("lake_sediment")
  )

  # types of output
  expect_is(deconv, "deconvolution_fit")
  expect_setequal(names(deconv), c("fit", "response", "components", "peaks"))
  expect_is(deconv$fit, "data.frame")
  expect_is(deconv$response, "data.frame")
  expect_is(deconv$components, "data.frame")
  expect_is(deconv$peaks, "data.frame")

  # rows of output
  expect_equal(nrow(deconv$peaks), dplyr::n_distinct(xrf_energies("lake_sediment")$element))
  expect_equal(nrow(deconv$fit), 1)
  expect_equal(nrow(deconv$response), nrow(spec_simple))
  expect_equal(nrow(deconv$components), nrow(spec_simple) * nrow(deconv$peaks))
})

test_that("deconvolution works on spectra objects", {
  spec <- read_xrf_example(.which = 7) %>%
    dplyr::select(SampleIdent, ConditionSet, kV) %>%
    xrf_add_baseline_snip(.values = .spectra$cps, iterations = 20) %>%
    xrf_add_smooth_filter(filter = xrf_filter_gaussian(width = 7, alpha = 2.5), .iter = 5)

  spec_simple <- spec %>%
    dplyr::pull(.spectra) %>%
    dplyr::first()

  deconv <- xrf_deconvolute_gaussian_least_squares(
    spec_simple$energy_kev,
    spec_simple$cps - spec_simple$baseline,
    peaks = xrf_energies("lake_sediment"),
    energy_max_kev = 7.5
  )

  spec_deconv <- spec %>%
    xrf_add_deconvolution_gls(energy_max_kev = 7.5, peaks = xrf_energies("lake_sediment"))

  expect_identical(spec_deconv$.deconvolution_components[[1]], deconv$components)
})

test_that("deconvolution function errors are caught", {
  spec <- read_xrf_example(.which = 7)
  expect_error(xrf_add_deconvolution_fun(spec, function(...) "not a list obj"), "not a list")
  expect_error(xrf_add_deconvolution_fun(spec, function(...) list(1)), "empty or missing names")
  expect_error(xrf_add_deconvolution_fun(spec, function(...) list(1, a = "fish")), "empty or missing names")
  expect_silent(xrf_add_deconvolution_fun(spec, function(...) list(a = 1, b = 2)))
})
