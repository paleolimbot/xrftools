context("test-deconvolution.R")

test_that("test deconvolution", {

  test_df <- tibble::tibble(
    energy_kev = seq(1, 20, 0.1),
    response = xrf:::gaussian_fun(energy_kev, mu = 5, sigma = 1, height = 4) +
      xrf:::gaussian_fun(energy_kev, mu = 10, sigma = 1, height = 2) +
      rnorm(length(energy_kev), mean = 0, sd = 0.05)
  )

  deconv <- xrf_deconvolute_gaussian_least_squares(
    test_df$energy_kev, test_df$response,
    peaks = tibble::tibble(element = c("a", "b"), energy_kev = c(5, 10)),
    sigma = 1
  )

  # make sure peaks line up and are the right height
  expect_equal(deconv$peaks$element, c("a", "b"))
  expect_true(all(abs(deconv$peaks$height - c(4, 2)) < 0.05))

  # plot(test_df, type = "l", col = "blue")
  # lines(deconv$deconvolution$.energy_kev, deconv$deconvolution$.response_fit)

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
  expect_setequal(names(deconv), c("fit", "deconvolution", "peaks"))
  expect_is(deconv$fit, "data.frame")
  expect_is(deconv$deconvolution, "data.frame")
  expect_is(deconv$peaks, "data.frame")

  # rows of output
  expect_equal(nrow(deconv$peaks), dplyr::n_distinct(xrf_energies("lake_sediment")$element))
  expect_equal(nrow(deconv$fit), 1)
  expect_equal(nrow(deconv$deconvolution), nrow(spec_simple))
})

test_that("deconvolution works on spectra objects", {
  spec <- read_xrf_example(.which = 7) %>%
    dplyr::select(SampleIdent, ConditionSet, kV) %>%
    xrf_add_baseline_snip(.values = .spectra$cps, iterations = 20) %>%
    xrf_add_smooth_filter(filter = xrf_filter_gaussian(width = 7, alpha = 2.5), .iter = 5)

  spec_simple <- spec %>%
    dplyr::pull(.spectra) %>%
    dplyr::first() %>%
    dplyr::filter(energy_kev <= 7.5)

  deconv <- xrf_deconvolute_gaussian_least_squares(
    spec_simple$energy_kev,
    spec_simple$cps - spec_simple$baseline,
    peaks = xrf_energies("lake_sediment")
  )

  spec_deconv <- spec %>%
    xrf_add_deconvolution_gls(energy_max_kev = 7.5, peaks = xrf_energies("lake_sediment"))

  expect_identical(spec_deconv$.deconvolution[[1]], deconv)
})
