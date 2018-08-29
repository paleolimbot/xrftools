context("test-baseline.R")

test_that("SNIP baseline works as expected", {
  spectra <- read_xrf_example(.which = 1)
  spectra_baseline <- xrf_add_baseline_snip(spectra)
  expect_true("baseline" %in% names(spectra_baseline$.spectra[[1]]))
  expect_identical(
    spectra_baseline$.spectra[[1]]$baseline,
    Peaks::SpectrumBackground(spectra$.spectra[[1]]$cps)
  )
})

test_that("baseline package functions work as expected", {
  spectra <- read_xrf_example(.which = 1)
  spectra_baseline <- xrf_add_baseline_pkg(spectra, method = "als", p = 0.003, .clamp = -Inf)
  expect_true("baseline" %in% names(spectra_baseline$.spectra[[1]]))
  expect_identical(
    spectra_baseline$.spectra[[1]]$baseline,
    baseline::getBaseline(
      baseline::baseline(
        matrix(spectra$.spectra[[1]]$cps, nrow = 1),
        method = "als", p = 0.003
      )
    )[1, , drop = TRUE]
  )
})

test_that("tidy evaluation works in baseline package functions", {
  spectra <- tidyr::crossing(
    tibble::tibble(p_val = c(0.003, 0.01, 0.05)),
    read_xrf_example(.which = 1)
  )

  spectra_baseline <- xrf_add_baseline_pkg(spectra, method = "als", p = p_val, .clamp = -Inf)

  expect_identical(
    spectra_baseline$.spectra[[1]]$baseline,
    baseline::getBaseline(
      baseline::baseline(
        matrix(spectra$.spectra[[1]]$cps, nrow = 1),
        method = "als", p = 0.003
      )
    )[1, , drop = TRUE]
  )

  expect_identical(
    spectra_baseline$.spectra[[2]]$baseline,
    baseline::getBaseline(
      baseline::baseline(
        matrix(spectra$.spectra[[2]]$cps, nrow = 1),
        method = "als", p = 0.01
      )
    )[1, , drop = TRUE]
  )
})
