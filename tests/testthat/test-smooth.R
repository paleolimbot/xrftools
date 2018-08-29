context("test-smooth.R")

test_that("smoothing works", {
  spec <- read_xrf_example(.which = 3)
  smoothed <- xrf_add_smooth_filter(spec, filter = xrf_filter_gaussian(width = 3, alpha = 2.5), .iter = 1)
  expect_true("smooth" %in% names(smoothed$.spectra[[1]]))
  expect_equal(
    smoothed$.spectra[[1]]$smooth[c(-1, -nrow(smoothed$.spectra[[1]]))],
    stats::filter(spec$.spectra[[1]]$cps, filter = xrf_filter_gaussian(width = 3, alpha = 2.5)) %>%
      as.numeric() %>%
      .[c(-1, -nrow(smoothed$.spectra[[1]]))]
  )
})

test_that("filter generating functions work", {
  expect_equal(sum(xrf_filter_gaussian()), 1)
  expect_equal(sum(xrf_filter_pyramid()), 1)
})
