context("test-smooth.R")

test_that("gaussian smoothing works", {
  spec <- read_xrf_example(.which = 3)
  smoothed <- xrf_add_smooth_gaussian(spec, window = 3L, alpha = 2.5, tails = TRUE, .iter = 0)
  expect_true("smooth" %in% names(smoothed$.spectra[[1]]))
  expect_identical(
    smoothed$.spectra[[1]]$smooth,
    smoother::smth.gaussian(spec$.spectra[[1]]$cps, window = 3L, alpha = 2.5, tails = TRUE)
  )

  # iterated
  smoothed2 <- xrf_add_smooth_gaussian(spec, window = 3L, alpha = 2.5, tails = TRUE, .iter = 1)
  expect_identical(
    smoothed2$.spectra[[1]]$smooth,
    smoother::smth.gaussian(spec$.spectra[[1]]$cps, window = 3L, alpha = 2.5, tails = TRUE) %>%
      smoother::smth.gaussian(window = 3L, alpha = 2.5, tails = TRUE)
  )
})
