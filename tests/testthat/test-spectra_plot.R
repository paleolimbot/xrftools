context("test-spectr_plot.R")

test_that("plots are generated", {

  pan <- read_xrf_example(SampleIdent %in% c("oreas 22d", "oreas 24b"))

  expect_is(pan %>% dplyr::slice(1) %>% ggplot2::autoplot(), "ggplot")

  print(
    xrftools:::autoplot.spectra(
      pan, energy_kev < kV,
      y = smooth - baseline,
      col = SampleIdent, facet = ConditionSet
    )
  )
})

test_that("xrf peaks are labelled", {

  pan <- read_xrf_example(SampleIdent %in% c("oreas 22d", "oreas 24b"), ConditionSet == "Omnian")
  p <- ggplot2::autoplot(
    pan, energy_kev < kV,
    y = smooth - baseline,
    col = SampleIdent
  ) +
    ggplot2::scale_y_sqrt()

  print(p + stat_xrf_peaks(element_list = c("Fe", "Mn", "Pb")))
  print(p + stat_xrf_peaks(energy_subset = energy_kev > 10 & energy_kev < 20, nudge_y = 50))
  expect_true(TRUE)
})
