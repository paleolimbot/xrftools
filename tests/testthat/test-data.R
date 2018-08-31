context("test-data.R")

test_that("read_example filters and slices its output", {
  expect_equal(nrow(read_xrf_example(.which = 1:4)), 4)
  expect_true(all(read_xrf_example(SampleIdent == "oreas 22d")$SampleIdent == "oreas 22d"))
})

test_that("xrf energy extractor works", {
  expect_identical(xrf_get_energies(), xrf_energies)
  expect_identical(xrf_get_energies(beam_energy_kev = 0), xrf_energies %>% dplyr::slice(0))
  expect_setequal(xrf_get_energies(c("Pb", "Cu", "Zn"))$element, c("Pb", "Cu", "Zn"))
})
