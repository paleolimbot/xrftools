context("test-units.R")

test_that("energy/wavelength functions work", {
  # Fe:
  # energy(0.1936) # from Wiki
  # wavelength(6.40) # from Kenna et al. 2011

  expect_identical(0.1936, wavelength(energy(0.1936)))
  expect_identical(6.40, energy(wavelength(6.40)))
  expect_true(abs(energy(0.1936) - 6.40) < 0.01)
  expect_true(abs(wavelength(6.40) - 0.1936) < 0.001)
})
