context("test-read_spectra.R")

test_that("panalytical spectra work with one or multiple files", {
  pan_spec_zero <- read_spec_panalytical(character(0))
  expect_equal(nrow(pan_spec_zero), 0)
  expect_true(all(c(".path", ".spectra") %in% colnames(pan_spec_zero)))
  expect_true(tibble::is_tibble(pan_spec_zero))

  pan_dir <- system.file("spectra_files/Panalytical", package = "xrf")
  pan_spec <- read_spec_panalytical(file.path(pan_dir, "Panalytical_2017-08-1632-Omnian.mp2"))
  expect_equal(nrow(pan_spec), 1)
  expect_true(all(c(".path", ".spectra") %in% colnames(pan_spec)))
  expect_true(tibble::is_tibble(pan_spec))

  pan_files_all <- list.files(pan_dir, ".mp2$", full.names = TRUE)
  pan_spec_all <- read_spec_panalytical(pan_files_all)
  expect_equal(nrow(pan_spec_all), length(pan_files_all))
  expect_true(all(c(".path", ".spectra") %in% colnames(pan_spec_all)))
  expect_true(tibble::is_tibble(pan_spec_all))
})
