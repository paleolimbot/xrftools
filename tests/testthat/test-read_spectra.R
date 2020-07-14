context("test-read_spectra.R")

test_that("panalytical spectra work with one or multiple files", {
  pan_xrf_zero <- read_xrf_panalytical(character(0))
  expect_equal(nrow(pan_xrf_zero), 0)
  expect_is(pan_xrf_zero, "spectra")
  expect_true(all(c(".path", ".spectra") %in% colnames(pan_xrf_zero)))
  expect_true(tibble::is_tibble(pan_xrf_zero))

  pan_dir <- system.file("spectra_files/Panalytical", package = "xrftools")
  pan_spec <- read_xrf_panalytical(file.path(pan_dir, "Panalytical_2017-08-1632-Omnian.mp2"))
  expect_is(pan_spec, "spectra")
  expect_equal(nrow(pan_spec), 1)
  expect_true(all(c(".path", ".spectra") %in% colnames(pan_spec)))
  expect_true(tibble::is_tibble(pan_spec))

  pan_files_all <- list.files(pan_dir, ".mp2$", full.names = TRUE)
  pan_xrf_all <- read_xrf_panalytical(pan_files_all)
  expect_is(pan_xrf_all, "spectra")
  expect_equal(nrow(pan_xrf_all), length(pan_files_all))
  expect_true(all(c(".path", ".spectra") %in% colnames(pan_xrf_all)))
  expect_true(tibble::is_tibble(pan_xrf_all))
})

test_that("meta parsing works properly", {
  pan_dir <- system.file("spectra_files/Panalytical", package = "xrftools")
  pan_spec <- read_xrf_meta_panalytical(
    file.path(pan_dir, "Panalytical_2017-08-1632-Omnian.mp2"),
    parse_meta = TRUE
  )
  pan_spec_charmeta <- read_xrf_meta_panalytical(
    file.path(pan_dir, "Panalytical_2017-08-1632-Omnian.mp2"),
    parse_meta = FALSE
  )

  expect_false(all(pan_spec %>% dplyr::select(-.position) %>% purrr::map_lgl(is.character)))
  expect_true(all(pan_spec_charmeta %>% dplyr::select(-.position) %>% purrr::map_lgl(is.character)))


  pan_files_all <- list.files(pan_dir, ".mp2$", full.names = TRUE)
  pan_spec_all <- read_xrf_meta_panalytical(pan_files_all, parse_meta = TRUE)
  pan_spec_all_charmeta <- read_xrf_meta_panalytical(pan_files_all, parse_meta = FALSE)

  expect_false(all(pan_spec_all %>% dplyr::select(-.position) %>% purrr::map_lgl(is.character)))
  expect_true(all(pan_spec_all_charmeta %>% dplyr::select(-.position) %>% purrr::map_lgl(is.character)))
})
