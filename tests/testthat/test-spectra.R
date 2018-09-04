context("test-spectra.R")

test_that("spectra objects can be created", {
  spec <- xrf_spectra(tibble::tibble(energy_kev = 1:5, cps = 0), info1 = "info1")
  expect_is(spec, "spectra")
  expect_is(spec, "data.frame")
  expect_true(all(c("info1", ".position", ".path", ".spectra") %in% colnames(spec)))
  expect_equal(nrow(spec), 1)
  expect_identical(validate_spectra(spec), spec)

  # .spectra as a list instead of a df
  expect_identical(
    xrf_spectra(tibble::tibble(energy_kev = 1:5, cps = 0), info1 = "info1"),
    xrf_spectra(list(tibble::tibble(energy_kev = 1:5, cps = 0)), info1 = "info1")
  )
})

test_that("validate spectra catches problems", {
  good_spec <- xrf_spectra(tibble::tibble(energy_kev = 1:5, cps = 0), info1 = "info1")
  expect_silent(validate_spectra(good_spec))

  expect_error(validate_spectra(unclass(good_spec)), "inherits")
  expect_error(validate_spectra(structure(unclass(good_spec), class = "spectra")), "tibble")
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.position = as.character(.position)) %>%
      new_spectra() %>%
      validate_spectra(),
    "is\\.numeric"
  )
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.path = as.integer(.path)) %>%
      new_spectra() %>%
      validate_spectra(),
    "is\\.character"
  )
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.spectra = 1) %>%
      new_spectra() %>%
      validate_spectra(),
    "is\\.list"
  )
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.spectra = list(1)) %>%
      new_spectra() %>%
      validate_spectra(),
    "Some \\.spectra are not data frames"
  )
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.spectra = list(tibble::tibble(energy_kev = 1))) %>%
      new_spectra() %>%
      validate_spectra(),
    "Some \\.spectra are missing column 'cps'"
  )
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.spectra = list(tibble::tibble(cps = "not numeric", energy_kev = 1))) %>%
      new_spectra() %>%
      validate_spectra(),
    "cps are not numeric"
  )
  expect_error(
    good_spec %>%
      xrf_despectra() %>%
      dplyr::mutate(.spectra = list(tibble::tibble(cps = 1, energy_kev = "not numeric"))) %>%
      new_spectra() %>%
      validate_spectra(),
    "energy_kev are not numeric"
  )
})

test_that("data frame s3 methods work on spectra objects", {
  spec <- xrf_spectra(tibble::tibble(energy_kev = 1:5, cps = 0), info1 = "info1")
  expect_identical(spec, dplyr::filter(spec))
  expect_identical(spec, dplyr::slice(spec, 1))
  expect_identical(spec, dplyr::select(spec, dplyr::everything()))
  expect_identical(spec, dplyr::select(spec, info1))
  expect_identical(spec, head(spec))
  expect_identical(spec, tail(spec))
  expect_identical(spec, spec[TRUE, ])
  expect_identical(spec, spec %>% dplyr::mutate())

  expect_identical(
    xrf_combine_spectra(spec, spec),
    xrf_combine_spectra(list(spec, spec))
  )
  expect_identical(
    xrf_combine_spectra(spec, spec),
    rbind(spec, spec)
  )
  expect_equal(nrow(xrf_combine_spectra(spec, spec)), 2)
  expect_is(xrf_combine_spectra(spec, spec), "spectra")

  expect_true("test2" %in% colnames(xrf_combine_spectra(t1 = spec, t2 = spec, .id = "test2")))
})
