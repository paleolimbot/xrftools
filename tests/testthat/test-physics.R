context("physics")

test_that("jump ratios are calculated correctly", {

  expect_identical(xrf_absorption_jump(1:99, "K"), (125.0 / 1:99) + 3.5)
  expect_identical(xrf_absorption_jump(1:99, "L1"), rep(1.2, 99))
  expect_identical(xrf_absorption_jump(1:99, "L2"), rep(1.4, 99))
  expect_identical(xrf_absorption_jump(1:99, "L3"), (80 / 1:99) + 1.5)

  # element symbol or Z as input
  expect_identical(
    xrf_absorption_jump(1:5, "K"),
    xrf_absorption_jump(c("H", "He", "Li", "Be", "B"), "K")
  )

  # vectorized over shell
  expect_identical(
    xrf_absorption_jump("Pb", c("K", "L1", "L2", "L3")),
    c((125.0 / 82) + 3.5, 1.2, 1.4, (80 / 82) + 1.5)
  )

  # vectorized over both
  expect_identical(
    xrf_absorption_jump(c("H", "Pb", "H", "Pb"), c("K", "L1", "L2", "L3")),
    c((125.0 / 1) + 3.5, 1.2, 1.4, (80 / 82) + 1.5)
  )

  # problems are caught
  expect_error(xrf_absorption_jump("Fm", "K"), "z < 100")
  expect_error(xrf_absorption_jump(c("H", "Be"), c("K", "L1", "L2")), "is not TRUE")
  expect_identical(xrf_absorption_jump("H", "not a shell"), NA_real_)

  rK <- xrf_absorption_jump("Pb", c("K", "L1", "L2", "L3"))
  expect_identical(
    xrf_absorption_jump_ratio("Pb", c("K", "L1", "L2", "L3")),
    (rK - 1) / rK
  )
})

test_that("transition probabilities are correct", {
  expect_identical(
    xrf_transition_probability("C", "KL2"),
    x_ray_emission_probabilities %>%
      dplyr::filter(element == "C") %>%
      dplyr::filter(trans == "KL2") %>%
      dplyr::pull(emission_probability)
  )

  expect_identical(
    xrf_transition_probability(c("C", "N", "O"), "KL2"),
    xrf_transition_probability(c(6, 7, 8), "KL2")
  )
})

test_that("fluorescence yields are correct", {
  expect_identical(
    xrf_fluorescence_yield("C", "K"),
    x_ray_fluorescence_yields %>%
      dplyr::filter(element == "C") %>%
      dplyr::filter(shell == "K") %>%
      dplyr::pull(fluorescence_yield)
  )

  expect_identical(
    xrf_fluorescence_yield(c("C", "N", "O"), "K"),
    xrf_fluorescence_yield(c(6, 7, 8), "K")
  )
})

test_that("Coster Kronig transitions are correct", {
  expect_identical(
    xrf_coster_kronig_probability("Pb", "L1", "f12"),
    x_ray_coster_kronig_probabilities %>%
      dplyr::filter(element == "Pb", shell == "L1", coster_kronig_trans == "f12") %>%
      dplyr::pull(coster_kronig_prob)
  )

  expect_identical(
    xrf_coster_kronig_probability(c("Pb", "Zn", "U"), "L1", "f12"),
    xrf_coster_kronig_probability(c(82, 30, 92), "L1", "f12")
  )
})
