context("test-element_list.R")

test_that("getting and setting element lists works", {
  expect_identical(xrf_element_list("my_precious", default = character(0)), character(0))
  xrf_set_element_list("my_precious", c("Ag", "Au", "Pt"))
  expect_identical(xrf_element_list("my_precious"), c("Ag", "Au", "Pt"))
  xrf_set_element_list("my_precious", NULL)
  expect_identical(xrf_element_list("my_precious", default = NULL), character(0))
})

test_that("non-elements cannot be set as part of an element list", {
  expect_error(xrf_set_element_list("new", c("not an element")), "is not TRUE")
  expect_error(xrf_set_element_list("new", c(NA)), "is not TRUE")
})

test_that("element names cannot be used as element list names", {
  expect_error(xrf_set_element_list("Pb", c("Pb", "Zn")), "is not TRUE")
})

test_that("Unknown element lists throw errors", {
  expect_error(xrf_element_list("not a list"), "not found")
})

test_that("xrf_element_list can accept multiple length input", {
  xrf_set_element_list("my_precious", c("Ag", "Au", "Pt"))
  expect_identical(xrf_element_list(character(0)), character(0))
  expect_identical(xrf_element_list("my_precious"), c("Ag", "Au", "Pt"))
  expect_identical(xrf_element_list(c("my_precious", "Pb")), c("Ag", "Au", "Pt", "Pb"))
  xrf_set_element_list("my_precious", NULL)
})

test_that("built-in element lists are all elements", {
  expect_true(
    all(xrf_element_list("major") %in% all_elements)
  )
  expect_true(
    all(xrf_element_list("lake_sediment") %in% all_elements)
  )
  expect_true(
    all(xrf_element_list("everything") %in% all_elements)
  )
})
