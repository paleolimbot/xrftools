context("test-data.R")

test_that("read_example filters and slices its output", {
  expect_equal(nrow(read_xrf_example(.which = 1:4)), 4)
  expect_true(all(read_xrf_example(SampleIdent == "oreas 22d")$SampleIdent == "oreas 22d"))
})
