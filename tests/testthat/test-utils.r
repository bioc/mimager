context("Utilities")

test_that("Empty rows/columns are dropped", {
  expect_equal(trim_dims(1, nrow = 2, ncol = 1), c(1, 1))
  expect_equal(trim_dims(1, nrow = 1, ncol = 2), c(1, 1))
  expect_equal(trim_dims(2, nrow = 3, ncol = 1), c(2, 1))
  expect_equal(trim_dims(2, nrow = 1, ncol = 3), c(1, 2))
})