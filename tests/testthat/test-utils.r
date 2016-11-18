context("Utilities")

test_that("Empty rows/columns are dropped", {
  expect_equal(trim_dims(1, nrow = 2, ncol = 1), c(1, 1))
  expect_equal(trim_dims(1, nrow = 1, ncol = 2), c(1, 1))
  expect_equal(trim_dims(2, nrow = 3, ncol = 1), c(2, 1))
  expect_equal(trim_dims(2, nrow = 1, ncol = 3), c(1, 2))
})


test_that("Matrix to array conversion", {
  m <- matrix(1:27, ncol = 3)
  a <- array(1:27, c(3, 3, 3))
  a <- aperm(a, c(2, 1, 3))

  expect_equivalent(to_array(m, 3, 3), a)
})

test_that("Array is recreated from matrix and coordinates", {
  n        <- 4
  x.idx    <- 1:24
  x.dim    <- c(2, 3)
  x.array  <- array(x.idx, c(x.dim, n))

  x.coords <- arrayInd(x.idx, dim(x.array))
  x.coords <- x.coords[x.coords[,3] == 1, 1:2] # keep coords for 1st array element
  x.coords <- setNames(data.frame(x.coords), c("x", "y"))

  x.mat <- matrix(x.idx, ncol = n)

  # recreate original array from matrix
  x.array2 <- to_array(x.mat, x.dim[1], x.dim[2], x.coords, transpose = FALSE)
  expect_equivalent(x.array, x.array2)

  # return transpose of original array from matrix
  x.array3 <- to_array(x.mat, x.dim[1], x.dim[2], x.coords, transpose = TRUE)
  expect_equivalent(x.array3, aperm(x.array, c(2, 1, 3)))
})
