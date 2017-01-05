context("Utilities")

test_that("Empty rows/columns are dropped from grid of images", {
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
  x.coords <- x.coords[x.coords[, 3] == 1, 1:2]
  x.coords <- setNames(data.frame(x.coords), c("x", "y"))

  x.mat <- matrix(x.idx, ncol = n)

  # recreate original array from matrix
  x.array2 <- to_array(x.mat, x.dim[1], x.dim[2], x.coords, transpose = FALSE)
  expect_equivalent(x.array, x.array2)

  # return transpose of original array from matrix
  x.array3 <- to_array(x.mat, x.dim[1], x.dim[2], x.coords, transpose = TRUE)
  expect_equivalent(x.array3, aperm(x.array, c(2, 1, 3)))
})


test_that("Empty rows are filled with values from adjacent rows", {
  x <- matrix(c(1:5,
                c(1, rep(NA, 4)),
                6:10), ncol = 5, byrow = TRUE)
  x2 <- abind::abind(x, x, along = 3)
  x2[,, 2] <- x2[,, 2] + 1

  # missings must exceed threshold
  out <- fill_rows(x, empty.thresh = 1)
  expect_identical(c(1, rep(NA, 4)), out[2, ])

  # missing row is filled
  out <- fill_rows(x, 0.8)
  expect_identical(out[1, ], out[2, ])

  # extends to arrays
  out <- fill_rows(x2, 0.8)
  expect_identical(out[1,, 1], out[2,, 1])
  expect_identical(out[1,, 2], out[2,, 2])

  # fill-up if first row is empty
  x <- rbind(NA, x)
  out <- fill_rows(x, 0.8)
  expect_identical(out[c(1, 3), ], x[c(2, 4), ])

  # warn if too many rows are empty
  expect_warning(
    fill_rows(rbind(x, matrix(NA, ncol = ncol(x), nrow = 2)), 0.8)
  )
})


test_that("trim_values winsorizes to a specified percentile", {
  x <- c(-1, 1:3, 10)
  expect_error(trim_values(x, -1))
  expect_error(trim_values(x, 2))
  expect_equal(trim_values(x, 0.05), c(-0.6, 1, 2, 3, 8.6))
})

test_that("trim_values squishes values into a specified range", {
  x <- c(-1, 1:3, 10)
  expect_error(trim_values(x, c(1, 3, 4)))
  expect_equal(trim_values(x, c(1, 3)), c(1, 1, 2, 3, 3))
})
