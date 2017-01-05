context("Grid layout")

x <- array(runif(100), c(5, 5, 4))

test_that("Empty cells are filled with nullGrobs", {
  expect_silent(out <- mimage(x[,, 1:3], ncol = 2))
  expect_equal(class(out[4, 4]$grobs[[1]]), c("null", "grob", "gDesc"))
})

test_that("Empty columns are removed from layout", {
  out <- mimage(x[,, 1:2], ncol = 3)
  expect_equal(ncol(out), 7)
})

test_that("Empty rows are removed from layout", {
  out <- mimage(x[,, 1:2], nrow = 1)
  expect_equal(nrow(out), 4)
})
