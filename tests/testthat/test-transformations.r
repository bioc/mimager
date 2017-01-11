context("Transformations")

set.seed(1:3)
ary <- array(runif(100), dim = c(5, 5, 4))
out <- array(dim = dim(ary))

test_that("arank properly ranks arrays", {
  for (i in 1:4) out[,, i] <- rank(ary[,, i])
  expect_equal(arank(ary), out)
})

test_that("arle properly computes relative-log expression", {
  ary <- log2(ary)
  mdns <- apply(ary, 1:2, median)
  for (i in 1:4) out[,, i] <- ary[,, i] - mdns
  expect_equal(arle(ary, log2 = FALSE, normalize = FALSE), out)
})
