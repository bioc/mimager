context("Affymetrix array layout")

data(Dilution, package = "affydata")
ref.mat  <- affy::exprs(Dilution)
pm.mat   <- affy::pm(Dilution)
affy.mat <- ma_layout(Dilution, transpose = TRUE)


test_that("Affymetrix GeneChip dimensions", {
  expect_identical(dimnames(affy.mat)[[3]], affy::sampleNames(Dilution))
  expect_identical(nrow(affy.mat), nrow(Dilution))
  expect_identical(ncol(affy.mat), ncol(Dilution))
})

test_that("Affymetrix GeneChip orientation", {
  # NAs represent missing control probes
  expect_true(all(is.na(affy.mat[11, , ])))
  expect_true(all(is.na(affy.mat[16, 1:182, ])))
  expect_true(all(is.na(affy.mat[1:8, 61:110, ])))
})


test_that("xy locations contain expected values", {
  affy.mat <- aperm(affy.mat, perm = c(2, 1, 3))

  index   <- as.numeric(rownames(pm.mat))
  index   <- cbind(i = index, affy::indices2xy(index, nc = ncol(affy.mat)))

  probes  <- sample(seq_len(nrow(index)), 5)
  ref     <- ref.mat[index[probes, "i"], ]

  expect_equal(ref[1,],
               affy.mat[index[probes[1], "x"], index[probes[1], "y"],])
  expect_equal(ref[2,],
               affy.mat[index[probes[2], "x"], index[probes[2], "y"],])
  expect_equal(ref[3,],
               affy.mat[index[probes[3], "x"], index[probes[3], "y"],])
  expect_equal(ref[4,],
               affy.mat[index[probes[4], "x"], index[probes[4], "y"],])
  expect_equal(ref[5,],
               affy.mat[index[probes[5], "x"], index[probes[5], "y"],])
})

DilutionPLM <- affyPLM::fitPLM(Dilution)
affy.plm <- ma_layout(DilutionPLM, transpose = TRUE)

test_that("Affymetrix GeneChip PLM dimensions", {
  expect_identical(dimnames(affy.plm)[[3]], affy::sampleNames(DilutionPLM))
  expect_identical(nrow(affy.plm), DilutionPLM@nrow)
  expect_identical(ncol(affy.plm), DilutionPLM@ncol)
})

test_that("Affymetrix GeneChip orientation", {
  # NAs represent missing control probes
  expect_true(all(is.na(affy.plm[11, , ])))
  expect_true(all(is.na(affy.plm[16, 1:182, ])))
  expect_true(all(is.na(affy.plm[1:8, 61:110, ])))
})

test_that("Empty cells are filled with nullGrobs", {
  expect_silent(out <- ma_image(Dilution[, 1:3], ncol = 2))
  expect_equal(class(out[4,4]$grobs[[1]]), c("null", "grob", "gDesc"))
})

test_that("Empty columns are removed from layout", {
  out <- ma_image(Dilution[, 1:2], ncol = 3)
  expect_equal(ncol(out), 7)
})

test_that("Empty rows are removed from layout", {
  out <- ma_image(Dilution[, 1:2], nrow = 1)
  expect_equal(nrow(out), 4)
})



