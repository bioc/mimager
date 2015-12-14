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


test_that("PM xy locations contain expected values", {
  affy.mat <- ma_layout(Dilution, transpose = FALSE, probes = "pm")
  index    <- unlist(affy::pmindex(Dilution), use.names = FALSE)
  probes   <- sample(index, 5)

  coords   <- affy::indices2xy(probes, abatch = Dilution)

  test.mat <- apply(affy.mat, 3, "[", coords)
  ref.mat  <- affy::exprs(Dilution)[probes,]

  expect_equivalent(test.mat, ref.mat)
})

test_that("MM xy locations contain expected values", {
  affy.mat <- ma_layout(Dilution, transpose = FALSE, probes = "mm")
  index    <- unlist(affy::mmindex(Dilution), use.names = FALSE)
  probes   <- sample(index, 5)

  coords   <- affy::indices2xy(probes, abatch = Dilution)

  test.mat <- apply(affy.mat, 3, "[", coords)
  ref.mat  <- affy::exprs(Dilution)[probes,]

  expect_equivalent(test.mat, ref.mat)
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



