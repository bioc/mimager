context("Affymetrix array layout")

data(Dilution, package = "affydata")
affy.mat <- ma_layout(Dilution)

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


DilutionPLM <- affyPLM::fitPLM(Dilution)
affy.plm <- ma_layout(DilutionPLM)

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
