context("oligo featureset layout")

if (requireNamespace("oligoData", quietly = TRUE)) {
  if (suppressPackageStartupMessages(require("pd.hugene.1.0.st.v1", quietly = TRUE))) {
    data(affyGeneFS, package = "oligoData")
    obj <- affyGeneFS[, 1:4]
    ary <- marray(obj, transpose = FALSE)

    test_that("Affymetrix Gene ST dimensions", {
      expect_identical(dimnames(ary)[[3]], Biobase::sampleNames(obj))
      expect_identical(dim(ary)[1:2], oligo::geometry(obj))
    })

    test_that("Affymetrix Gene ST orientation", {
      # verify that position of NAs in the generated array match the locations of
      # positions missing from the index
      indx <- mindex(obj, probes = "all")
      mask <- Matrix::sparseMatrix(i = indx$x, j = indx$y, dims = oligo::geometry(obj))
      expect_equal(which(is.na(ary[,,1])), Matrix::which(!mask))
    })

    test_that("Single sample objects return an array", {
      obj <- obj[, 1]
      ary <- marray(obj)

      expect_is(ary, "array")
      expect_identical(dimnames(ary)[[3]], Biobase::sampleNames(obj))
      expect_identical(dim(ary)[1:2], oligo::geometry(obj))
    })
  }
}