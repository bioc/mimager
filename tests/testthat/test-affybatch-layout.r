if (requireNamespace("affydata", quietly = TRUE)) {
  if (requireNamespace("pd.hugene.1.0.st.v1", quietly = TRUE)) {
    context("AffyBatch layout")

    data(Dilution, package = "affydata")
    suppressWarnings(library(hgu95av2cdf))
    obj <- Dilution[, 1:4]
    ary <- marray(obj, transpose = FALSE, probes = "all")

    test_that("array dimensions", {
      expect_identical(dimnames(ary)[[3]], Biobase::sampleNames(obj))
      expect_identical(dim(ary)[1:2], dim(obj))
    })

    test_that("array orientation", {
      indx <- mindex(obj, probes = "all")
      mask <- Matrix::sparseMatrix(i = indx$x, j = indx$y, dims = dim(obj))
      expect_equal(which(is.na(ary[,,1])), Matrix::which(!mask))
    })

    test_that("Single sample objects return an array", {
      obj <- obj[, 1]
      ary <- marray(obj)

      expect_is(ary, "array")
      expect_identical(dimnames(ary)[[3]], Biobase::sampleNames(obj))
      expect_identical(nrow(ary), nrow(obj))
      expect_identical(ncol(ary), ncol(obj))
    })


    context("PLMset layout")

    plm <- affyPLM::fitPLM(obj)
    ary <- marray(plm, type = "residuals")

    test_that("array dimensions", {
      expect_identical(dimnames(ary)[[3]], Biobase::sampleNames(obj))
      expect_identical(dim(ary)[1:2], dim(obj))
    })

    test_that("array orientation", {
      indx <- mindex(obj, probes = "pm")
      mask <- Matrix::sparseMatrix(i = indx$x, j = indx$y, dims = dim(obj))
      expect_equal(which(is.na(ary[,,1])), Matrix::which(!mask))
    })
  }
}
