context("Affymetrix array layout")

if (requireNamespace("affydata", quietly = TRUE)) {

  data(Dilution, package = "affydata")
  affy.mat <- marray(Dilution, transpose = TRUE)

  test_that("Affymetrix GeneChip dimensions", {
    expect_identical(dimnames(affy.mat)[[3]], Biobase::sampleNames(Dilution))
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
    affy.mat <- marray(Dilution, transpose = FALSE, probes = "pm")
    index    <- unlist(affy::pmindex(Dilution), use.names = FALSE)
    probes   <- sample(index, 5)

    coords   <- affy::indices2xy(probes, abatch = Dilution)

    test.mat <- apply(affy.mat, 3, "[", coords)
    ref.mat  <- affy::exprs(Dilution)[probes,]

    expect_equivalent(test.mat, ref.mat)
  })

  test_that("MM xy locations contain expected values", {
    affy.mat <- marray(Dilution, transpose = FALSE, probes = "mm")
    index    <- unlist(affy::mmindex(Dilution), use.names = FALSE)
    probes   <- sample(index, 5)

    coords   <- affy::indices2xy(probes, abatch = Dilution)

    test.mat <- apply(affy.mat, 3, "[", coords)
    ref.mat  <- affy::exprs(Dilution)[probes,]

    expect_equivalent(test.mat, ref.mat)
  })

  test_that("Combiend PM/MM xy locations contain expected values", {
    affy.mat <- marray(Dilution, transpose = FALSE, probes = "all")
    index    <- unlist(affy::indexProbes(Dilution, which = "both"), use.names = FALSE)
    probes   <- sample(index, 5)

    coords   <- affy::indices2xy(probes, abatch = Dilution)

    test.mat <- apply(affy.mat, 3, "[", coords)
    ref.mat  <- affy::exprs(Dilution)[probes,]

    expect_equivalent(test.mat, ref.mat)
  })
}