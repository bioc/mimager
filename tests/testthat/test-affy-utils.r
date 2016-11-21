context("AffyBatch utilities")

if (requireNamespace("affydata", quietly = TRUE)) {

  samples <- affy::sampleNames(Dilution)

  test_that("select samples from AffyBatch", {
    x <- mvalues(Dilution, select = samples[1])
    expect_equal(dim(x), c(201800, 1))
    expect_equal(colnames(x), samples[1])

    x <- mvalues(Dilution, select = samples[c(1, 3)])
    expect_equal(dim(x), c(201800, 2))
    expect_equal(colnames(x), samples[c(1, 3)])

    x <- mvalues(Dilution, select = samples[c(1, 3)], probes = "all")
    expect_equal(dim(x), c(403600, 2))
    expect_equal(colnames(x), samples[c(1, 3)])
  })
}

