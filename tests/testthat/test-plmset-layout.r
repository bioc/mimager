context("PLMset array layout")

if (requireNamespace(c("affydata", "affyPLM"), quietly = TRUE)) {

  plm <- affyPLM::fitPLM(Dilution)
  affy.plm <- ma_layout(plm, transpose = TRUE)

  test_that("Affymetrix GeneChip PLM dimensions", {
    expect_identical(dimnames(affy.plm)[[3]], affy::sampleNames(plm))
    expect_identical(nrow(affy.plm), plm@nrow)
    expect_identical(ncol(affy.plm), plm@ncol)
  })

  test_that("Affymetrix GeneChip orientation", {
    # NAs represent missing control probes
    expect_true(all(is.na(affy.plm[11, , ])))
    expect_true(all(is.na(affy.plm[16, 1:182, ])))
    expect_true(all(is.na(affy.plm[1:8, 61:110, ])))
  })


  test_that("PM xy locations contain expected values", {
    affy.plm <- ma_layout(plm, transpose = FALSE, probes = "pm")
    index    <- unlist(affy::pmindex(Dilution), use.names = FALSE)
    probes   <- sample(index, 5)

    coords   <- affy::indices2xy(probes, nc = plm@ncol)

    test.mat <- apply(affy.plm, 3, "[", coords)
    ref.mat  <- affyPLM::resid(plm)$PM.resid[match(probes, index),]

    expect_equivalent(test.mat, ref.mat)
  })

  test_that("Warning is thrown if model didn't include MM probes", {
    expect_warning(ma_values(plm, probes = "mm"))
  })


  plm <- affyPLM::fitPLM(Dilution, MM ~ -1 + probes + samples)

  test_that("MM xy locations contain expected values", {
    affy.plm <- ma_layout(plm, transpose = FALSE, probes = "mm")
    index    <- unlist(affy::mmindex(Dilution), use.names = FALSE)
    probes   <- sample(index, 5)

    coords   <- affy::indices2xy(probes, nc = plm@ncol)

    test.mat <- apply(affy.plm, 3, "[", coords)
    ref.mat  <- affyPLM::resid(plm)$MM.resid[match(probes, index),]

    expect_equivalent(test.mat, ref.mat)
  })

  test_that("Warning is thrown if model didn't include PM probes", {
    expect_warning(ma_values(plm, probes = "pm"))
  })


  plm <- affyPLM::fitPLM(Dilution, MM ~ -1 + probes + samples)

  test_that("Combined PM/MM xy locations contain expected values", {
    plm      <- affyPLM::fitPLM(Dilution, PMMM ~ -1 + probes + samples)
    affy.plm <- ma_layout(plm, transpose = FALSE, probes = "all")
    index    <- unlist(affy::indexProbes(plm, "both"), use.names = FALSE)
    probes   <- sample(index, 5)

    coords   <- affy::indices2xy(probes, nc = plm@ncol)

    test.mat <- apply(affy.plm, 3, "[", coords)
    ref.mat  <- ma_values(plm, probes = "all")[as.character(probes),]

    expect_equivalent(test.mat, ref.mat)
  })
}
