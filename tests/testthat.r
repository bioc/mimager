library(testthat)
library(arraygrid)

if (requireNamespace("affydata"))
  test_file("tests/testthat/test-affy-layout.r")

