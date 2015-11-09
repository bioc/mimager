# based on ggplot2:::wrap_dims() by @hadleywickham
layout_dims <- function(n, nrow = NULL, ncol = NULL) {
  if (is.null(ncol) && is.null(nrow)) {
    rc <- grDevices::n2mfrow(n)
    nrow <- rc[2]
    ncol <- rc[1]
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  stopifnot(nrow * ncol >= n)

  c(nrow, ncol)
}