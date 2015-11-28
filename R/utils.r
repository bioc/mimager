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

# reduce nrow/ncol to prevent empty rows/columns in grid layout
# dims: c(nrow, ncol)
trim_dims <- function(n, nrow, ncol) {
  i <- seq_len(n)[1:prod(nrow, ncol)]
  m <- matrix(i, nrow, ncol, byrow = TRUE)

  nas <- apply(m, c(1, 2), function(x) sum(is.na(x)))
  m <- m[!rowSums(nas) == ncol(m), !colSums(nas) == nrow(m), drop = FALSE]
  dim(m)
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}