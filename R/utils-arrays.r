# convert a matrix (x) with dimensions i,j to an array with dimensions nrow,
# ncol and j. That is, each column in x will become an element of the returned
# array. coords is a 2-dimensional matrix that provides the row and column
# locations of values from x in the returned array.

to_array <- function(x, nrow, ncol, coords = NULL, transpose = TRUE) {
  if (is.null(coords)) coords <- arrayInd(1:nrow(x), c(nrow, ncol))
  stopifnot(nrow(coords) == nrow(x))

  dim.names <- list(seq_len(nrow), seq_len(ncol), colnames(x))
  out <- array(dim = c(nrow, ncol, ncol(x)), dimnames = dim.names)

  coords.index <-
    cbind(
      dim1 = coords$x,
      dim2 = coords$y,
      dim3 = rep(seq_len(ncol(x)), each = nrow(coords))
    )
  out[coords.index] <- as.numeric(x)
  if (transpose) out <- aperm(out, perm = c(2, 1, 3))
  out
}