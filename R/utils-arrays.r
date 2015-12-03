# convert a matrix (x) with dimensions i,j to an array with dimensions nrow,
# ncol and j. That is, each column in x will become an element of the returned
# array. coords is a 2-dimensional matrix that provides the row and column
# locations of values from x in the returned array.

to_array <- function(x, nrow, ncol, coords = NULL, transpose = TRUE) {
  if (is.null(coords)) coords <- arrayInd(1:nrow(x), c(nrow, ncol))

  dim.names <- list(seq_len(nrow), seq_len(ncol), colnames(x))
  out <- array(dim = c(nrow, ncol, ncol(x)), dimnames = dim.names)

  ncells <- prod(nrow, ncol)

  mat.index    <- matrix(seq_len(ncells), nrow = nrow)
  coords.index <- mat.index[coords]
  coords.index <- matrix(coords.index, nrow = length(coords.index), ncol = ncol(x))

  z.adjust <- (ncells * (seq_len(ncol(x)) - 1))

  z.adjust <- matrix(z.adjust,
                     nrow = nrow(coords.index),
                     ncol = ncol(coords.index),
                     byrow = TRUE)

  coords.index <- coords.index + z.adjust
  coords.index <- as.numeric(coords.index)

  out[coords.index] <- as.numeric(x)
  if (transpose) out <- aperm(out, perm = c(2, 1, 3))
  out
}

