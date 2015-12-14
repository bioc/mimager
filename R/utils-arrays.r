# convert a matrix (x) with dimensions i,j to an array with dimensions nrow,
# ncol and j. That is, each column in x will become an element of the returned
# array. coords is a 2-dimensional matrix that provides the row and column
# locations of values from x in the returned array.

to_array <- function(x, nrow, ncol, coords = NULL, transpose = TRUE, index) {
  if (is.null(coords)) coords <- arrayInd(1:nrow(x), c(nrow, ncol))

  dim.names <- list(seq_len(nrow), seq_len(ncol), colnames(x))
  out <- array(dim = c(nrow, ncol, ncol(x)), dimnames = dim.names)

  # location of value in the array
  coords.index <- xy2index(coords[, 1], coords[, 2], nrow, ncol)

  # scale matrix coordinates to multi-dimensional array
  ncells <- prod(nrow, ncol)
  offset <- ncells * seq_len(ncol(x)) - ncells
  offset <- rep(offset, each = length(coords.index))
  coords.index <- rep(coords.index, ncol(x)) + offset

  out[coords.index] <- as.numeric(x)
  if (transpose) out <- aperm(out, perm = c(2, 1, 3))
  out
}


# convert x,y coordinates to linear index
# byrow: Default is FALSE (column-major order)
xy2index <- function(x, y, nrow, ncol, byrow = FALSE) {
  x <- x - 1
  y <- y - 1
  switch(byrow + 1,
    (nrow * y + x) + 1,
    (ncol * x + y) + 1)
}

