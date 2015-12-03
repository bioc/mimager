# convert a matrix (x) with dimensions i,j to an array with dimensions nrow,
# ncol and j. That is, each column in x will become an element of the returned
# array. coords is a 2-dimensional matrix that provides the row and column
# locations of values from x in the returned array.

to_array <- function(x, nrow, ncol, coords = NULL) {
  if (is.null(coords)) coords <- arrayInd(1:nrow(x), c(nrow, ncol))

  out <- lapply(1:ncol(x), function(i) {
    mat <- matrix(nrow = nrow, ncol = ncol)
    mat[coords] <- x[, i]
    t(mat)
  })

  out <- abind::abind(out, along = 3)
  dimnames(out) <- list(NULL, NULL, colnames(x))
  return(out)
}
