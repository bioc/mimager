# Convert AffyBatch or PLMSet to an array of matrixes that correspond to the
# physical layout of their microarray features
#
# The reconstructed arrays are tranposed by default to orient the Affymetrix
# chips vertically, as is typically expected. Set to FALSE to return an array in
# the orientation specified by the coordinates.

setMethod("ma_layout", c(object = "AffyBatch"),
  function(object, probes = NULL, transpose = FALSE) {
    probes <- check_probes(probes)

    coords <- probe_index(object, probes)
    values <- ma_values(object, probes)

    to_array2(values, nrow(object), ncol(object), coords[, c("x", "y")], transpose)
})


# THIS ISN'T WORKING
setMethod("ma_layout", c(object = "PLMset"),
  function(object, probes = NULL, transpose = FALSE) {
    probes <- check_probes(probes)

    coords <- probe_index(object, probes)
    values <- ma_values(object, probes)

    to_array2(values, object@nrow, object@ncol, coords[, c("x", "y")], transpose)
  })


# temporary fix until I can get indexing in to_array() working properly
to_array2 <- function(object, nrow, ncol, coords, transpose = FALSE) {
  x <- apply(object, 2, function(x) Matrix::sparseMatrix(
        x = x, i = coords[,1], j = coords[,2], dims = c(nrow, ncol)))

  x <- lapply(x, as.matrix)
  x <- abind::abind(x, along = 3)
  x <- replace(x, x == 0, NA)
  if (transpose) x <- aperm(x, perm = c(2, 1, 3))
  x
}
