# Convert AffyBatch or PLMSet to an array of matrixes that correspond to the
# physical layout of their microarray features
#
# The reconstructed arrays are tranposed by default to orient the Affymetrix
# chips vertically, as is typically expected. Set to FALSE to return an array in
# the orientation specified by the coordinates.

setMethod("ma_layout", c(object = "AffyBatch"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE) {

    probes <- check_probes(probes)

    coords <- probe_index(object, probes)
    values <- ma_values(object, probes, select)

    # fill in missing rows if pm or mm rows were selected
    values <- values[match(coords[, "index"], rownames(values)),]

    to_array(values, nrow(object), ncol(object), coords[, c("x", "y")], transpose)
})


setMethod("ma_layout", c(object = "PLMset"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE,
           type = "resid") {

    probes <- check_probes(probes)

    coords <- probe_index(object, probes)
    values <- ma_values(object, probes, select, type)

    # fill in missing rows if pm or mm rows were selected
    values <- values[match(coords[, "index"], rownames(values)),]

    to_array(values, object@nrow, object@ncol, coords[, c("x", "y")], transpose)
})
