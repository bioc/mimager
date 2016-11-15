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


setMethod("ma_layout", c(object = "FeatureSet"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE) {

    probes <- check_probes(probes)

    coords <- probe_index_oligo(object, probes)

    # much more efficient to return all values simultaneously with exprs() than
    # use pm/mm accessors
    if (is.null(select)) select <- Biobase::sampleNames(object)
    values <- Biobase::exprs(object)[coords$index, select, drop = FALSE]

    # adjust for 0-based indexing
    coords[, c('x', 'y')] <- coords[, c('x', 'y')] + 1
    dims <- oligo::geometry(object) + 1

    to_array(values, dims[1], dims[2], coords[, c("x", "y")], transpose)
})
