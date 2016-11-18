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

    if (is.null(select)) select <- Biobase::sampleNames(object)

    probes <- check_probe(object, probes)
    index  <- mindex(object, probes)
    values <- Biobase::exprs(object)[index$index, select]

    to_array(values, nrow(object), ncol(object), index[c("x", "y")], transpose)
})


setMethod("ma_layout", c(object = "PLMset"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE,
           type = "resid") {

    probes <- check_probe(object,probes)
    index  <- mindex(object, probes)
    values <- ma_values(object, probes, select, type)

    to_array(values, object@nrow, object@ncol, index[c("x", "y")], transpose)
})


setMethod("ma_layout", c(object = "FeatureSet"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE) {

    probes <- check_probe(object, probes)
    index  <- mindex(object, probes)
    dims   <- oligo::geometry(object)

    # much more efficient to return all values simultaneously with exprs() than
    # use pm/mm accessors
    if (is.null(select)) select <- Biobase::sampleNames(object)
    values <- Biobase::exprs(object)[index$index, select, drop = FALSE]

    to_array(values, dims[1], dims[2], index[c("x", "y")], transpose)
})


setMethod("ma_layout", c(object = "oligoPLM"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE,
           type = "residuals") {

    type  <- match.arg(type, c("residuals", "weights"))
    index <- mindex(object, probes = "all")
    dims  <- oligo::geometry(object)

    values <- switch(type,
      residuals = oligo::residuals(object),
      weights   = oligo::weights(object)
    )

    # only include annotated probes
    values <- values[index$index,]
    dimnames(values) <- list(index$index, colnames(object@chip.coefs))
    if (!is.null(select)) values <- values[, select, drop = FALSE]

    to_array(values, dims[1], dims[2], index[c("x", "y")], transpose)
})
