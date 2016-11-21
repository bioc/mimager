#' Microarray array
#'
#' Convert S4 microarray data structures into a three-dimensional array of
#' matrices, where each matrix corresponds to an individual sample's microarray
#' with values arranged to reflect the physical position of the corresponding
#' feature (i.e., probe) on the microarray surface.
#'
#' @param object \code{AffyBatch} or \code{PLMset} object
#' @param probes probe type see section for more information
#' @param select a numeric, character or logical vector indicating samples to
#'   include
#' @param transpose \code{TRUE} (the default), ensures the reconstructed
#'   microarrays are vertically oriented, as is typically expected. Set to
#'   \code{FALSE} to return an array in the orientation strictly specified by
#'   the coordinates

#' @name marray
#' @export
setMethod("marray", c(object = "AffyBatch"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE) {

    if (is.null(select)) select <- Biobase::sampleNames(object)

    probes <- check_probe(object, probes)
    index  <- mindex(object, probes)
    values <- Biobase::exprs(object)[index$index, select, drop = FALSE]

    to_array(values, nrow(object), ncol(object), index[c("x", "y")], transpose)
})


#' @name marray
#' @export
setMethod("marray", c(object = "PLMset"),
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


#' @name marray
#' @export
setMethod("marray", c(object = "FeatureSet"),
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


#' @name marray
#' @export
setMethod("marray", c(object = "oligoPLM"),
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
