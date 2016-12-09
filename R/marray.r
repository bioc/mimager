#' Microarray array
#'
#' Convert S4 microarray data structures into a three-dimensional array of
#' matrices, where each matrix corresponds to an individual sample's microarray
#' with values arranged to reflect the physical position of the corresponding
#' feature (i.e., probe) on the microarray surface.
#'
#' @template probes
#' @param object a valid Bioconductor microarray data structure
#' @param probes probe type see section for more information
#' @param select a numeric, character or logical vector indicating samples to
#'   include
#' @param transpose \code{TRUE} (the default), ensures the reconstructed
#'   microarrays are vertically oriented, as is typically expected. Set to
#'   \code{FALSE} to return an array in the orientation strictly specified by
#'   the platform coordinates
#' @param type for objects containing probe-level models (e.g., \code{PLMsets}),
#'   specify either \code{"residuals"} (the default) or \code{"weights"}
#' @param ... additional arguments
#'
#' @examples
#' if (require(affydata, quietly = TRUE)) {
#'   data("Dilution", package = "affydata")
#'   dilution.array <- marray(Dilution, select = c("20A", "10A"))
#' }
#'
#' @name marray
NULL

#' @rdname marray
#' @export
setMethod("marray", c(object = "AffyBatch"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE) {

    if (is.null(select)) select <- sampleNames(object)

    probes <- check_probe(object, probes)
    index  <- mindex(object, probes)
    values <- Biobase::exprs(object)[index$index, select, drop = FALSE]

    to_array(values, nrow(object), ncol(object), index[c("x", "y")], transpose)
})


#' @rdname marray
#' @export
setMethod("marray", c(object = "PLMset"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE,
           type = "residuals") {

    if (is.null(select)) select <- sampleNames(object)

    values <- switch(type,
      residuals = object@residuals,
      weights   = object@weights
    )
    names(values) <- sub("([pm]m).*", "\\1", tolower(names(values)))

    # determine which probe types were included in plm
    values <- values[S4Vectors::elementNROWS(values) != 0]
    probes <- ifelse(length(values) == 1, names(values), "all")

    index  <- mindex(object, probes = probes)
    values <- do.call("rbind", values)

    to_array(values, object@nrow, object@ncol, index[c("x", "y")], transpose)
})


#' @rdname marray
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
    if (is.null(select)) select <- sampleNames(object)
    values <- Biobase::exprs(object)[index$index, select, drop = FALSE]

    to_array(values, dims[1], dims[2], index[c("x", "y")], transpose)
})


#' @rdname marray
#' @export
setMethod("marray", c(object = "oligoPLM"),
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE,
           type = "residuals") {

    if (is.null(select)) select <- sampleNames(object)

    type  <- match.arg(type, c("residuals", "weights"))
    index <- mindex(object, probes = "all")
    dims  <- oligo::geometry(object)

    values <- switch(type,
      residuals = oligo::residuals(object),
      weights   = oligo::weights(object)
    )

    # only include indexed probes
    # (this seems like a dangerous approach but oligo::fitProbeLevelModel
    #  *always* returns matrices with dimensions equal to the input)
    values <- values[index$index, ]
    dimnames(values) <- list(index$index, sampleNames(object))

    values <- values[, select, drop = FALSE]

    to_array(values, dims[1], dims[2], index[c("x", "y")], transpose)
})
