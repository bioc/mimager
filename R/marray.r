#' Microarray array
#'
#' Convert S4 microarray data structures into a three-dimensional array of
#' matrices, where each matrix corresponds to an individual sample's microarray
#' with values arranged to reflect the physical position of the corresponding
#' feature (i.e., probe) on the microarray surface.
#'
#' @template probes
#' @param object a valid Bioconductor microarray data structure
#' @param type for microarray objects \code{type} refers to \emph{probe type};
#'   for objects containing probe-level models (e.g., \code{PLMsets})
#'   \code{type} refers to the \emph{value type} (i.e, \code{"residuals"} or
#'   \code{"weights"}). See probe type section for more information.
#' @param select a numeric, character or logical vector indicating samples to
#'   include
#' @param transpose \code{TRUE} (the default), ensures the reconstructed
#'   microarrays are vertically oriented, as is typically expected. Set to
#'   \code{FALSE} to return an array in the orientation strictly specified by
#'   the platform coordinates
#' @param ... additional arguments
#'
#' @examples
#' if (require(affydata, quietly = TRUE)) {
#'   data("Dilution", package = "affydata")
#'   dilution.array <- marray(Dilution, select = c("20A", "10A"))
#' }
#'
#' @return three-dimensional \code{\link{array}}
#' @name marray
NULL

#' @rdname marray
#' @export
setMethod("marray", c(object = "AffyBatch"),
  function(object,
           type = "pm",
           select = NULL,
           transpose = FALSE) {

    if (is.null(select)) select <- sampleNames(object)

    type <- check_type(object, type)
    index  <- mindex(object, type)
    values <- Biobase::exprs(object)[index$index, select, drop = FALSE]

    to_array(values, nrow(object), ncol(object), index[c("x", "y")], transpose)
})


#' @rdname marray
#' @export
setMethod("marray", c(object = "PLMset"),
  function(object,
           type = "residuals",
           select = NULL,
           transpose = FALSE) {

    if (is.null(select)) select <- sampleNames(object)

    values <- switch(type,
      residuals = object@residuals,
      weights   = object@weights
    )
    names(values) <- sub("([pm]m).*", "\\1", tolower(names(values)))

    # determine which probe types were included in plm
    values <- values[S4Vectors::elementNROWS(values) != 0]
    probes <- ifelse(length(values) == 1, names(values), "all")

    index  <- mindex(object, type = probes)
    values <- do.call("rbind", values)

    to_array(values, object@nrow, object@ncol, index[c("x", "y")], transpose)
})


#' @rdname marray
#' @export
setMethod("marray", c(object = "FeatureSet"),
  function(object,
           type = "pm",
           select = NULL,
           transpose = FALSE) {

    type <- check_type(object, type)
    index  <- mindex(object, type)
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
           type = "residuals",
           select = NULL,
           transpose = FALSE) {

    if (is.null(select)) select <- sampleNames(object)

    type  <- check_type(object, type)
    index <- mindex(object, type = "all")
    dims  <- oligo::geometry(object)

    values <- switch(type,
      residuals = oligo::residuals(object),
      weights   = oligo::weights(object)
    )

    # only include indexed probes
    # (this seems like a dangerous approach but oligo::fitProbeLevelModel
    #  *always* returns matrices with dimensions equal to the input)
    values <- values[index$index, , drop = FALSE]
    dimnames(values) <- list(index$index, sampleNames(object))

    values <- values[, select, drop = FALSE]

    to_array(values, dims[1], dims[2], index[c("x", "y")], transpose)
})
