setGeneric("mindex",
  function(object,
           type = NULL) standardGeneric("mindex"))

#' @rdname marray
#' @export
setGeneric("marray",
  function(object,
           type = NULL,
           select = NULL,
           transpose = NULL) standardGeneric("marray"))

#' @rdname mimage
#' @export
setGeneric("mimage",
  function(object,
           type = NULL,
           select = NULL,
           colors = NULL,
           legend.label = NULL,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           empty.rows  = "fill",
           empty.thresh = 0.6,
           transform = NULL,
           trim = 0.01,
           fontsize = 12) {
    empty.rows <- match.arg(empty.rows, c("fill", "drop", "ignore"))
    stopifnot(empty.thresh > 0 & empty.thresh <= 1)
    standardGeneric("mimage")
})
