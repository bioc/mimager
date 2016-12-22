#' Array rank
#'
#' Determines the rank of values within each matrix of a three-dimensional
#' array.
#'
#' @param x a three-dimensional \code{\link{array}} of matrices
#' @inheritParams base::rank
#'
#' @examples
#' # microarray visualization
#' if (require(affydata, quietly = TRUE)) {
#'   data("Dilution", package = "affydata")
#'   x <- arank(marray(Dilution, transpose = TRUE))
#' }
#'
#' @return an \code{\link{array}} with the same dimensions as \code{x}
#' @family array transformations
#' @seealso \code{\link[base]{rank}}
#' @export

arank <- function(x, na.last = TRUE, ties.method = "first") {
  ties.method <-
    match.arg(ties.method,
              c("average", "first", "last", "random", "max", "min"))

  coords <- which(!is.na(x), arr.ind = TRUE)

  for (i in 1:dim(x)[3]) {
    icoords <- coords[coords[, "dim3"] == i,]
    x[icoords] <- rank(x[icoords], ties.method = "first")
  }
  return(x)
}


#' Array relative log expression
#'
#' The relative log expression (RLE) quantifies the extent to which each sample
#' in a dataset differs from a "reference" sample, which represents each probe's
#' median value across all samples.
#'
#' @inheritParams arank
#' @param log2 should values be $log_2$ transformed
#' @param normalize should values be quantile normalized
#'
#' @examples
#' # microarray visualization
#' if (require(affydata, quietly = TRUE)) {
#'   data("Dilution", package = "affydata")
#'   x <- arle(marray(Dilution, transpose = TRUE))
#' }
#'
#' @return an \code{\link{array}} with the same dimensions as \code{x}
#' @family array transformations
#' @seealso \code{\link[affyPLM]{RLE}}
#' @export
arle <- function(x, log2 = TRUE, normalize = TRUE) {
  if (log2) x <- log2(x)

  coords <- which(!is.na(x), arr.ind = TRUE)
  values <- matrix(x[coords], nrow = nrow(coords) / dim(x)[3])

  if (normalize) {
    values <- preprocessCore::normalize.quantiles(values)
  }

  values <- sweep(values, MARGIN = 1, Biobase::rowMedians(values), "-")
  x[coords] <- as.numeric(values)
  return(x)
}
