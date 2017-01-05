
setMethod("featureNames", signature(object = "PLMset"),
  function(object) {
      cdf.envir <- affy::getCdfInfo(object)
      ls(envir = cdf.envir)
  })

setMethod("sampleNames", signature(object = "oligoPLM"),
  function(object) colnames(oligo::coef(object)))

# fill empty rows with values from adjacent rows
fill_rows <- function(x, empty.thresh) {
  ndims   <- length(dim(x))
  na.rows <- which(rowMeans(is.na(x)) >= empty.thresh, useNames = FALSE)

  if (length(na.rows) == 0) return(x)

  if (length(na.rows) / nrow(x) > 0.55) {
    warning("Too many empty rows to fill", call. = FALSE)
    return(x)
  }

  # if 1st row is empty fill-up instead of down
  offset <- ifelse(any(na.rows == 1), 1, -1)
  # can't fill last row if we're filling-up
  if (offset == 1 & max(na.rows) == nrow(x)) na.rows <- utils::head(na.rows, -1)

  if (ndims == 2) {
    x[na.rows, ] <- x[na.rows + offset, ]
  } else if (ndims == 3) {
    x[na.rows,, ] <- x[na.rows + offset,, ]
  } else {
    stop("x must be a 2d matrix or 3d array.", call. = FALSE)
  }
  return(x)
}
