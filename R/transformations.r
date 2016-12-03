# Per-array rank transformation
#
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


arle <- function(x, log2 = TRUE, normalize = TRUE, trim = 0.02) {
  if (log2) x <- log2(x)
  trim <- check_trim(trim)

  coords <- which(!is.na(x), arr.ind = TRUE)
  values <- matrix(x[coords], nrow = nrow(coords) / dim(x)[3])

  if (normalize) {
    values <- preprocessCore::normalize.quantiles(values)
  }

  values <- sweep(values, MARGIN = 1, Biobase::rowMedians(values), "-")
  trim.range <- c(0 + trim * 0.5, 1 - trim * 0.5)
  values <- scales::squish(values, quantile(values, trim.range))

  x[coords] <- as.numeric(values)
  return(x)
}
