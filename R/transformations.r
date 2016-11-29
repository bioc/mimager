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
