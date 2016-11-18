
setMethod("featureNames", signature(object = "PLMset"),
  function(object) {
      cdf.envir <- affy::getCdfInfo(object)
      ls(envir = cdf.envir)
  })

# Return the index and xy-coords for Affymetrix probes
#
# If probes=pm or probes=mm then the index is repeated for the specified
# probetype and associated with the other probetype's xy coordinates. This is to
# avoid empty rows that result from plotting a single probetype
probe_index <- function(object, probes) {

  cdf <- affy::getCdfInfo(object)
  index <- BiocGenerics::mget(featureNames(object), cdf)

  index <- data.frame(
    feature = rep(names(index), S4Vectors::elementLengths(index)),
    do.call("rbind", index),
    row.names = NULL, stringsAsFactors = FALSE
  )

  if (probes == "all") {
    out <- unlist(index[c("pm", "mm")], use.names = FALSE)
    out <- cbind(index = out, affy::indices2xy(out, nc = object@ncol))
  } else {
    coords <- lapply(index[c("pm", "mm")], affy::indices2xy, nc = object@ncol)

    probes <- switch(probes,
       all = c("pm", "mm"),
       pm  = rep("pm", 2),
       mm  = rep("mm", 2))

    out <- unlist(index[probes], use.names = FALSE)
    out <- cbind(index = out, do.call("rbind", coords))
  }

  return(out)
}


# fill in empty rows if not plotting both probe types
fill_affy_ivt <- function(x, probes) {
  last.row <- max(which(!is.na(x[, 1, 1])))
  na.rows <- which(apply(is.na(x[, , 1]), 1, all))
  if (probes == "mm") na.rows <- na.rows[na.rows < last.row]
  offset  <- switch(probes, pm = -1, mm = 1)
  x[na.rows, , ] <- x[na.rows + offset, , ]
  return(x)
}
