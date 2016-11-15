
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

  if (probes == "both") {
    out <- unlist(index[c("pm", "mm")], use.names = FALSE)
    out <- cbind(index = out, affy::indices2xy(out, nc = object@ncol))
  } else {
    coords <- lapply(index[c("pm", "mm")], affy::indices2xy, nc = object@ncol)

    probes <- switch(probes,
       both = c("pm", "mm"),
       pm = rep("pm", 2),
       mm = rep("mm", 2))

    out <- unlist(index[probes], use.names = FALSE)
    out <- cbind(index = out, do.call("rbind", coords))
  }

  return(out)
}

# Affymetrix FeatureSet objects handled by oligo package
probe_index_oligo <- function(object, probes) {

  warning('Currently both PM and MM probes are always retrieved for ',
          class(object), ' objects.', call. = FALSE)

  dbcon <- oligo::db(object)
  DBI::dbListTables(dbcon)
  dbtbls <- dbGetQuery(dbcon, "SELECT tbl FROM table_info WHERE tbl LIKE '%feature' AND row_count > 0")[[1]]

  index <- lapply(dbtbls, function(tbl) dbGetQuery(dbcon, paste("SELECT x, y, fid FROM", tbl)))
  index <- do.call("rbind", index)
  index <- index[order(index$fid), c('fid', 'x', 'y')]
  names(index) <- c('index', 'x', 'y')

  return(index)
}
