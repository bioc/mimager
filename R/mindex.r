# Microarray feature index
#
# Returns a data frame with the following columns:
#  - index: feature's position within the intensity matrix
#  - x/y: feature's x/y coordinates on the physical array
#  - type: feature type (e.g., pm, mm or bg)
#  - fset: featureset identifier
setMethod("mindex", c(object = "AffyBatch"),
  function(object, type = "pm") .affy_mindex(object, type))

setMethod("mindex", c(object = "PLMset"),
  function(object, type = "pm") .affy_mindex(object, type))

.affy_mindex <- function(object, probes) {
  cdf    <- affy::getCdfInfo(object)
  index  <- BiocGenerics::mget(featureNames(object), cdf, ifnotfound = NA)

  index <- data.frame(
    fset = rep(names(index), vapply(index, nrow, numeric(1))),
    do.call("rbind", index),
    row.names = NULL, stringsAsFactors = FALSE
  )

  index <- data.frame(
    fset = rep(index$fset, 2),
    utils::stack(index[setdiff(names(index), "fset")]),
    stringsAsFactors = FALSE
  )
  names(index) <- c("fset", "index", "type")
  index$type <- as.character(index$type)

  index <- cbind(index, affy::indices2xy(index$index, nc = object@ncol))
  index <- index[c("index", "x", "y", "fset", "type")]
  switch(probes,
    all = index,
    pm  = index[index$type == "pm", ],
    mm  = index[index$type == "mm", ]
  )
}


setMethod("mindex", c(object = "FeatureSet"),
  function(object, type = "pm") .featureset_mindex(object, type))

setMethod("mindex", c(object = "oligoPLM"),
  function(object, type = "pm") .featureset_mindex(object, type))

.featureset_mindex <- function(object, probes) {
  annot  <- check_annotation(oligo::annotation(object))
  status <- require(annot, character.only = TRUE, quietly = TRUE)
  if (!status)
    stop("Please install the ", annot, " package from Bioconductor.",
         call. = FALSE)
  dbcon <- oligo::db(get(annot))
  tbls  <- DBI::dbListTables(dbcon)

  bg.index <- mm.index <- pm.index <- NULL

  if (probes == "bg") {
    # consistent with oligo::bgindex assume all probe types != 1 are background
    sql <- paste("SELECT fid,x,y,featureSet.fsetid FROM",
                 "pmfeature, featureSet",
                 "WHERE pmfeature.fsetid=featureSet.fsetid",
                 "AND type > 1")
    bg.index <- DBI::dbGetQuery(dbcon, sql)
    bg.index$type <- "bg"
  }

  fields <- c("fid", "x", "y", "fsetid")
  if (probes %in% c("mm", "all") & "mmfeature" %in% tbls) {
    mm.index <- DBI::dbReadTable(dbcon, "mmfeature")[, fields]
    mm.index$type <- "mm"
  }
  if (probes %in% c("pm", "all")) {
    pm.index <- DBI::dbReadTable(dbcon, "pmfeature")[, fields]
    pm.index$type <- "pm"
  }

  out <- switch(probes,
    all = rbind(pm.index, mm.index),
    pm  = pm.index,
    mm  = mm.index,
    bg  = bg.index
  )
  out$fsetid <- as.character(out$fsetid)

  # account for 0-based indexing
  if (any(out[c("x", "y")] == 0))
    out[c("x", "y")] <- out[c("x", "y")] + 1

  stats::setNames(out, c("index", "x", "y", "fset", "type"))
}
