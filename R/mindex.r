# Microarray feature index
#
# Returns a data frame with the following columns:
#  - index: feature's position within the intensity matrix
#  - x/y: feature's x/y coordinates on the physical array
#  - type: feature type (e.g., pm, mm or bg)
#  - fset: featureset identifier
setMethod("mindex", c(object = "AffyBatch"),
  function(object, probes = NULL) .affy_mindex(object, probes))

setMethod("mindex", c(object = "PLMset"),
  function(object, probes = NULL) .affy_mindex(object, probes))

.affy_mindex <- function(object, probes) {
  probes <- check_probe(object, probes)
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
  function(object, probes = NULL) {
    probes <- check_probe(object, probes)
    .featureset_mindex(object, probes)
})

setMethod("mindex", c(object = "oligoPLM"),
  function(object, probes = NULL) .featureset_mindex(object, probes))

.featureset_mindex <- function(object, probes) {
  annot  <- check_annotation(oligo::annotation(object))
  status <- require(annot, character.only = TRUE, quietly = TRUE)
  if (!status)
    stop("Please install the ", annot, " package from BioConductor.", call. = FALSE)
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
  if (probes %in% c("mm", "all") & "mmfeature" %in% tbls) {
    mm.index <- DBI::dbReadTable(dbcon, "mmfeature")[, c("fid", "x", "y", "fsetid")]
    mm.index$type <- "mm"
  }
  if (probes %in% c("pm", "all")) {
    pm.index <- DBI::dbReadTable(dbcon, "pmfeature")[, c("fid", "x", "y", "fsetid")]
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
  if (class(object) %in% "ExonFeatureSet")
    out[c("x", "y")] <- out[c("x", "y")] + 1

  stats::setNames(out, c("index", "x", "y", "fset", "type"))
}
