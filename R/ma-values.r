# Retrieve values from eSets
#
# Always returns a matrix where rownames contain the locations of probes in the intensity matrix
#
# @param select variables to keep

setMethod("ma_values", c(object = "AffyBatch"),
  function(object,
           probes = NULL,
           select = NULL) {

    probes <- check_probe(object, probes)
    if (!is.null(select)) object <- object[, select]

   switch(probes,
       all = rbind(affy::pm(object), affy::mm(object)),
        pm = affy::pm(object),
        mm = affy::mm(object)
    )
})


# Access PLMset values
setMethod("ma_values", c(object = "PLMset"),
  function(object,
           probes = NULL,
           select = NULL,
           type = "resid") {

    probes <- check_probes(probes)
    type <- match.arg(type, "resid")

    values <- index <- NULL

    if (type == "resid") {
      resids <- slot(object, "residuals")
      n <- S4Vectors::elementLengths(resids)

      # TODO: dry this out
      if (probes %in% c("pm", "both")) {
        if (n["PM.resid"] == 0) {
          warning("PLM model did not include PM probes.", call. = FALSE)
        } else {
          index <- unlist(affy::indexProbes(object, "pm"), use.names = FALSE)
          values <- resids$PM.resid
        }
      }

      if (probes %in% c("mm", "both")) {
        if (n["MM.resid"] == 0) {
          warning("PLM model did not include MM probes.", call. = FALSE)
        } else {
          index <- c(index, unlist(affy::indexProbes(object, "mm"), use.names = FALSE))
          values <- rbind(values, object@residuals$MM.resid)
        }
      }
    }

    rownames(values) <- index
    if (!is.null(select)) values <- values[, select, drop = FALSE]
    values
})

