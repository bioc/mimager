# Convert AffyBatch or PLMSet to an array of matrixes that correspond to the
# physical layout of their microarray features

setMethod("ma_layout", c(object = "AffyBatch"),
  function(object) .affy_layout(object))

setMethod("ma_layout", c(object = "PLMset"),
  function(object) .affy_layout(object))

.affy_layout <- function(object) {

  labels <- Biobase::sampleNames(object)

  n <- list(
       rows = object@nrow,
       cols = object@ncol,
    samples = length(labels)
  )

  index  <- stack(affy::indexProbes(object, which = "pm"))
  coords <- affy::indices2xy(index$values, nc = n$cols)

  # fill in empty rows if returning values for only pm or mm probes
  coords <- rbind(coords, cbind(coords[, "x"], coords[, "y"] + 1))

  # extract array values
  values2d <- switch(class(object),
         PLMset = residuals(object)$PM.resid,
      AffyBatch = affy::pm(object)
  )

  values3d <- lapply(labels, function(l) {
    mat <- matrix(nrow = n$rows, ncol = n$cols)
    mat[coords] <- values2d[, l]
    mat[, n$cols:1]
  })

  values3d <- abind::abind(values3d, along = 3)
  dimnames(values3d) <- list(seq_len(n$rows), seq_len(n$cols), labels)

  values3d
}
